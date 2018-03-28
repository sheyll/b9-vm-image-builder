module B9.ShakeRule () where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary as Binary
import qualified B9

-- | Run 'b9c' build
b9BuildFromB9File :: FilePath -> FilePath -> [String] -> Action ()
b9BuildFromB9File b9Root b9File args = do
  need [b9Root </> b9File]
  runB9
    (defaultB9RunParameters $ do
        modifyInvokationConfig
                (appendPositionalArguments args)
        modify
        runBuildArtifacts)
  -- cmd Shell (Cwd b9Root) "b9c" "-v" "build" "-f" b9File "--" b9ExtraArgs

-- * Rules to build and depend on b9 shared images.
needSharedImage :: B9.SharedImageName -> Action B9.SharedImageBuildId
needSharedImage = apply1

-- | Specify an action to build a shared image identified by its name.
sharedImageCustom :: B9.SharedImageName -> Action a -> Rules ()
sharedImageCustom b9img imgInfo f = sharedImageRuleFull b9img imgInfo f'
  where f' = fromJust <$> (f >> runLookupLocalSharedImage b9img)

-- | Specify an action to build a B9 shared image.
sharedImageRuleFull
  :: B9.SharedImageName -> Action B9.SharedImageBuildId -> Rules ()
sharedImageRuleFull sn@(B9.SharedImageName n) imgInfo act =
  addUserRule $ CreationRule sn act

type instance RuleResult B9.SharedImageName = B9.SharedImageBuildId

data CreationRule =
  CreationRule B9.SharedImageName (Action B9.SharedImageBuildId)
  deriving Typeable

addB9SharedImagesRule :: Rules ()
addB9SharedImagesRule = addBuiltinRule noLint run
 where
  run
    :: B9.SharedImageName
    -> Maybe ByteString.ByteString
    -> Bool
    -> Action (RunResult B9.SharedImageBuildId)
  run nameQ mSerlializedBuildId dependenciesChanged = do
    mCurrentBuildId <- runLookupLocalSharedImage nameQ

    case (decodeBuildId <$> mSerlializedBuildId, mCurrentBuildId) of

      (Nothing, Nothing) ->
        do newBuildId <- rebuild
           return (RunResult ChangedRecomputeDiff (encodeBuildId newBuildId) newBuildId)

      (Nothing, Just currentBuildId) ->
        if dependenciesChanged then
          do newBuildId <- rebuild
             let runChanged = if newBuildId == currentBuildId
                                 then ChangedStore
                                 else ChangedRecomputeDiff
             return (RunResult runChanged (encodeBuildId newBuildId) newBuildId)
        else
          return (RunResult ChangedStore (encodeBuildId currentBuildId) currentBuildId)

      (Just oldBuildId, Nothing) ->
          do newBuildId <- rebuild
             let runChanged = if oldBuildId == newBuildId
                                 then ChangedRecomputeSame
                                 else ChangedRecomputeDiff
             return (RunResult runChanged (encodeBuildId newBuildId) newBuildId)

      (Just oldBuildId, Just currentBuildId) ->
        do newBuildId <- if dependenciesChanged
                            then rebuild
                            else return currentBuildId
           let runChanged = if oldBuildId == newBuildId
                               then if dependenciesChanged
                                       then ChangedRecomputeSame
                                       else ChangedNothing
                               else ChangedRecomputeDiff

           return (RunResult runChanged (encodeBuildId newBuildId) newBuildId)

    where
      decodeBuildId :: ByteString.ByteString -> B9.SharedImageBuildId
      decodeBuildId = Binary.decode . LazyByteString.fromStrict

      encodeBuildId :: B9.SharedImageBuildId -> ByteString.ByteString
      encodeBuildId = LazyByteString.toStrict . Binary.encode

      rebuild :: Action B9.SharedImageBuildId
      rebuild = do
        rules <- getUserRules
        case userRuleMatch rules imgMatch of
          [] -> fail $ "No rules to build B9 shared image " ++ show nameQ ++ " found"
          [act] -> act
          _rs  -> fail $ "Multiple rules for the B9 shared image " ++ show nameQ ++ " found"
        where
          imgMatch (CreationRule name mkImage) =
              if name == nameQ then Just mkImage else Nothing

