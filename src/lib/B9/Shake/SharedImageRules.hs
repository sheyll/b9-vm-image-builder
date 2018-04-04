-- | A crude, unsafe and preliminary solution to building B9 'SharedImage's
-- from Shake.
module B9.Shake.SharedImageRules (customSharedImageAction, needSharedImage) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary as Binary
import B9
import B9.Shake.Actions (b9InvokationAction)

-- | Add a dependency to the creation of a 'SharedImage'. The build action
-- for the shared image must have been supplied by e.g. 'customSharedImageAction'.
--
needSharedImage :: SharedImageName -> Action SharedImageBuildId
needSharedImage = apply1

-- | Specify an arbitrary action that is supposed to build the given shared
-- image identified by a 'SharedImageName'.
customSharedImageAction :: SharedImageName -> Action () -> Rules ()
customSharedImageAction b9img customAction =
  addUserRule $ SharedImageCustomActionRule b9img $ do
    customAction
    (after, success) <- b9InvokationAction (runLookupLocalSharedImage b9img)
    unless success
      (internalErrorSharedImageNotFound b9img)
    maybe
      (errorSharedImageNotFound b9img)
      return
      after

type instance RuleResult SharedImageName = SharedImageBuildId

data SharedImageCustomActionRule =
  SharedImageCustomActionRule SharedImageName (Action SharedImageBuildId)
  deriving Typeable

addB9SharedImagesRule :: Rules ()
addB9SharedImagesRule = addBuiltinRule noLint go
 where
  go :: SharedImageName
     -> Maybe ByteString.ByteString
     -> Bool
     -> Action (RunResult SharedImageBuildId)
  go nameQ mSerlializedBuildId dependenciesChanged = do
    (mCurrentBuildId, success) <- b9InvokationAction (runLookupLocalSharedImage nameQ)
    unless success
      (internalErrorSharedImageNotFound nameQ)

    case (decodeBuildId <$> mSerlializedBuildId, mCurrentBuildId) of

      (Nothing, Nothing) ->
        do newBuildId <- rebuild
           return (RunResult ChangedRecomputeDiff (encodeBuildId newBuildId) newBuildId)

      (Nothing, Just currentBuildId) ->
        if dependenciesChanged then
          do newBuildId <- rebuild
             let changed = if newBuildId == currentBuildId
                                 then ChangedStore
                                 else ChangedRecomputeDiff
             return (RunResult changed (encodeBuildId newBuildId) newBuildId)
        else
          return (RunResult ChangedStore (encodeBuildId currentBuildId) currentBuildId)

      (Just oldBuildId, Nothing) ->
          do newBuildId <- rebuild
             let changed = if oldBuildId == newBuildId
                                 then ChangedRecomputeSame
                                 else ChangedRecomputeDiff
             return (RunResult changed (encodeBuildId newBuildId) newBuildId)

      (Just oldBuildId, Just currentBuildId) ->
        do newBuildId <- if dependenciesChanged
                            then rebuild
                            else return currentBuildId
           let changed = if oldBuildId == newBuildId
                               then if dependenciesChanged
                                       then ChangedRecomputeSame
                                       else ChangedNothing
                               else ChangedRecomputeDiff

           return (RunResult changed (encodeBuildId newBuildId) newBuildId)

    where
      decodeBuildId :: ByteString.ByteString -> SharedImageBuildId
      decodeBuildId = Binary.decode . LazyByteString.fromStrict

      encodeBuildId :: SharedImageBuildId -> ByteString.ByteString
      encodeBuildId = LazyByteString.toStrict . Binary.encode

      rebuild :: Action SharedImageBuildId
      rebuild = do
        rules <- getUserRules
        case userRuleMatch rules imgMatch of
          [] -> fail $ "No rules to build B9 shared image " ++ show nameQ ++ " found"
          [act] -> act
          _rs  -> fail $ "Multiple rules for the B9 shared image " ++ show nameQ ++ " found"
        where
          imgMatch (SharedImageCustomActionRule name mkImage) =
              if name == nameQ then Just mkImage else Nothing


internalErrorSharedImageNotFound :: Monad m => SharedImageName -> m a
internalErrorSharedImageNotFound =
  fail
  . printf "Internal Error: SharedImage %s not found. Please report this."
  . show

errorSharedImageNotFound :: Monad m => SharedImageName -> m a
errorSharedImageNotFound =
  fail
  . printf "Error: SharedImage %s not found."
  . show
