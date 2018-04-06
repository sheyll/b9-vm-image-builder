-- | A crude, unsafe and preliminary solution to building B9 'SharedImage's
-- from Shake.
module B9.Shake.SharedImageRules ( customSharedImageAction
                                 , needSharedImage
                                 , enableSharedImageRules) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary as Binary
import B9
import B9.Shake.Actions (b9InvokationAction)

-- | In order to use 'needSharedImage' and 'customSharedImageAction' you need to
-- call this action before using any of the afore mentioned.
enableSharedImageRules :: Rules ()
enableSharedImageRules = addBuiltinRule noLint go
 where
  go :: SharedImageName
     -> Maybe ByteString.ByteString
     -> Bool
     -> Action (RunResult SharedImageBuildId)
  go nameQ mSerlializedBuildId dependenciesChanged =
    let mOld = decodeBuildId <$> mSerlializedBuildId
        in do
          (rebuilt, newBuildId) <-
           if dependenciesChanged then     (True,) <$> rebuild
             else
              do mNewBuildId <- getImgBuildId
                 maybe ((\i -> (True, i)) <$> rebuild)
                          (\i -> return (False, i))
                          mNewBuildId
          let newBuildIdBin = encodeBuildId newBuildId
              change =
                        if rebuilt then
                          maybe ChangedRecomputeDiff
                                    (\buildIdChanged ->
                                      if buildIdChanged then  ChangedRecomputeSame
                                           else ChangedRecomputeSame )
                                    ((/= newBuildId) <$> mOld)
                         else
                           maybe ChangedStore
                                 (\buildIdChanged ->
                                  if buildIdChanged then  ChangedRecomputeSame
                                        else ChangedRecomputeSame )
                                 ((/= newBuildId) <$> mOld)
              result = RunResult change newBuildIdBin newBuildId
          return result
    where
      getImgBuildId = b9InvokationAction (runLookupLocalSharedImage nameQ)

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

-- | Add a dependency to the creation of a 'SharedImage'. The build action
-- for the shared image must have been supplied by e.g. 'customSharedImageAction'.
-- NOTE: You must call 'enableSharedImageRules' before this action works.
needSharedImage :: SharedImageName -> Action SharedImageBuildId
needSharedImage = apply1

-- | Specify an arbitrary action that is supposed to build the given shared
-- image identified by a 'SharedImageName'.
-- NOTE: You must call 'enableSharedImageRules' before this action works.
customSharedImageAction :: SharedImageName -> Action () -> Rules ()
customSharedImageAction b9img customAction =
  addUserRule (SharedImageCustomActionRule b9img customAction')
  where
    customAction' = do
      customAction
      mCurrentBuildId <- b9InvokationAction (runLookupLocalSharedImage b9img)
      putLoud (printf "Finished custom action, for %s, build-id is: %s"
                      (show b9img) (show mCurrentBuildId))
      maybe (errorSharedImageNotFound b9img) return mCurrentBuildId


type instance RuleResult SharedImageName = SharedImageBuildId

data SharedImageCustomActionRule =
  SharedImageCustomActionRule SharedImageName (Action SharedImageBuildId)
  deriving Typeable

errorSharedImageNotFound :: Monad m => SharedImageName -> m a
errorSharedImageNotFound =
  fail
  . printf "Error: %s not found."
  . show