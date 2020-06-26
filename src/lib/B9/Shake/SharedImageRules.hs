-- | A crude, unsafe and preliminary solution to building B9 'SharedImage's
-- from Shake.
module B9.Shake.SharedImageRules
  ( customSharedImageAction,
    needSharedImage,
    enableSharedImageRules,
  )
where

import B9
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import GHC.Stack

-- | In order to use 'needSharedImage' and 'customSharedImageAction' you need to
-- call this action before using any of the aforementioned 'Rules'.
enableSharedImageRules :: HasCallStack => B9ConfigOverride -> Rules ()
enableSharedImageRules b9inv = addBuiltinRule noLint noIdentity go
  where
    go :: BuiltinRun SharedImageName SharedImageBuildId
    go nameQ mOldBIdBinary dependenciesChanged = do
      mCurrentBId <- getImgBuildId
      let mCurrentBIdBinary = encodeBuildId <$> mCurrentBId
      putLoud $
        "share image rule for: "
          ++ show nameQ
          ++ ". Deps: "
          ++ show dependenciesChanged
          ++ ", current BId: "
          ++ show mCurrentBId
          ++ " Binary: "
          ++ show mCurrentBIdBinary
          ++ ", old BId: "
          ++ show mOldBIdBinary
      case mCurrentBIdBinary of
        Just currentBIdBinary ->
          if dependenciesChanged == RunDependenciesSame && mOldBIdBinary == Just currentBIdBinary
            then return $ RunResult ChangedNothing currentBIdBinary (fromJust mCurrentBId)
            else rebuild (Just currentBIdBinary)
        Nothing -> rebuild Nothing
      where
        getImgBuildId = liftIO (runB9ConfigActionWithOverrides (runLookupLocalSharedImage nameQ) b9inv)
        encodeBuildId :: SharedImageBuildId -> ByteString.ByteString
        encodeBuildId = LazyByteString.toStrict . Binary.encode
        rebuild :: Maybe ByteString.ByteString -> Action (RunResult SharedImageBuildId)
        rebuild mCurrentBIdBinary = do
          (_, act) <- getUserRuleOne nameQ (const Nothing) imgMatch
          _ <- act b9inv
          mNewBId <- getImgBuildId
          newBId <-
            maybe
              (error ("failed to get SharedImageBuildId for " ++ show nameQ ++ " in context of " ++ show b9inv))
              return
              mNewBId
          let newBIdBinary = encodeBuildId newBId
          let change =
                if Just newBIdBinary == mCurrentBIdBinary
                  then ChangedRecomputeSame
                  else ChangedRecomputeDiff
          return $ RunResult change newBIdBinary newBId
          where
            imgMatch (SharedImageCustomActionRule name mkImage) =
              if name == nameQ
                then Just mkImage
                else Nothing

-- | Add a dependency to the creation of a 'SharedImage'. The build action
-- for the shared image must have been supplied by e.g. 'customSharedImageAction'.
-- NOTE: You must call 'enableSharedImageRules' before this action works.
needSharedImage :: HasCallStack => SharedImageName -> Action SharedImageBuildId
needSharedImage = apply1

-- | Specify an arbitrary action that is supposed to build the given shared
-- image identified by a 'SharedImageName'.
-- NOTE: You must call 'enableSharedImageRules' before this action works.
customSharedImageAction :: HasCallStack => SharedImageName -> Action () -> Rules ()
customSharedImageAction b9img customAction = addUserRule (SharedImageCustomActionRule b9img customAction')
  where
    customAction' b9inv = do
      customAction
      mCurrentBuildId <- liftIO (runB9ConfigActionWithOverrides (runLookupLocalSharedImage b9img) b9inv)
      putLoud (printf "Finished custom action, for %s, build-id is: %s" (show b9img) (show mCurrentBuildId))
      maybe (errorSharedImageNotFound b9img) return mCurrentBuildId

type instance RuleResult SharedImageName = SharedImageBuildId

data SharedImageCustomActionRule
  = SharedImageCustomActionRule
      SharedImageName
      (B9ConfigOverride -> Action SharedImageBuildId)
  deriving (Typeable)

errorSharedImageNotFound :: (HasCallStack, MonadFail m) => SharedImageName -> m a
errorSharedImageNotFound = fail . printf "Error: %s not found." . show
