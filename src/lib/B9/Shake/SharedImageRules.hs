-- | A crude, unsafe and preliminary solution to building B9 'SharedImage's
-- from Shake.
module B9.Shake.SharedImageRules
  ( customSharedImageAction
  , needSharedImage
  , enableSharedImageRules
  )
where

import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Rule
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Builder       as Builder
import           Data.ByteString.Builder        ( stringUtf8 )
import qualified Data.ByteString.Lazy          as LazyByteString
import qualified Data.Binary                   as Binary
import           B9

-- | In order to use 'needSharedImage' and 'customSharedImageAction' you need to
-- call this action before using any of the aforementioned 'Rules'.
enableSharedImageRules :: B9ConfigOverride -> Rules ()
enableSharedImageRules b9inv = addBuiltinRule noLint sharedImageIdentity go
 where
  sharedImageIdentity :: BuiltinIdentity SharedImageName SharedImageBuildId
  sharedImageIdentity (SharedImageName k) (SharedImageBuildId v) =
      Just (LazyByteString.toStrict (Builder.toLazyByteString (stringUtf8 k <> stringUtf8 v)))

  go :: BuiltinRun SharedImageName SharedImageBuildId
  go nameQ mOldBIdBinary dependenciesChanged = do
    mCurrentBId <- getImgBuildId
    case mCurrentBId of
      Just currentBId ->
        let currentBIdBinary = encodeBuildId currentBId

        in  if dependenciesChanged
                 == RunDependenciesChanged
                 && mOldBIdBinary
                 == Just currentBIdBinary
              then return $ RunResult ChangedNothing currentBIdBinary currentBId
              else rebuild (Just currentBIdBinary)
      _ -> rebuild Nothing
   where
    getImgBuildId = execB9ConfigAction (runLookupLocalSharedImage nameQ) b9inv

    encodeBuildId :: SharedImageBuildId -> ByteString.ByteString
    encodeBuildId = LazyByteString.toStrict . Binary.encode

    rebuild
      :: Maybe ByteString.ByteString -> Action (RunResult SharedImageBuildId)
    rebuild mCurrentBIdBinary = do
      (_, act) <- getUserRuleOne nameQ (const Nothing) imgMatch
      act b9inv
      mNewBId <- getImgBuildId
      newBId  <- maybe
        (error
          (  "failed to get SharedImageBuildId for "
          ++ show nameQ
          ++ " in context of "
          ++ show b9inv
          )
        )
        return
        mNewBId
      let newBIdBinary = encodeBuildId newBId
      let change = if Just newBIdBinary == mCurrentBIdBinary
            then ChangedRecomputeSame
            else ChangedRecomputeDiff
      return $ RunResult change newBIdBinary newBId
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
customSharedImageAction b9img customAction = addUserRule
  (SharedImageCustomActionRule b9img customAction')
 where
  customAction' b9inv = do
    customAction
    mCurrentBuildId <- execB9ConfigAction (runLookupLocalSharedImage b9img)
                                          b9inv
    putLoud
      (printf "Finished custom action, for %s, build-id is: %s"
              (show b9img)
              (show mCurrentBuildId)
      )
    maybe (errorSharedImageNotFound b9img) return mCurrentBuildId

type instance RuleResult SharedImageName = SharedImageBuildId

data SharedImageCustomActionRule =
  SharedImageCustomActionRule SharedImageName (B9ConfigOverride -> Action SharedImageBuildId)
  deriving Typeable

errorSharedImageNotFound :: Monad m => SharedImageName -> m a
errorSharedImageNotFound = fail . printf "Error: %s not found." . show
