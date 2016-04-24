{-# LANGUAGE ScopedTypeVariables #-}
module B9.Dsl.ImageRepository where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.Repository
import Data.Singletons.TH
import Data.Typeable

$(singletons
      [d|

  data ImageRepository = ImageRepository
                       deriving (Show, Typeable)
  |])

instance Show (Sing 'ImageRepository) where
  show _ = show ImageRepository

type instance IoCompilerArtifactState 'ImageRepository = ImageRepository

-- | A Global handle repesenting the (local) share image repository.
imageRepositoryH :: Handle 'ImageRepository
imageRepositoryH = globalHandle SImageRepository

ensureImageRepositoryH :: IoCompiler ()
ensureImageRepositoryH = do
    (mImgRepo :: Maybe ImageRepository) <- useArtifactState imageRepositoryH
    case mImgRepo of
        Nothing -> do
            allocPredefinedHandle imageRepositoryH
            putArtifactState imageRepositoryH ImageRepository
        Just _ -> return ()

instance CanAdd IoCompiler 'ImageRepository 'VmImage where
    type AddSpec IoCompiler 'ImageRepository 'VmImage = (SharedImageName, Handle 'VmImage)
    runAdd _ _ (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- useArtifactState vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH (Just snStr)
        ensureImageRepositoryH
        vmI --> imageRepositoryH
        addAction
            imageRepositoryH
            (liftIoProgram (imageRepoPublish imgFile srcType sn))

instance CanExtract IoCompiler 'ImageRepository 'VmImage where
    type ExtractionArg IoCompiler 'ImageRepository 'VmImage = SharedImageName
    runExtract _ _ sharedImgName = do
        (sharedImgInfo,cachedImage) <-
            liftIoProgram (imageRepoLookup sharedImgName)
        imgH <- runCreate SExternalFile cachedImage
        imgCopyH <- runExtract imgH SFreeFile ()
        createVmImage imgCopyH (siImgType sharedImgInfo)
