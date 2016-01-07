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

type instance AddSpec 'ImageRepository 'VmImage =
     (SharedImageName, Handle 'VmImage)

type instance ConvSpec 'ImageRepository 'VmImage = SharedImageName

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
    runAdd _ _ (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- useArtifactState vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH (Just snStr) -- TODO
        ensureImageRepositoryH
        vmI --> imageRepositoryH
        addAction
            imageRepositoryH
            (liftIoProgram (imageRepoPublish imgFile srcType sn))

instance CanConvert IoCompiler 'ImageRepository 'VmImage where
    runConvert _ _ sharedImgName = do
        (sharedImgInfo,cachedImage) <-
            liftIoProgram (imageRepoLookup sharedImgName)
        imgH <- runCreate SExternalFile cachedImage
        imgCopyH <- runConvert imgH SFreeFile ()
        createVmImage imgCopyH (siImgType sharedImgInfo)
