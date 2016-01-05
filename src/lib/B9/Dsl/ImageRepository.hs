module B9.Dsl.ImageRepository where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.Repository
import Control.Monad.Trans
import Data.Singletons.TH

$(singletons
      [d|

  data ImageRepository = ImageRepository
                       deriving Show
  |])

type instance AddSpec 'ImageRepository 'VmImage =
     (SharedImageName, Handle 'VmImage)

type instance ConvSpec 'ImageRepository 'VmImage = SharedImageName

-- * Image import

-- | A Global handle repesenting the (local) share image repository.
imageRepositoryH :: Handle 'ImageRepository
imageRepositoryH = globalHandle SImageRepository

fromShared :: (CanConvert m 'ImageRepository 'VmImage)
              => String -> ProgramT m (Handle 'VmImage)
fromShared sharedImgName = convert
        imageRepositoryH
        SVmImage
        (SharedImageName sharedImgName)

-- * Image export

-- | Store an image in the local cache with a name as key for lookups, e.g. from
-- 'fromShared'
sharedAs :: (CanAdd m 'ImageRepository 'VmImage)
            => Handle 'VmImage -> String -> ProgramT m ()
sharedAs hnd name = add imageRepositoryH SVmImage (SharedImageName name, hnd)

instance CanAdd IoCompiler 'ImageRepository 'VmImage where
    runAdd _ _ (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- useArtifactState vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH snStr
        vmI --> imageRepositoryH
        addAction imageRepositoryH (lift (imageRepoPublish imgFile srcType sn))

instance CanConvert IoCompiler 'ImageRepository 'VmImage where
    runConvert _ _ sharedImgName = do
        (sharedImgInfo,cachedImage) <- lift (imageRepoLookup sharedImgName)
        imgH <- runCreate SExternalFile cachedImage
        imgCopyH <- runConvert imgH SFreeFile ()
        createVmImage imgCopyH (siImgType sharedImgInfo)
