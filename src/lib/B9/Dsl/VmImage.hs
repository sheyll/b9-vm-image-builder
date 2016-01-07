module B9.Dsl.VmImage where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import Control.Lens
import Data.Data
import Data.Singletons.TH
import Text.Printf

$(singletons
      [d|

  data VmImage = VmImage
               deriving Show
  |])

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFile :: Handle 'FreeFile
    , _vmiType :: ImageType
    } deriving (Show, Typeable)

makeLenses ''VmImgCtx

type instance ConvSpec 'FreeFile 'VmImage = ImageType
type instance ConvSpec 'VmImage 'FreeFile = ()
type instance ConvSpec 'VmImage 'VmImage = Either ImageType ImageSize
type instance ExportSpec 'VmImage = FilePath

instance CanConvert IoCompiler 'FreeFile 'VmImage where
    runConvert hnd _ imgT = do
        newHnd <- runConvert hnd SFreeFile (Just (printf "vm-image-%s" (show imgT)))
        createVmImage newHnd imgT

instance CanConvert IoCompiler 'VmImage 'FreeFile where
    runConvert hnd _ () = do
        Just (VmImgCtx srcFileH _srcType) <- useArtifactState hnd
        return srcFileH

instance CanConvert IoCompiler 'VmImage 'VmImage where
    runConvert hnd _ (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        destImgFileH <-
            runConvert
                srcImgFileH
                SFreeFile
                (Just (printf "resized-%d-%s" destSize (show destSizeU)))
        Just (FileCtx destImgFile _) <- useArtifactState destImgFileH
        addAction
            hnd
            (liftIoProgram
                 (resizeVmImage destImgFile destSize destSizeU srcType))
        hnd --> destImgFileH
        createVmImage destImgFileH srcType
    runConvert hnd@(Handle _ hndT) _ (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        srcFileCopy <- freeFileTempCopy srcImgFileH "conversion-src"
        (destImgFileH,destImgFile) <-
            createFreeFile (hndT ++ "-converted-to-" ++ show destType)
        addAction
            hnd
            (liftIoProgram
                 (convertVmImage srcFileCopy srcType destImgFile destType))
        hnd --> destImgFileH
        createVmImage destImgFileH destType

instance CanExport IoCompiler 'VmImage where
    runExport hnd@(Handle SVmImage _) destFile = do
        Just (VmImgCtx fH _) <- useArtifactState hnd
        runExport fH destFile

-- | Create a vm image entry in the context.
createVmImage :: Handle 'FreeFile -> ImageType -> IoCompiler (Handle 'VmImage)
createVmImage srcFileH vmt = do
    (hnd,_) <- allocHandle SVmImage ("vm-image-" ++ show vmt)
    putArtifactState hnd $ VmImgCtx srcFileH vmt
    srcFileH --> hnd
    return hnd
