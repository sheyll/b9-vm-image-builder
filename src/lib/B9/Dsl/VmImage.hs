module B9.Dsl.VmImage where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import Data.Data
import Data.Singletons.TH

$(singletons
      [d|

  data VmImage = VmImage
               deriving Show
  |])

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFile :: Handle 'FreeFile
    , _vmiType :: ImageType
    } deriving (Show,Typeable)

type instance IoCompilerArtifactState 'VmImage = VmImgCtx

makeLenses ''VmImgCtx

type instance ExtractionArg 'FreeFile 'VmImage = ImageType
type instance ExtractionArg 'VmImage 'FreeFile = ()
type instance ExtractionArg 'VmImage 'VmImage = Either ImageType ImageSize
type instance ExportSpec 'VmImage = FilePath

instance CanExtract IoCompiler 'FreeFile 'VmImage where
    runConvert hnd _ imgT = do
        newHnd <- runConvert hnd SFreeFile Nothing
        createVmImage newHnd imgT

instance CanExtract IoCompiler 'VmImage 'FreeFile where
    runConvert hnd _ () = do
        Just (VmImgCtx srcFileH _srcType) <- useArtifactState hnd
        return srcFileH

instance CanExtract IoCompiler 'VmImage 'VmImage where
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
    runConvert hnd _ (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        let srcImgFileTitle = case srcImgFileH of (Handle _ x) -> x
        srcFileCopy <- freeFileTempCopy srcImgFileH Nothing
        (destImgFileH,destImgFile) <-
            createFreeFile (srcImgFileTitle ++ "-" ++ show destType)
        addAction
            hnd
            (liftIoProgram
                 (convertVmImage srcFileCopy srcType destImgFile destType))
        hnd --> destImgFileH
        createVmImage destImgFileH destType

instance CanExport IoCompiler 'VmImage where
    runExport hnd@(Handle _ _) destFile = do
        Just (VmImgCtx fH _) <- useArtifactState hnd
        runExport fH destFile

-- | Create a vm image entry in the context.
createVmImage :: Handle 'FreeFile -> ImageType -> IoCompiler (Handle 'VmImage)
createVmImage srcFileH vmt = do
    (hnd,_) <- allocHandle SVmImage ("vm-image-" ++ show vmt)
    putArtifactState hnd $ VmImgCtx srcFileH vmt
    srcFileH --> hnd
    return hnd
