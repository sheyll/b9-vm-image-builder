module B9.Dsl.VmImage where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
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

instance CanExtract IoCompiler 'FreeFile 'VmImage where
    type ExtractionArg IoCompiler 'FreeFile 'VmImage = ImageType
    runExtract hnd _ imgT = do
        newHnd <- runExtract hnd SFreeFile Nothing
        createVmImage newHnd imgT

instance CanExtract IoCompiler 'VmImage 'FreeFile where
    runExtract hnd _ () = do
        Just (VmImgCtx srcFileH _srcType) <- useArtifactState hnd
        return srcFileH

instance CanExtract IoCompiler 'VmImage 'VmImage where
    type ExtractionArg IoCompiler 'VmImage 'VmImage = Either ImageType ImageSize
    runExtract hnd _ (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        destImgFileH <-
            runExtract
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
    runExtract hnd _ (Left destType) = do
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
    type ExportSpec IoCompiler 'VmImage = FilePath
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
