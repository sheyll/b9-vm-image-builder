module B9.Dsl.FileSystem where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.FileSystems
import Control.Lens        hiding ((<.>))
import Data.Data
import Data.Singletons.TH
import System.FilePath

-- * File System API Implementation

$(singletons
      [d|
  data FileSystemArtifact = FileSystemBuilder
                          | FileSystemImage
                          deriving Show
  |])

-- | Context of a 'SFileSystemBuilder'
data FsBuilderCtx = FsBuilderCtx
    { _fsFiles :: [FileSpec]
    , _fsTempDir :: FilePath
    , _fsImgH :: Handle 'FileSystemImage
    } deriving (Show, Typeable)

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsFileH :: Handle 'FreeFile
    , _fsType :: FileSystem
    } deriving (Show, Typeable)

makeLenses ''FsCtx
makeLenses ''FsBuilderCtx

type instance CreateSpec 'FileSystemBuilder = FileSystemSpec
type instance AddSpec 'FileSystemBuilder 'FreeFile =
     (FileSpec, Handle 'FreeFile)
type instance ConvSpec 'FileSystemBuilder 'FileSystemImage = ()
type instance ConvSpec 'FileSystemBuilder 'FreeFile = ()
type instance ConvSpec 'FileSystemImage 'FileSystemImage =
     FileSystemResize
type instance ConvSpec 'FileSystemImage 'FreeFile = ()
type instance ConvSpec 'FreeFile 'FileSystemImage = FileSystem
type instance ConvSpec 'FileSystemBuilder 'VmImage = ()
type instance ConvSpec 'FileSystemImage 'VmImage = ()
type instance ConvSpec 'VmImage 'FileSystemImage = ()
type instance ExportSpec 'FileSystemImage = FilePath

instance CanCreate IoCompiler 'FileSystemBuilder where
    runCreate _ fsSpec@(FileSystemSpec t fsLabel _ _) = do
        let title =
                (if null fsLabel
                     then "image"
                     else fsLabel)
                <.> show t
        (hnd,_) <- allocHandle SFileSystemBuilder fsLabel
        (tmpFileH,tmpFile) <- createFreeFile title
        hnd --> tmpFileH
        fH <- createFsImage tmpFileH t
        tmpFileH --> fH
        tmpDir <- liftIoProgram (mkTempDir (title <.> "d"))
        putArtifactState hnd $ FsBuilderCtx [] tmpDir fH
        addAction
            hnd
            (do Just fileSys <- getArtifactState hnd
                liftIoProgram
                    (createFileSystem
                         tmpFile
                         fsSpec
                         tmpDir
                         (fileSys ^. fsFiles)))
        return hnd

-- | Create a 'FsCtx' from an existing file and the file system type.
createFsImage :: Handle 'FreeFile
              -> FileSystem
              -> IoCompiler (Handle 'FileSystemImage)
createFsImage fH fs = do
    (hnd,_) <- allocHandle SFileSystemImage ("fs-img-" ++ show fs)
    putArtifactState hnd $ FsCtx fH fs
    return hnd

instance CanAdd IoCompiler 'FileSystemBuilder 'FreeFile where
    runAdd fsH _ (fSpec,fH) = do
        modifyArtifactState fsH (traverse . fsFiles <>~ [fSpec])
        Just fsBuilder <- useArtifactState fsH
        let tmpDir = fsBuilder ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec

instance CanConvert IoCompiler 'FileSystemBuilder 'FileSystemImage where
    runConvert hnd _ () = do
        Just fsBuilder <- useArtifactState hnd
        return (fsBuilder ^. fsImgH)

instance CanConvert IoCompiler 'FileSystemBuilder 'FreeFile where
    runConvert hnd _ () = do
        Just fsBuilder <- useArtifactState hnd
        runConvert (fsBuilder ^. fsImgH) SFreeFile ()

instance CanConvert IoCompiler 'FileSystemImage 'FileSystemImage where
    runConvert hnd _ destSize = do
        Just (FsCtx inFileH fS) <- useArtifactState hnd
        outFileH <- runConvert inFileH SFreeFile (Just "resized")
        Just (FileCtx outFile _) <- useArtifactState outFileH
        inFileH --> hnd
        hnd --> outFileH
        addAction hnd (liftIoProgram (resizeFileSystem outFile destSize fS))
        createFsImage outFileH fS

instance CanConvert IoCompiler 'FileSystemImage 'FreeFile where
    runConvert hnd _ () = do
        Just (FsCtx fH _fS) <- useArtifactState hnd
        return fH

instance CanConvert IoCompiler 'FreeFile 'FileSystemImage where
    runConvert hnd _ fs = do
        copyH <- runConvert hnd SFreeFile (Just (show fs))
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg

instance CanExport IoCompiler 'FileSystemImage where
     runExport hnd destFile = do
         Just fsImg <- useArtifactState hnd
         runExport (fsImg ^. fsFileH) destFile

instance CanConvert IoCompiler 'FileSystemBuilder 'VmImage where
    runConvert hnd _ () = do
        Just fsImg <- useArtifactState hnd
        runConvert (fsImg ^. fsImgH) SVmImage ()

instance CanConvert IoCompiler 'FileSystemImage 'VmImage where
    runConvert hnd _ () = do
        Just (FsCtx fH _) <- useArtifactState hnd
        fH' <- runConvert fH SFreeFile (Just "Raw-image")
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH

instance CanConvert IoCompiler 'VmImage 'FileSystemImage where
    runConvert hnd _ () = do
        hnd' <- runConvert hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- useArtifactState hnd'
        runConvert srcFileH SFileSystemImage Ext4
