module B9.Dsl.FileSystem where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.FileSystems
import Data.Data
import Data.Singletons.TH

-- * File System API Implementation

$(singletons
      [d|
  data FileSystemArtifact = FileSystemBuilder
                          | FileSystemImage
                          deriving Show
  |])

instance Show (Sing 'FileSystemBuilder) where show _ = "FileSystemBuilder"
instance Show (Sing 'FileSystemImage) where show _ = "FileSystemImage"

-- | Context of a 'SFileSystemBuilder'
data FsBuilderCtx = FsBuilderCtx
    { _fsFiles :: [FileSpec]
    , _fsTempDir :: FilePath
    , _fsImgH :: Handle 'FileSystemImage
    } deriving (Show, Typeable)

type instance IoCompilerArtifactState 'FileSystemBuilder =
     FsBuilderCtx

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsFileH :: Handle 'FreeFile
    , _fsType :: FileSystem
    } deriving (Show, Typeable)

type instance IoCompilerArtifactState 'FileSystemImage = FsCtx

makeLenses ''FsCtx
makeLenses ''FsBuilderCtx

instance CanCreate IoCompiler 'FileSystemBuilder where
    type CreateSpec IoCompiler 'FileSystemBuilder = FileSystemSpec
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
    type AddSpec IoCompiler 'FileSystemBuilder 'FreeFile = (FileSpec, Handle 'FreeFile)
    runAdd fsH _ (fSpec,fH) = do
        modifyArtifactState fsH (traverse . fsFiles <>~ [fSpec])
        Just fsBuilder <- useArtifactState fsH
        let tmpDir = fsBuilder ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec

instance CanExtract IoCompiler 'FileSystemBuilder 'FileSystemImage where
    runExtract hnd _ () = do
        Just fsBuilder <- useArtifactState hnd
        return (fsBuilder ^. fsImgH)

instance CanExtract IoCompiler 'FileSystemBuilder 'FreeFile where
    runExtract hnd _ () = do
        Just fsBuilder <- useArtifactState hnd
        runExtract (fsBuilder ^. fsImgH) SFreeFile ()

instance CanExtract IoCompiler 'FileSystemImage 'FileSystemImage where
    type ExtractionArg IoCompiler 'FileSystemImage 'FileSystemImage = FileSystemResize
    runExtract hnd _ destSize = do
        Just (FsCtx inFileH fS) <- useArtifactState hnd
        outFileH <- runExtract inFileH SFreeFile (Just "resized")
        Just (FileCtx outFile _) <- useArtifactState outFileH
        inFileH --> hnd
        hnd --> outFileH
        addAction hnd (liftIoProgram (resizeFileSystem outFile destSize fS))
        createFsImage outFileH fS

instance CanExtract IoCompiler 'FileSystemImage 'FreeFile where
    runExtract hnd _ () = do
        Just (FsCtx fH _fS) <- useArtifactState hnd
        return fH

instance CanExtract IoCompiler 'FreeFile 'FileSystemImage where
    type ExtractionArg IoCompiler 'FreeFile 'FileSystemImage = FileSystem
    runExtract hnd _ fs = do
        copyH <- runExtract hnd SFreeFile (Just (show fs))
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg

instance CanExport IoCompiler 'FileSystemImage where
    type ExportSpec IoCompiler 'FileSystemImage = FilePath
    runExport hnd destFile = do
         Just fsImg <- useArtifactState hnd
         runExport (fsImg ^. fsFileH) destFile

instance CanExtract IoCompiler 'FileSystemBuilder 'VmImage where
    runExtract hnd _ () = do
        Just fsImg <- useArtifactState hnd
        runExtract (fsImg ^. fsImgH) SVmImage ()

instance CanExtract IoCompiler 'FileSystemImage 'VmImage where
    runExtract hnd _ () = do
        Just (FsCtx fH _) <- useArtifactState hnd
        fH' <- runExtract fH SFreeFile Nothing
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH

instance CanExtract IoCompiler 'VmImage 'FileSystemImage where
    runExtract hnd _ () = do
        hnd' <- runExtract hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- useArtifactState hnd'
        runExtract srcFileH SFileSystemImage Ext4
