module B9.Dsl.FileSystem where

import B9.B9IO
import B9.Dsl.Content
import B9.Dsl.File
import B9.ShellScript       (toBashOneLiner)
import Control.Lens         hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Default
import Data.Graph           as Graph
import Data.Map             as Map hiding (null)
import Data.Monoid
import Data.Singletons
import Data.Tree            as Tree
import System.FilePath
import Text.Printf          (printf)

-- * File System API

-- | Create a file sytem image with a given type, label and size in giga
-- byte. Use the supplied action to add files to the fileSystemBuilder

-- * File System API Implementation

$(singletons
      [d|
  data FileSystemArtifact = FileSystemBuilder
                          | FileSystemImage
                          deriving Show
  |])

type instance CreateSpec 'FileSystemBuilder = FileSystemSpec
type instance AddSpec 'FileSystemBuilder 'FreeFile =
     (FileSpec, Handle 'FreeFile)
type instance ConvSpec 'FileSystemBuilder 'FileSystemImage = ()
type instance ConvSpec 'FileSystemBuilder 'FreeFile = ()
type instance ConvSpec 'FileSystemImage 'FileSystemImage =
     FileSystemResize
type instance ConvSpec 'FileSystemImage 'FreeFile = ()
type instance ConvSpec 'FreeFile 'FileSystemImage = FileSystem
type instance ExportSpec 'FileSystemImage = FilePath

instance CanCreate IoCompiler 'FileSystemBuilder where
     runCreate _ fsSpec@(FileSystemSpec t fsLabel _ _) = do
         let title =
                 show t ++ "-" ++
                 (if null fsLabel
                      then "image"
                      else fsLabel)
         (hnd,_) <- allocHandle SFileSystemBuilder fsLabel
         (tmpFileH,tmpFile) <- createFreeFile title
         hnd --> tmpFileH
         fH <- createFsImage tmpFileH t
         tmpDir <- lift (mkTempDir (title <.> "d"))
         putArtifactState hnd $ FsBuilderCtx [] tmpDir fH
         addAction
             hnd
             (do Just fileSys <- getArtifactState hnd
                 lift
                     (createFileSystem
                          tmpFile
                          fsSpec
                          tmpDir
                          (fileSys ^. fsFiles)))
         return hnd

instance CanAdd IoCompiler 'FileSystemBuilder 'FreeFile where
    runAdd fsH _ (fSpec,fH) = do
        modifyArtifactState fsH (traverse . fsFiles <>~ [fSpec])
        Just fileSys <- useArtifactState fsH
        let tmpDir = fileSys ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec

-- | Create a 'FsCtx' from an existing file and the file system type.
createFsImage :: Handle 'FreeFile -> FileSystem -> IoCompiler (Handle 'FileSystemImage)
createFsImage fH fs = do
    (hnd,_) <- allocHandle SFileSystemImage ( "fs-img-" ++ show fs)
    putArtifactState hnd $ FsCtx fH fs
    return hnd

instance CanConvert IoCompiler 'FileSystemBuilder 'FreeFile where
    runConvert hnd _ () = do
        Just fileSys <- useArtifactState hnd
        runConvert (fileSys ^. fsImgH) SFreeFile ()

instance CanConvert IoCompiler 'FileSystemImage 'FileSystemImage where
    runConvert hnd _ destSize = do
        Just (FsCtx inFileH fS) <- useArtifactState hnd
        outFileH <- runConvert inFileH SFreeFile "resized"
        Just (FileCtx outFile _) <- useArtifactState outFileH
        inFileH --> hnd
        hnd --> outFileH
        addAction hnd (lift (resizeFileSystem outFile destSize fS))
        createFsImage outFileH fS

instance CanConvert IoCompiler 'FileSystemImage 'FreeFile where
    runConvert hnd _ () = do
        Just (FsCtx fH _fS) <- useArtifactState hnd
        return fH

instance CanConvert IoCompiler 'FreeFile 'FileSystemImage where
    runConvert hnd _ fs = do
        copyH <- runConvert hnd SFreeFile (show fs)
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg

instance CanConvert IoCompiler 'FreeFile 'FileSystemImage where
    runConvert hnd _ fs = do
        copyH <- runConvert hnd SFreeFile (show fs)
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg

instance CanExport IoCompiler 'FileSystemImage where
     runExport hnd destFile = do
         Just fileSys <- useArtifactState hnd
         runExport (fileSys ^. fsFileH) destFile
