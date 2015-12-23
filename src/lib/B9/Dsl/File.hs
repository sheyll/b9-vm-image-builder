module B9.Dsl.File where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import B9.Logging
import Control.Lens         hiding (from, (<.>))
import Control.Monad.Reader
import Data.Data
import Data.Singletons.TH   (singletons,Sing)
import Data.Word
import System.FilePath
import Text.Printf          (printf)

$(singletons [d|
  data FileArtifacts = ExternalFile | FreeFile | LocalDirectory
             deriving Show
  |])

-- * Files

-- | Reference an external file. An external file will never be modified.
externalFile
    :: CanCreate m 'ExternalFile
    => FilePath -> ProgramT m (Handle 'ExternalFile)
externalFile = create SExternalFile

-- | Get a reference to a copy of an external file inside the build root that
-- can be modified.
externalFileTempCopy
    :: (CanCreate m 'ExternalFile, CanConvert m 'ExternalFile 'FreeFile)
    => FilePath -> ProgramT m (Handle 'FreeFile)
externalFileTempCopy f = do
    extH <- externalFile f
    convert extH SFreeFile ()

-- | 'use' an external file to introduce an artifact with the help of a
--  artifact dependent extra arguement and return the artifacts handle.
fromFile
    :: (Show (ConvSpec 'FreeFile b), CanCreate m 'ExternalFile, CanConvert m 'FreeFile b, CanConvert m 'ExternalFile 'FreeFile)
    => FilePath -> Sing b -> ConvSpec 'FreeFile b -> ProgramT m (Handle b)
fromFile f a conversionArg = do
    h <- externalFileTempCopy f
    convert h a conversionArg

-- | Given an artifact that support extraction or conversion to a file
-- create/write a file to a given output path.
outputFile
    :: (Show (ConvSpec a 'FreeFile), CanConvert m a 'FreeFile, CanExport m 'FreeFile)
    => Handle a -> ConvSpec a 'FreeFile -> FilePath -> ProgramT m ()
outputFile e src dst = do
    outF <- convert e SFreeFile src
    export outF dst

-- * File Inclusion, File-Templating and Script Rendering

-- | Add an existing file to an artifact.
-- Strip the directories from the path, e.g. @/etc/blub.conf@ will be
-- @blob.conf@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addFile
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addFile d f = addFileP d f (0, 6, 4, 4)

-- | Same as 'addFile' but set the destination file permissions to @0755@
-- (executable for all).
addExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addExe d f = addFileP d f (0, 7, 5, 5)

-- | Same as 'addFile' but with an extra output file permission parameter.
addFileP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> ProgramT m ()
addFileP dstH f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
    origH <- create SExternalFile f
    tmpH <- convert origH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpH)

-- * Directories

-- | Create a temp directory
newDirectory :: (CanCreate m 'LocalDirectory)
                => ProgramT m (Handle 'LocalDirectory)
newDirectory = create SLocalDirectory ()

-- | Render the directory to the actual destination (which must not exist)
exportDir :: (CanExport m 'LocalDirectory)
             => Handle 'LocalDirectory -> FilePath -> ProgramT m ()
exportDir = export

-- * Implementation

type instance CreateSpec 'ExternalFile = FilePath
type instance CreateSpec 'FreeFile = Maybe String
type instance CreateSpec 'LocalDirectory = ()

type instance AddSpec 'LocalDirectory 'FreeFile =
     (FileSpec, Handle 'FreeFile)

type instance ConvSpec 'ExternalFile 'FreeFile = ()
type instance ConvSpec 'FreeFile 'ExternalFile = FilePath
type instance ConvSpec 'FreeFile 'FreeFile = String

type instance ExportSpec 'FreeFile = FilePath
type instance ExportSpec 'LocalDirectory = FilePath

-- | Context of a 'SFreeFile'
data FileCtx = FileCtx
    { _fFileName :: FilePath
    , _fCopies :: [FilePath]
    } deriving (Show, Typeable)

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
    { _dirTempDir :: FilePath
    , _dirExports :: [FilePath]
    } deriving (Show, Typeable)

makeLenses ''FileCtx
makeLenses ''DirCtx

instance CanCreate IoCompiler 'ExternalFile where
     runCreate _ fn = do
         (hnd,_) <- allocHandle SExternalFile (takeFileName fn)
         fn' <- lift (getRealPath fn)
         putArtifactState hnd fn'
         return hnd

instance CanCreate IoCompiler 'FreeFile where
    runCreate _ mTempName = do
        -- TODO escape tempName, allow only a-zA-Z0-9.-_:+=
        let tempName = maybe "tmp-file" takeFileName mTempName
        (hnd,_) <- createFreeFile tempName
        return hnd

instance CanCreate IoCompiler 'LocalDirectory where
    runCreate _ () = do
        tmp <- lift (mkTempDir "local-dir")
        (hnd,_) <- allocHandle SLocalDirectory tmp
        putArtifactState hnd (DirCtx tmp [])
        addAction
            hnd
            (do Just (DirCtx src dests) <- getArtifactState hnd
                case reverse dests of
                    [] -> lift (errorL hnd "not exported!")
                    (lastDest:firstDests) ->
                        lift
                            (do mapM_ (copyDir src) (reverse firstDests)
                                moveDir src lastDest))
        return hnd

instance CanAdd IoCompiler 'LocalDirectory 'FreeFile where
    runAdd dirH _ (fSpec,fH) = do
        Just localDir <- useArtifactState dirH
        copyFreeFile' fH (localDir ^. dirTempDir) fSpec
        fH --> dirH

instance CanConvert IoCompiler 'ExternalFile 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ () = do
        Just externalFileName <- useArtifactState hnd
        (tmpFileH,tmpFile) <- createFreeFile (hndT ++ "-copy")
        hnd --> tmpFileH
        addAction hnd (lift (copy externalFileName tmpFile))
        return tmpFileH

instance CanConvert IoCompiler 'FreeFile 'ExternalFile where
    runConvert hnd _ dest = do
        dest' <- lift (ensureParentDir dest)
        newFileH <- runCreate SExternalFile dest'
        hnd --> newFileH
        copyFreeFile hnd dest'
        return newFileH

instance CanConvert IoCompiler 'FreeFile 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ dest = do
        (newFileH,newFile) <- createFreeFile (hndT ++ "-" ++ dest)
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH

instance CanExport IoCompiler 'FreeFile where
    runExport hnd destFile =
        lift (ensureParentDir destFile) >>= copyFreeFile hnd

instance CanExport IoCompiler 'LocalDirectory where
    runExport hnd destDir = do
        destDir' <- lift (ensureParentDir destDir)
        modifyArtifactState hnd (traverse . dirExports %~ (destDir':))

-- | Create and allocate a new 'FreeFile' and return the handle as well as the
-- path to the temporary file.
createFreeFile :: String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFile title = do
    src <- lift (mkTempCreateParents title)
    hnd <- asFreeFile src title
    return (hnd, src)

-- | Create and allocate a new 'FreeFile' inside a given directory and
-- return the handle as well as the path to the temporary file.
createFreeFileIn :: FilePath -> String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFileIn parent title = do
    src <- lift (mkTempIn parent title)
    hnd <- asFreeFile src title
    return (hnd, src)

-- | Allocate a 'FreeFile' artifact for a given file with a given title.
asFreeFile :: FilePath -> String -> IoCompiler (Handle 'FreeFile)
asFreeFile src title = do
    (hnd,_) <- allocHandle SFreeFile title
    putArtifactState hnd $ FileCtx src []
    addAction
        hnd
        (do Just (FileCtx _ destinations) <- getArtifactState hnd
            lift
                (case reverse destinations of
                     (lastCopy:firstCopies) -> do
                         mapM_ (copy src) (reverse firstCopies)
                         moveFile src lastCopy
                     [] -> dbgL "No copies of" src "required"))
    return hnd

-- | Add a new copy to a 'FreeFile' at the specified destination
copyFreeFile :: Handle 'FreeFile -> FilePath -> IoCompiler ()
copyFreeFile src dest = modifyArtifactState src $ traverse . fCopies <>~ [dest]

-- | Add a new copy to a 'FreeFile' using a unique temp file containg
-- a given string for better debugging, and return the path to the copy.
freeFileTempCopy :: Handle 'FreeFile -> String -> IoCompiler FilePath
freeFileTempCopy src name = do
    Just fileCtx <- useArtifactState src
    dest <-
        lift
            (mkTempCreateParents
                 (printf
                      "%s-%s"
                      (takeFileName (fileCtx ^. fFileName))
                      (takeFileName name)))
    copyFreeFile src dest
    return dest

-- | Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))
