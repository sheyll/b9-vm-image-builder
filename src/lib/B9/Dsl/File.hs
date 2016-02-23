module B9.Dsl.File where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import B9.Logging
import Control.Lens       hiding (from, (<.>))
import Data.Data
import Data.Singletons.TH (singletons)
import System.FilePath

$(singletons [d|
  data FileArtifacts = ExternalFile | FreeFile | LocalDirectory
             deriving Show
  |])

type instance CreateSpec 'ExternalFile = FilePath
type instance CreateSpec 'FreeFile = Maybe String
type instance CreateSpec 'LocalDirectory = ()

type instance AddSpec 'LocalDirectory 'FreeFile =
     (FileSpec, Handle 'FreeFile)

type instance ConvSpec 'ExternalFile 'FreeFile = ()
type instance ConvSpec 'FreeFile 'ExternalFile = FilePath
type instance ConvSpec 'FreeFile 'FreeFile = Maybe String

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
         fn' <- liftIoProgram (getRealPath fn)
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
        tmp <- liftIoProgram (mkTempDir "local-dir")
        (hnd,_) <- allocHandle SLocalDirectory tmp
        putArtifactState hnd (DirCtx tmp [])
        addAction
            hnd
            (do Just (DirCtx src dests) <- getArtifactState hnd
                case reverse dests of
                    [] -> errorL hnd "not exported!"
                    (lastDest:firstDests) ->
                        liftIoProgram
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
        (tmpFileH,tmpFile) <- createFreeFile hndT
        hnd --> tmpFileH
        addAction hnd (liftIoProgram (copy externalFileName tmpFile))
        return tmpFileH

instance CanConvert IoCompiler 'FreeFile 'ExternalFile where
    runConvert hnd _ dest = do
        dest' <- liftIoProgram (ensureParentDir dest)
        newFileH <- runCreate SExternalFile dest'
        hnd --> newFileH
        copyFreeFile hnd dest'
        return newFileH

instance CanConvert IoCompiler 'FreeFile 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ mdest = do
        (newFileH,newFile) <-
            createFreeFile (maybe hndT ((hndT ++ "-") ++) mdest)
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH

instance CanExport IoCompiler 'FreeFile where
    runExport hnd destFile =
        liftIoProgram (ensureParentDir destFile) >>= copyFreeFile hnd

instance CanExport IoCompiler 'LocalDirectory where
    runExport hnd destDir = do
        destDir' <- liftIoProgram (ensureParentDir destDir)
        modifyArtifactState hnd (traverse . dirExports %~ (destDir':))

-- | Create and allocate a new 'FreeFile' and return the handle as well as the
-- path to the temporary file.
createFreeFile :: String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFile title = do
    src <- liftIoProgram (mkTemp title)
    hnd <- asFreeFile src title
    return (hnd, src)

-- | Create and allocate a new 'FreeFile' inside a given directory and
-- return the handle as well as the path to the temporary file.
createFreeFileIn :: FilePath -> String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFileIn parent title = do
    src <- liftIoProgram (mkTempIn parent title)
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
            liftIoProgram
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
freeFileTempCopy :: Handle 'FreeFile -> Maybe String -> IoCompiler FilePath
freeFileTempCopy src@(Handle _ oldName) mname = do
    let prefix = maybe oldName ((oldName ++ "-") ++) mname
    dest <- liftIoProgram (mkTemp prefix)
    copyFreeFile src dest
    return dest

-- | Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))
