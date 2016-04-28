module B9.Dsl.File where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import Data.Singletons.TH (singletons, Sing)

$(singletons [d|
  data FileArtifacts = ExternalFile | FreeFile | LocalDirectory
             deriving Show
  |])

instance Show (Sing 'ExternalFile) where show _ = "SExternalFile"
instance Show (Sing 'FreeFile) where show _ = "SFreeFile"
instance Show (Sing 'LocalDirectory) where show _ = "SLocalDirectory"

type instance IoCompilerArtifactState 'ExternalFile = FilePath

-- | Context of a 'SFreeFile'
data FileCtx = FileCtx
    { _fFileName :: FilePath
    , _fCopies :: [FilePath]
    } deriving (Show, Typeable)

type instance IoCompilerArtifactState 'FreeFile = FileCtx

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
    { _dirTempDir :: FilePath
    , _dirExports :: [FilePath]
    } deriving (Show, Typeable)

type instance IoCompilerArtifactState 'LocalDirectory = DirCtx

makeLenses ''FileCtx
makeLenses ''DirCtx

instance CanCreate IoCompiler 'ExternalFile where
     type CreateSpec IoCompiler 'ExternalFile = FilePath
     runCreate _ fn = do
         (hnd,_) <- allocHandle SExternalFile (takeFileName fn)
         fn' <- liftIoProgram (getRealPath fn)
         putArtifactState hnd fn'
         return hnd

instance CanCreate IoCompiler 'FreeFile where
    type CreateSpec IoCompiler 'FreeFile = Maybe String
    runCreate _ mTempName = do
        -- TODO escape tempName, allow only a-zA-Z0-9.-_:+=
        let tempName = maybe "tmp-file" takeFileName mTempName
        (hnd,_) <- createFreeFile tempName
        return hnd

instance CanCreate IoCompiler 'LocalDirectory where
    type CreateSpec IoCompiler 'LocalDirectory = ()
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
    type AddSpec IoCompiler 'LocalDirectory 'FreeFile = (FileSpec, Handle 'FreeFile)
    runAdd dirH _ (fSpec,fH) = do
        Just localDir <- useArtifactState dirH
        copyFreeFile' fH (localDir ^. dirTempDir) fSpec
        fH --> dirH

instance CanExtract IoCompiler 'ExternalFile 'FreeFile where
    runExtract hnd _ () = do
        Just externalFileName <- useArtifactState hnd
        (tmpFileH,tmpFile) <- createFreeFile (handleTitle hnd)
        hnd --> tmpFileH
        addAction hnd (liftIoProgram (copy externalFileName tmpFile))
        return tmpFileH

instance CanExtract IoCompiler 'FreeFile 'ExternalFile where
    type ExtractionArg IoCompiler 'FreeFile 'ExternalFile = FilePath
    runExtract hnd _ dest = do
        dest' <- liftIoProgram (ensureParentDir dest)
        newFileH <- runCreate SExternalFile dest'
        hnd --> newFileH
        copyFreeFile hnd dest'
        return newFileH

instance CanExtract IoCompiler 'FreeFile 'FreeFile where
    type ExtractionArg IoCompiler 'FreeFile 'FreeFile = Maybe String
    runExtract hnd _ mdest = do
        let hndT = handleTitle hnd
        (newFileH,newFile) <-
            createFreeFile (maybe hndT ((hndT ++ "-") ++) mdest)
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH

instance CanExport IoCompiler 'FreeFile where
    type ExportSpec IoCompiler 'FreeFile = FilePath
    runExport hnd destFile =
        liftIoProgram (ensureParentDir destFile) >>= copyFreeFile hnd

instance CanExport IoCompiler 'LocalDirectory where
    type ExportSpec IoCompiler 'LocalDirectory = FilePath
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
freeFileTempCopy src mname = do
    let prefix = maybe oldName ((oldName ++ "-") ++) mname
        oldName = handleTitle src
    dest <- liftIoProgram (mkTemp prefix)
    copyFreeFile src dest
    return dest

-- | Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))
