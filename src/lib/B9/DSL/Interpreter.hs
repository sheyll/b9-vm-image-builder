-- | Compile a 'Program' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.DSL.Interpreter where -- TODO rename to Compiler

import B9.B9IO
import B9.Content
       (Content(..), Environment(..), AST(..),
        YamlObject(..),astMerge,FileSpec(..))
import B9.DSL
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..), Mounted, MountPoint(..), FileSystemCreation(..))
import B9.ShellScript (Script(..), toBashOneLiner)
import Text.Printf (printf)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Control.Monad
import Data.Monoid
#endif
import System.FilePath
import Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (from, (<.>))
import Data.Default
import Data.Maybe

-- | The monad used to compile a 'Program' into an 'IoProgram'
type IoCompiler = StateT Ctx IoProgram

-- | The internal state of the 'IoCompiler' monad
data Ctx = Ctx
    { _idCounter :: Int
    , _vars :: Map.Map String String
    , _ci :: Map.Map (Handle 'CloudInit) CiCtx
    , _fileContent :: Map.Map (Handle 'FileContent) Content
    , _localDirs :: Map.Map (Handle 'LocalDirectory) DirCtx
    , _fileSystems :: Map.Map (Handle 'FileSystemImage) FsCtx
    } deriving (Show)

instance Default Ctx where
    def = Ctx 0 def def def def def

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
    { _dirTempDir :: FilePath
    , _dirFiles :: Map.Map FileSpec (Handle 'FileContent)
    , _dirExports :: [FilePath]
    } deriving (Show)

instance Default DirCtx where
    def = DirCtx "/tmp" def def

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsCreation :: FileSystemCreation
    , _fsFiles :: Map.Map FileSpec (Handle 'FileContent)
    , _fsExports :: [FilePath]
    } deriving (Show)

instance Default FsCtx where
    def = FsCtx (FileSystemCreation Ext4 "/" 10 MB) def def

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaData :: (AST Content YamlObject)
    , _userData :: (AST Content YamlObject)
    , _metaDataH :: Handle 'FileContent
    , _userDataH :: Handle 'FileContent
    } deriving (Show)

instance Default CiCtx where
    def =
        CiCtx
            (ASTObj [])
            (ASTObj [])
            (singletonHandle SFileContent)
            (singletonHandle SFileContent)

makeLenses ''Ctx
makeLenses ''CiCtx
makeLenses ''DirCtx
makeLenses ''FsCtx

-- | Compile a 'Program' to an 'IoProgram'
compile :: Program a -> IoProgram a
compile p = evalStateT compileSt def
  where
    compileSt = do
        result <- interpret p
        generateAllCloudInitContent
        generateAllDirectories
        generateAllFileSystems
        return result

instance Interpreter IoCompiler where
    -- Create
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        mh <- interpret $ createContent (FromString "#cloud-config\n")
        uh <- interpret $ createContent (FromString "#cloud-config\n")
        hnd@(Handle _ iid) <-
            uniqueHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        let ciCtx =
                def & metaData .~ (ASTObj [("instance-id", ASTString iid)]) &
                userData .~
                (ASTObj []) &
                metaDataH .~
                mh &
                userDataH .~
                uh
        ci . at hnd ?= ciCtx
        return hnd
    runCreate SFileContent c = do
        hnd <- uniqueHandle SFileContent ""
        fileContent . at hnd ?= c
        return hnd
    runCreate SLocalDirectory () = do
        tmp <- lift $ mkTempDir "local-dir"
        hnd <- uniqueHandle SLocalDirectory tmp
        localDirs . at hnd ?= (def & dirTempDir .~ tmp)
        return hnd
    runCreate SFileSystemImage fsCreate@(FileSystemCreation _ fsLabel _ _) = do
        hnd <- uniqueHandle SFileSystemImage fsLabel
        fileSystems . at hnd ?= (def & fsCreation .~ fsCreate)
        return hnd
    runCreate sa _src = return $ singletonHandle sa
    -- Update
    runUpdate hnd@(Handle SFileContent _) c = do
        fileContent . at hnd . traverse %=
            \cOld ->
                 Concat [cOld, c]
        return ()
    runUpdate _hnd _src = return ()
    -- Add
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _ STemplateVariable (k,v) = vars . at k ?= v
    runAdd hnd@(Handle SLocalDirectory _) SFileContent (fSpec,cHnd) =
        localDirs . at hnd . traverse . dirFiles . at fSpec ?= cHnd
    runAdd hnd@(Handle SFileSystemImage _) SFileContent (fSpec,cHnd) =
        fileSystems . at hnd . traverse . fsFiles . at fSpec ?= cHnd
    runAdd hnd@(Handle SCloudInit _) SCloudInitMetaData ast =
        ci . at hnd . traverse . metaData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SCloudInitUserData ast =
        ci . at hnd . traverse . userData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SFileContent (fspec,contentHnd) = do
        Just content <- use (fileContent . at contentHnd)
        runAdd hnd SCloudInitUserData (toUserDataWriteFilesAST fspec content)
    runAdd hnd@(Handle SCloudInit _) SExecutableScript scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)
    runAdd _hnde _sa _src = return ()
    -- Export
    runExport hnd@(Handle SCloudInit _) () = do
        mh <- use (ci . at hnd . to fromJust . metaDataH)
        uh <- use (ci . at hnd . to fromJust . userDataH)
        return (mh, uh)
    runExport hnd@(Handle SLocalDirectory _) destDir = do
        localDirs . at hnd . traverse . dirExports <>= [destDir]
        return destDir
    runExport hnd@(Handle SFileSystemImage _) destFile = do
        fileSystems . at hnd . traverse . fsExports <>= [destFile]
        (FileSystemCreation fsType _ _ _) <-
            use (fileSystems . at hnd . to fromJust . fsCreation)
        return (Image destFile Raw fsType)
    runExport _hnd _dest = fail "Not Yet Implemented"

-- | Create a @cloud-config@ compatibe @write_files@ 'AST' object.
toUserDataWriteFilesAST :: FileSpec -> Content -> AST Content YamlObject
toUserDataWriteFilesAST (FileSpec fileName (s,u,g,o) userName groupName) content =
    ASTObj
        [ ( "write_files"
          , ASTArr
                [ ASTObj
                      [ ("path", ASTString fileName)
                      , ("owner", ASTString (userName ++ ":" ++ groupName))
                      , ("permissions", ASTString (printf "%i%i%i%i" s u g o))
                      , ("content", ASTEmbed content)]])]

-- | Create a @cloud-config@ compatibe @runcmd@ 'AST' object.
toUserDataRunCmdAST :: Script -> AST Content YamlObject
toUserDataRunCmdAST scr = ASTObj [("runcmd", ASTArr [ASTString cmd])]
  where
    cmd = toBashOneLiner scr

-- | Merge all the content accumulated in the user-data and meta-data of CI
-- artifacts into the corresponding file contents.
generateAllCloudInitContent :: IoCompiler ()
generateAllCloudInitContent = do
    ciContexts <- use (ci . to Map.toList)
    mapM_ updateContentWithCI ciContexts
  where
    updateContentWithCI (_ciHnd,ciCtx) = do
        let uH = ciCtx ^. userDataH
            uC = ciCtx ^. userData
            mH = ciCtx ^. metaDataH
            mC = ciCtx ^. metaData
        interpret $
            do appendContent mH (RenderYaml mC)
               appendContent uH (RenderYaml uC)
        use fileContent

-- | Generate 'SLocalDirectory' exports
generateAllDirectories :: IoCompiler ()
generateAllDirectories = do
    dirs <- uses localDirs Map.toList
    mapM_ generateDir dirs
  where
    generateDir (_h,c) = do
        let fcs = Map.toList (c ^. dirFiles)
            tmpDir = c ^. dirTempDir
            exports = c ^. dirExports
        void $ generateFileContentsToDir tmpDir fcs
        forM_ exports (lift . copyToDest tmpDir)
    copyToDest src dest = do
        src' <- getRealPath src
        dest' <- ensureParentDir dest
        copyDir src' dest'

-- | Generate 'SFileSystemImage' exports
generateAllFileSystems :: IoCompiler ()
generateAllFileSystems = do
    fss <- uses fileSystems Map.toList
    mapM_ generateFS fss
  where
    generateFS (_h,c) = do
        let fcs = Map.toList (c ^. fsFiles)
            fsc = c ^. fsCreation
            exports = c ^. fsExports
        tmpDir <- lift $ mkTempDir "file-system-content"
        files <- generateFileContentsToDir tmpDir fcs
        tmpFsImage <- lift $ mkTemp "file-system-image"
        lift $ createFileSystem tmpFsImage fsc tmpDir (snd <$> files)
        forM_ exports (lift . copyToDest tmpFsImage)
    copyToDest src dest = do
        dest' <- ensureParentDir dest
        copy src dest'

-- | Generate a list iof file contents to a temp directory and return a list of
-- absolute file paths of all files added/created. These paths are in temporary
-- directories and are designed to be removed as soon as possible, e.g. when the
-- program terminates.
generateFileContentsToDir
    :: FilePath
    -> [(FileSpec, Handle 'FileContent)]
    -> IoCompiler [(FilePath, FileSpec)]
generateFileContentsToDir tmpDir fcs = do
    env <- uses vars (Environment . Map.toList)
    mapM (gen env) fcs
  where
    gen env (fspec@(FileSpec fp _ _ _),cHnd) = do
        fp' <- lift $ ensureParentDir (tmpDir </> fp)
        c <- use (fileContent . at cHnd . to fromJust)
        lift $ renderContentToFile fp' c env
        return (fp', fspec)

-- * Utilities

uniqueHandle :: SArtifact a -> String -> IoCompiler (Handle a)
uniqueHandle sa str = do
    nextId <- use idCounter
    idCounter += 1
    return $
        handle sa $
        if str == ""
            then (show nextId)
            else (str ++ "-" ++ show nextId)
