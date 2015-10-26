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
        SizeUnit(..), Mounted, MountPoint(..))
import B9.ShellScript (Script(..), toBashOneLiner)
import Text.Printf (printf)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Control.Monad
#endif
import Data.Monoid
import System.FilePath
import Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (from, (<.>))
import Data.Default

-- | The monad used to compile a 'Program' into an 'IoProgram'
type IoCompiler = StateT Ctx IoProgram

-- | The internal state of the 'IoCompiler' monad
data Ctx = Ctx
    { _idCounter :: Int
    , _vars :: Map.Map String String
    , _ci :: Map.Map (Handle 'CloudInit) CiCtx
    , _fileContent :: Map.Map (Handle 'FileContent) Content
    , _dirs :: Map.Map (Handle 'LocalDirectory) DirCtx
    , _fs :: Map.Map (Handle 'FileSystemImage) FsCtx
    } deriving (Show)

instance Default Ctx where
    def = Ctx 0 def def def def def

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
    { _dirExports :: [FilePath]
    , _dirPath :: FilePath
    } deriving (Show)

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsExports :: [FilePath]
    , _fsCreation :: FileSystemCreation
    } deriving (Show)

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaData :: (AST Content YamlObject)
    , _userData :: (AST Content YamlObject)
    , _metaDataH :: Handle 'FileContent
    , _userDataH :: Handle 'FileContent
    } deriving (Show)

instance Default CiCtx where
    def = CiCtx (ASTObj []) (ASTObj []) def

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
      generateAllCI
      return result

instance Interpreter IoCompiler where
    -- Create
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        mh <- interpret $ createContent (FromString "#cloud-config\n")
        uh <- interpret $ createContent (FromString "#cloud-config\n")
        hnd@(Handle _ iid) <-
            uniqueHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        let ciCtx = def & metaData .~ (ASTObj [("instance-id", ASTString iid)])
                        & userData .~ (ASTObj [])
                        & metaDataH .~ mh
                        & userDataH .~ uh
        ci . at hnd ?= ciCtx
        return hnd
    runCreate SFileContent c = do
        hnd <- uniqueHandle SFileContent ""
        fileContent . at hnd ?= c
        return hnd
    runCreate sa _src = return $ singletonHandle sa
    -- Update
    runUpdate hnd@(Handle SFileContent _) c =
      fileContent . at hnd . traverse %= \cOld -> Concat [cOld, c]
      return ()
    runUpdate _hnd _src = return ()
    -- Add
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _ STemplateVariable (k,v) = vars . at k ?= v
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
    runExport hnd@(Handle SCloudInit _) d =
        ci . at hnd . traverse . ciExports <>= [d]
    runExport _hnd _dest = return ()

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

-- | Wrap either the meta-data or user-data 'AST' into a 'Content' that contains
--   the obligatory first line with the string @#cloud-config@.
wrapCloudConfigAST :: AST Content YamlObject -> Content
wrapCloudConfigAST a = Concat [FromString "#cloud-config\n", RenderYaml a]

-- | Generate all the cloud init exports
generateAllCI :: IoCompiler ()
generateAllCI = do
    ciContexts <- uses ci Map.toList
    env <- uses vars (Environment . Map.toList)
    mapM_ (lift . uncurry (generateCI env)) ciContexts

-- | Generate the exports of a single cloud init instance.
generateCI :: Environment -> Handle 'CloudInit -> CiCtx -> IoProgram ()
generateCI env h c = do
    let (Handle _ iid) = h
        numberOfExports = c ^. ciExports . to length
    when (numberOfExports > 0) $
        do tmpDir <- mkTempDir $ "CloudInit" </> iid
           let mf = tmpDir </> "meta-data"
               mc = wrapCloudConfigAST (c ^. metaData)
               uf = tmpDir </> "user-data"
               uc = wrapCloudConfigAST (c ^. userData)
           renderContentToFile mf mc env
           renderContentToFile uf uc env
           mapM_
               (generateCIExport (numberOfExports == 1) tmpDir)
               (c ^. ciExports)
           return ()
  where
    generateCIExport _resuseTmpDir tmpDir (Left outDir) = do
        copyDir tmpDir outDir
    generateCIExport resuseTmpDir tmpDir (Right dest) =
        convertImageTo resuseTmpDir src dest
      where
        src = ImageFromDir tmpDir "cidata" ISO9660 Raw (ImageSize 10 MB)


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
