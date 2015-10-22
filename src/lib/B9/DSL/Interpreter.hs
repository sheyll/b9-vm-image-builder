-- | Compile a 'Program' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.DSL.Interpreter where -- TODO rename to Compiler

import B9.DSL
import B9.B9IO
import B9.Content
       (Content(..), Environment(..), AST(..),
        YamlObject(..))
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..), Mounted, MountPoint(..))
import Text.Printf (printf)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
import Control.Monad
#endif
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
    } deriving (Show)

instance Default Ctx where
    def = Ctx 0 def def

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaData :: (AST Content YamlObject)
    , _userData :: (AST Content YamlObject)
    , _ciExports :: [ImageDestination]
    } deriving (Show)

instance Default CiCtx where
    def = CiCtx (ASTObj []) (ASTObj []) def

makeLenses ''Ctx
makeLenses ''CiCtx

-- | Compile a 'Program' to an 'IoProgram'
compile :: Program a -> IoProgram a
compile p = evalStateT compileSt def
  where
    compileSt = do
      result <- interpret p
      generateAllCI
      return result

instance Interpreter IoCompiler where
    -- Cloud-init
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        hnd@(Handle _ iid) <-
            uniqueHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        let ciCtx = set metaData (ASTObj [("instance-id", ASTString iid)]) def
        ci . at hnd ?= ciCtx
        return hnd
    -- Fall-through
    runCreate sa _src = return $ singletonHandle sa
    runUpdate _hnd _src = return ()
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _hnde _sa _src = return ()
    runExport hnd@(Handle SCloudInit _) d =
        ci . at hnd . traverse . ciExports <>= [d]
    runExport _hnd _dest = return ()

-- | Generate all the cloud init exports
generateAllCI :: IoCompiler ()
generateAllCI = do
    ciContexts <- uses ci Map.toList
    env <- uses vars (Environment . Map.toList)
    mapM_ (lift . uncurry (generateCI env)) ciContexts

-- | Generate the exports of a single cloud init instance.
generateCI :: Environment -> Handle 'CloudInit -> CiCtx -> IoProgram ()
generateCI env h ci = do
    let (Handle _ iid) = h
    tmpDir <- mkTempDir $ "CloudInit" </> iid
    let mf = tmpDir </> "meta-data"
        mc = Concat [FromString "#cloud-config\n", RenderYaml (ci ^. metaData)]
    renderContentToFile mf mc env
    return ()

-- * Utilities

uniqueHandle :: SArtifact a -> String -> IoCompiler (Handle a)
uniqueHandle sa str = do
    nextId <- use idCounter
    idCounter += 1
    return $ handle sa (str ++ "-" ++ show nextId)
