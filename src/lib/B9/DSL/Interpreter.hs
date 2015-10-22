-- | Compile a 'Program' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.DSL.Interpreter where -- TODO rename to Compiler

import B9.DSL
import B9.B9Monad hiding (getBuildId)
import B9.B9IO

import B9.ArtifactGenerator
       (CloudInitType(..), InstanceId(..))
import B9.B9Config (ExecEnvType(..))
import B9.Content (Content(..), FileSpec(..), fileSpec)
import B9.Content.AST (AST(..))
import B9.Content.StringTemplate
       (SourceFile(..), SourceFileConversion(..))
import B9.Content.YamlObject (YamlObject(..))
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..), Mounted, MountPoint(..))
import Text.Printf (printf)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
#endif
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Data.Map as Map
import Control.Monad.State
import Control.Lens hiding (from, (<.>))
import Control.Lens.TH

-- | Transform 'Program' to 'IoProgram'
compile :: Program a -> IoProgram a
compile p = evalStateT (interpret p) initialCtx

type IoCompiler = StateT Ctx IoProgram

data Ctx = Ctx {
  _idCounter :: Int,
  _vars :: Map.Map String String
  }

initialCtx :: Ctx
initialCtx = Ctx 0 Map.empty

makeLenses ''Ctx

instance Interpreter IoCompiler where
    -- Cloud-init
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        hnd@(Handle _ iid) <-
            uniqueHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        ciTempDir <- lift $ mkTemp $ "CloudInit" </> iid
        return hnd
    -- Fall-through
    runCreate sa src = do
        let hnd = singletonHandle sa
        return hnd
    runUpdate hnd src = return ()
    runAdd documentation SDocumentation str = lift $ logTrace str
    runAdd hnde sa src = return ()
    runExport hnd dest = return ()

-- * Utilities

uniqueHandle :: SArtifact a -> String -> IoCompiler (Handle a)
uniqueHandle sa str = do
  nextId <- use idCounter
  idCounter += 1
  return $ handle sa $ show nextId
