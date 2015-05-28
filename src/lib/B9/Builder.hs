{-# LANGUAGE GADTs #-}
{-|
Highest-level build functions and and B9-re-exports.
-}
module B9.Builder (buildArtifacts, module X) where
import B9.B9Monad as X
import B9.ConfigUtils as X
import B9.B9Config as X
import B9.ExecEnv as X
import B9.DiskImages as X
import B9.DiskImageBuilder as X
import B9.ShellScript as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.ArtifactGenerator as X
import B9.ArtifactGeneratorImpl as X
import B9.Vm as X
import B9.VmBuilder as X
import B9.QCUtil as X
import B9.Content.AST as X
import B9.Content.StringTemplate as X
import B9.Content.ErlTerms as X
import B9.Content.ErlangPropList as X
import B9.Content.YamlObject as X
import B9.Content.Generator as X

import Data.Monoid
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)
import Control.Monad.IO.Class
import System.Directory

import B9.B9Monad
import B9.ConfigUtils
import B9.B9Config
import B9.ExecEnv
import B9.DiskImages
import B9.DiskImageBuilder
import B9.ShellScript
import B9.Repository
import B9.RepositoryIO
import B9.ArtifactGenerator
import B9.ArtifactGeneratorImpl
import B9.Vm
import B9.VmBuilder
import B9.QCUtil

import B9.Content.AST
import B9.Content.StringTemplate
import B9.Content.ErlTerms
import B9.Content.ErlangPropList
import B9.Content.YamlObject
import B9.Content.Generator

buildArtifacts :: ArtifactGenerator -> ConfigParser -> B9Config -> IO Bool
buildArtifacts artifactGenerator cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg ->
    run cfgParser cfg $ do
      traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
      infoL "BUILDING ARTIFACTS"
      getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
      assemble artifactGenerator
      return True

withB9Config :: ConfigParser
             -> B9Config
             -> (B9Config -> IO Bool)
             -> IO Bool
withB9Config cfgParser cliCfg f = do
  let parsedCfg' = parseB9Config cfgParser
  case parsedCfg' of
    Left e -> do
      putStrLn (printf "B9 Failed to start: %s" e)
      return False
    Right parsedCfg ->
      let cfg = defaultB9Config <> parsedCfg <> cliCfg
          in f cfg
