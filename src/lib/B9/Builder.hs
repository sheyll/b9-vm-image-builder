{-# LANGUAGE GADTs #-}
{-|
Highest-level build functions and and B9-re-exports.
-}
module B9.Builder (buildArtifacts, module X) where
import B9.B9Monad as X
import Data.ConfigFile.B9Extras as X
import B9.B9Config as X
import B9.ExecEnv as X
import B9.DiskImages as X
import B9.DiskImageBuilder as X
import B9.Invokation as X
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

import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)
import Control.Monad.IO.Class
import System.Directory

-- | Execute an 'ArtifactGenerator' and return a 'B9Invokation' that returns
-- the build id obtained by 'getBuildId'.
buildArtifacts :: ArtifactGenerator -> B9Invokation String ()
buildArtifacts artifactGenerator = run $ const $ do
  traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
  infoL "BUILDING ARTIFACTS"
  getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
  assemble artifactGenerator
  getBuildId