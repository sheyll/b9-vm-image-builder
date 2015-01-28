module B9.Builder ( module B9.B9Monad
                  , module B9.ConfigUtils
                  , module B9.B9Config
                  , module B9.ExecEnv
                  , module B9.DiskImages
                  , module B9.DiskImageBuilder
                  , module B9.ShellScript
                  , module B9.Repository
                  , module B9.RepositoryIO
                  , module B9.ArtifactGenerator
                  , module B9.ArtifactGeneratorImpl
                  , buildArtifacts
                  ) where
import Data.Data
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)

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

buildArtifacts :: ArtifactGenerator -> ConfigParser -> B9Config -> IO Bool
buildArtifacts artifactGenerator cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg -> do
    run cfgParser cfg $ do
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
