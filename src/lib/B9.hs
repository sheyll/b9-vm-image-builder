{-| B9 is a library and build tool with primitive operations to rmrun a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.ArtifactGenerator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 (configure, b9_version, module X, defaultMain) where
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative as X
#endif
import           Control.Monad as X
import           Control.Monad.IO.Class as X
import           Data.Monoid as X
import           Data.List as X
import           Data.Maybe as X
import           Text.Show.Pretty as X (ppShow)
import           System.Exit as X (exitWith, ExitCode(..))
import           System.FilePath as X
       (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import           Text.Printf as X (printf)
import           Data.Version as X
import           B9.Builder as X
import           Paths_b9 (version)
import qualified B9.LibVirtLXC as LibVirtLXC
import qualified B9.DSL as Neu

-- | Merge 'existingConfig' with the configuration from the main b9 config
-- file. If the file does not exists, a new config file with the given
-- configuration will be written. The return value is a parser for the config
-- file. Returning the raw config file parser allows modules unkown to
-- 'B9.B9Config' to add their own values to the shared config file.
configure :: MonadIO m => Maybe SystemPath -> B9Config -> m ConfigParser
configure b9ConfigPath existingConfig = do
  writeInitialB9Config b9ConfigPath existingConfig LibVirtLXC.setDefaultConfig
  readB9Config b9ConfigPath

-- | Return the cabal package version of the B9 library.
b9_version :: Version
b9_version = version

defaultMain :: Neu.Program () -> IO Bool
defaultMain p = do
    (opts,vars) <- getGlobalOptsFromCLI
    let cfgCli = cliB9Config opts
        cfgFile = configFile opts
    cp <- configure cfgFile cfgCli
    runProgram (p >> return True) vars cp cfgCli
