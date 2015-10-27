{-# LANGUAGE GADTs #-}
{-|
Highest-level build functions and and B9-re-exports.
-}
module B9.Builder
       (buildArtifacts, runProgram, runProgramWithConfigAndCliArgs,
        runIoProgram, defaultMain, configure, module X)
       where
import B9.ArtifactGenerator as X
import B9.ArtifactGeneratorImpl as X
import B9.B9Config as X
import B9.B9Monad as X
import B9.ConfigUtils as X
import B9.Content as X
import B9.DiskImageBuilder as X
import B9.DiskImages as X
import B9.ExecEnv as X
import B9.QCUtil as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.ShellScript as X
import B9.Vm as X
import B9.VmBuilder as X
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import System.Directory
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)
import qualified B9.DSL as Neu
import qualified B9.DSL.Interpreter as Neu
import qualified B9.B9IO as Neu
import qualified B9.B9IOImpl as Neu
import qualified B9.LibVirtLXC as LibVirtLXC

-- | Use this in your 'B9' script to run a 'Program'.
defaultMain :: Neu.Program () -> IO ()
defaultMain = void . runProgramWithConfigAndCliArgs

-- | Execute a 'Program' using all b9 command line options
-- and settings from the b9 configuration file.
runProgramWithConfigAndCliArgs :: Neu.Program () -> IO Bool
runProgramWithConfigAndCliArgs p = do
    (opts,vars) <- getGlobalOptsFromCLI
    let cfgCli = cliB9Config opts
        cfgFile = configFile opts
    cp <- configure cfgFile cfgCli
    runProgram (p >> return True) vars cp cfgCli

-- | Merge 'existingConfig' with the configuration from the b9 config
-- file. If the file does not exists, a new config file with the given
-- configuration will be written. The return value is a parser for the config
-- file. Returning the raw config file parser allows modules unkown to
-- 'B9.B9Config' to add their own values to the shared config file.
configure
    :: MonadIO m
    => Maybe SystemPath -> B9Config -> m ConfigParser
configure b9ConfigPath existingConfig = do
    writeInitialB9Config
        b9ConfigPath
        existingConfig
        LibVirtLXC.setDefaultConfig
    readB9Config b9ConfigPath

-- | Execute a 'Program'.
runProgram :: Neu.Program Bool
           -> Environment
           -> ConfigParser
           -> B9Config
           -> IO Bool
runProgram dsl (Environment vars) cfgParser cliCfg =
    runIoProgram (Neu.traceEveryAction (Neu.compile dsl')) cfgParser cliCfg
  where
    dsl' = do
        mapM_ (uncurry (Neu.$=)) vars
        dsl

buildArtifacts :: ArtifactGenerator -> ConfigParser -> B9Config -> IO Bool
buildArtifacts artifactGenerator cfgParser cliCfg =
    withB9Config cfgParser cliCfg $
    \cfg ->
         run cfgParser cfg $
         do traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
            infoL "BUILDING ARTIFACTS"
            getConfig >>=
                traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
            void $ assemble artifactGenerator
            return True

runIoProgram :: Neu.IoProgram Bool -> ConfigParser -> B9Config -> IO Bool
runIoProgram prog cfgParser cliCfg =
    withB9Config cfgParser cliCfg $
    \cfg ->
         run cfgParser cfg $
         do traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
            infoL "BUILDING ARTIFACTS"
            getConfig >>=
                traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
            Neu.executeIoProg prog

withB9Config :: ConfigParser -> B9Config -> (B9Config -> IO Bool) -> IO Bool
withB9Config cfgParser cliCfg f = do
    let parsedCfg' = parseB9Config cfgParser
    case parsedCfg' of
        Left e -> do
            putStrLn (printf "B9 Failed to start: %s" e)
            return False
        Right parsedCfg ->
            let cfg = defaultB9Config <> parsedCfg <> cliCfg
            in f cfg
