{-# LANGUAGE GADTs #-}
{-|
Highest-level build functions and and B9-re-exports.
-}
module B9.Builder
       (runProgram, runProgramWithConfigAndCliArgs,
        runIoProgram, defaultMain, configure, module X)
       where
import B9.B9Config as X
import qualified B9.B9Monad as B9M
import B9.ConfigUtils as X
import B9.Content as X
import B9.DiskImageBuilder as X
import B9.DiskImages as X
import B9.ExecEnv as X
import B9.QCUtil as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.ShellScript as X
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import System.Directory
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)
import B9.DSL as X
import B9.DSL.Interpreter as X
import B9.B9IO as X
import B9.B9IOImpl as X
import qualified B9.LibVirtLXC as LibVirtLXC

-- | Use this in your 'B9' script to run a 'Program'.
defaultMain :: Program () -> IO ()
defaultMain = void . runProgramWithConfigAndCliArgs

-- | Execute a 'Program' using all b9 command line options
-- and settings from the b9 configuration file.
runProgramWithConfigAndCliArgs :: Program () -> IO Bool
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
runProgram :: Program Bool
           -> Environment
           -> ConfigParser
           -> B9Config
           -> IO Bool
runProgram dsl (Environment vars) cfgParser cliCfg =
    runIoProgram (traceEveryAction (compile dsl')) cfgParser cliCfg
  where
    dsl' = do
        mapM_ (uncurry ($=)) vars
        dsl

runIoProgram :: IoProgram Bool -> ConfigParser -> B9Config -> IO Bool
runIoProgram prog cfgParser cliCfg =
    withB9Config cfgParser cliCfg $
    \cfg ->
         B9M.runB9Monad cfgParser cfg $
         do B9M.traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
            B9M.infoL "BUILDING ARTIFACTS"
            B9M.getConfig >>=
                B9M.traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
            executeIoProg prog

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
