module Main where

import B9
import Control.Exception
import Data.ConfigFile as CF
import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty hiding ((</>))
import Paths_b9
import System.IO.Error hiding (isDoesNotExistErrorType)

data GlobalConfig = GlobalConfig {}

data BuildConfig = BuildConfig { _envVars :: [(String, String)]
                               , _configParser :: ConfigParser
                               , _globalOptions :: GlobalConfig
                               }

data EnvVar = EnvVar { _envVarKey :: String
                     , _envVarValue :: String
                     }

type BuildAction = BuildConfig -> IO Bool


main :: IO ()
main = do
    b9Opts <- parseCommandLine
    result <- runB9Command b9Opts
    exit result
    where exit success = unless success (exitWith (ExitFailure 128))

parseCommandLine :: IO CommandLineInvocation
parseCommandLine = execParser
    ( info
        (   helper
        <*> (   CommandLineInvocation
            <$> parseGlobalConfig
            <*> cmds
            <*> parseBuildVars
            )
        )
        (  fullDesc
        <> progDesc
               "Build and run VM-Images inside LXC containers. Custom arguments follow after '--' and are accessable in many strings in build files  trough shell like variable references, i.e. '${arg_N}' referes to positional argument $N.\n\nRepository names passed to the command line are looked up in the B9 configuration file, which is per default located in: '~/.b9/b9.conf'"
        <> headerDoc (Just helpHeader)
        )
    )
  where
    helpHeader =
        linebreak <> text ("B9 - a benign VM-Image build tool v. " ++ b9Version)


data CommandLineInvocation =
    CommandLineInvocation
        { _cliEnvVars :: [EnvVar]
        , _cliGlobalConfig :: GlobalConfig
        , _cliBuildAction :: BuildAction
        }

runB9Command :: CommandLineInvocation -> IO Bool
runB9Command cli = do
    let cfgWithArgs = cfgCli { _envVars = envVars cfgCli }
        cfgCli      = cliBuildConfig globalOpts
        cfgFile     = configFile globalOpts
    cp <- configure cfgFile cfgCli
    action cfgFile cp cfgWithArgs

runShowVersion :: BuildAction
runShowVersion _ = do
    putStrLn b9Version
    return True

cmds :: Parser BuildAction
cmds = subparser
    ( command
        "version"
        (info (pure runShowVersion) (progDesc "Show program version and exit."))
    )

buildFileParser :: Parser [FilePath]
buildFileParser =
    helper
        <*> some
                ( strOption
                    (  help
                          "Build file to load, specify multiple build files (each witch '-f') to build them all in a single run."
                    <> short 'f'
                    <> long "project-file"
                    <> metavar "FILENAME"
                    <> noArgError (ErrorMsg "No build file specified!")
                    )
                )


b9Version :: String
b9Version = showVersion version









