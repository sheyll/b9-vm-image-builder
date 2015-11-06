{-|
Static B9 configuration. Read, write and merge configurable properties.
The properties are independent of specific build targets.
-}
module B9.B9Config
       (B9Config(..), defaultB9ConfigFile, defaultRepositoryCache,
        defaultB9Config, getB9ConfigFile, writeB9Config,
        writeInitialB9Config, readB9Config, parseB9Config, LogLevel(..),
        BuildVariables, GlobalOpts(..), parseGlobalOpts,
        parseBuildVars, getGlobalOptsFromCLI)
       where

import B9.ConfigUtils
import B9.Content
import B9.ExecEnv
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics (Generic)
import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty hiding ((</>))
import Paths_b9 (version)
import System.Directory
import Text.Printf

type BuildVariables = [(String, String)]

data GlobalOpts = GlobalOpts
    { configFile :: Maybe SystemPath
    , cliB9Config :: B9Config
    }

parseBuildVars :: Parser BuildVariables
parseBuildVars =
    zip (("arg_" ++) . show <$> ([1 ..] :: [Int])) <$> many (strArgument idm)

getGlobalOptsFromCLI :: IO (GlobalOpts, Environment)
getGlobalOptsFromCLI =
    execParser
        (info
             (helper <*>
              ((,) <$> parseGlobalOpts <*> (Environment <$> parseBuildVars)))
             (fullDesc <> headerDoc (Just helpHeader)))
  where
    helpHeader = linebreak <> text ("A B9 build script ")

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts =
    toGlobalOpts <$>
    optional
        (strOption
             (help "Path to user's b9-configuration (default: ~/.b9/b9.conf)" <>
              short 'c' <>
              long "configuration-file" <>
              metavar "FILENAME")) <*>
    switch
        (help "Log everything that happens to stdout" <> short 'v' <>
         long "verbose") <*>
    switch (help "Suppress non-error output" <> short 'q' <> long "quiet") <*>
    optional
        (strOption
             (help "Path to a logfile" <> short 'l' <> long "log-file" <>
              metavar "FILENAME")) <*>
    optional
        (strOption
             (help "Output file for a command/timing profile" <>
              long "profile-file" <>
              metavar "FILENAME")) <*>
    optional
        (strOption
             (help "Root directory for build directories" <> short 'b' <>
              long "build-root-dir" <>
              metavar "DIRECTORY")) <*>
    switch
        (help "Keep build directories after exit" <> short 'k' <>
         long "keep-build-dir") <*>
    switch
        (help "Predictable build directory names" <> short 'u' <>
         long "predictable-build-dir") <*>
    optional
        (strOption
             (help
                  "Cache directory for shared images, default: '~/.b9/repo-cache'" <>
              long "repo-cache" <>
              metavar "DIRECTORY")) <*>
    optional
        (strOption
             (help "Remote repository to share image to" <> short 'r' <>
              long "repo" <>
              metavar "REPOSITORY_ID"))
  where
    toGlobalOpts
        :: Maybe FilePath
        -> Bool
        -> Bool
        -> Maybe FilePath
        -> Maybe FilePath
        -> Maybe FilePath
        -> Bool
        -> Bool
        -> Maybe FilePath
        -> Maybe String
        -> GlobalOpts
    toGlobalOpts cfg verbose quiet logF profF buildRoot keep notUnique mRepoCache repo =
        let minLogLevel =
                if verbose
                    then Just LogTrace
                    else if quiet
                             then Just LogError
                             else Nothing
            b9cfg' =
                let b9cfg =
                        mempty
                        { verbosity = minLogLevel
                        , logFile = logF
                        , profileFile = profF
                        , buildDirRoot = buildRoot
                        , keepTempDirs = keep
                        , uniqueBuildDirs = not notUnique
                        , repository = repo
                        }
                in b9cfg
                   { repositoryCache = Path <$> mRepoCache
                   }
        in GlobalOpts
           { configFile = (Path <$> cfg) <|> pure defaultB9ConfigFile
           , cliB9Config = b9cfg'
           }

data LogLevel
    = LogTrace
    | LogDebug
    | LogInfo
    | LogError
    | LogNothing
    deriving (Eq,Show,Ord,Read,Generic,Data,Typeable)

data B9Config = B9Config
    { verbosity :: Maybe LogLevel
    , logFile :: Maybe FilePath
    , buildDirRoot :: Maybe FilePath
    , keepTempDirs :: Bool
    , execEnvType :: ExecEnvType
    , profileFile :: Maybe FilePath
    , envVars :: BuildVariables
    , uniqueBuildDirs :: Bool
    , repositoryCache :: Maybe SystemPath
    , repository :: Maybe String
    , interactive :: Bool
    } deriving (Show)


instance Monoid B9Config where
    mempty =
        B9Config
            Nothing
            Nothing
            Nothing
            False
            LibVirtLXC
            Nothing
            []
            True
            Nothing
            Nothing
            False
    mappend c c' =
        B9Config
        { verbosity = getLast $ on mappend (Last . verbosity) c c'
        , logFile = getLast $ on mappend (Last . logFile) c c'
        , buildDirRoot = getLast $ on mappend (Last . buildDirRoot) c c'
        , keepTempDirs = getAny $ on mappend (Any . keepTempDirs) c c'
        , execEnvType = LibVirtLXC
        , profileFile = getLast $ on mappend (Last . profileFile) c c'
        , envVars = on mappend envVars c c'
        , uniqueBuildDirs = getAll
              ((mappend `on` (All . uniqueBuildDirs)) c c')
        , repositoryCache = getLast $ on mappend (Last . repositoryCache) c c'
        , repository = getLast ((mappend `on` (Last . repository)) c c')
        , interactive = getAny ((mappend `on` (Any . interactive)) c c')
        }

defaultB9Config :: B9Config
defaultB9Config =
    B9Config
    { verbosity = Just LogInfo
    , logFile = Nothing
    , buildDirRoot = Nothing
    , keepTempDirs = False
    , execEnvType = LibVirtLXC
    , profileFile = Nothing
    , envVars = []
    , uniqueBuildDirs = True
    , repository = Nothing
    , repositoryCache = Just defaultRepositoryCache
    , interactive = False
    }

defaultRepositoryCache :: SystemPath
defaultRepositoryCache = InB9UserDir "repo-cache"
defaultB9ConfigFile :: SystemPath
defaultB9ConfigFile = InB9UserDir "b9.conf"
verbosityK :: String
verbosityK = "verbosity"
logFileK :: String
logFileK = "log_file"
buildDirRootK :: String
buildDirRootK = "build_dir_root"
keepTempDirsK :: String
keepTempDirsK = "keep_temp_dirs"
execEnvTypeK :: String
execEnvTypeK = "exec_env"
profileFileK :: String
profileFileK = "profile_file"
envVarsK :: String
envVarsK = "environment_vars"
uniqueBuildDirsK :: String
uniqueBuildDirsK = "unique_build_dirs"
repositoryCacheK :: String
repositoryCacheK = "repository_cache"
repositoryK :: String
repositoryK = "repository"
cfgFileSection :: String
cfgFileSection = "global"

getB9ConfigFile
    :: MonadIO m
    => Maybe SystemPath -> m FilePath
getB9ConfigFile mCfgFile = do
    cfgFile <- resolve (fromMaybe defaultB9ConfigFile mCfgFile)
    ensureDir cfgFile
    return cfgFile

writeB9Config
    :: MonadIO m
    => Maybe SystemPath -> ConfigParser -> m ()
writeB9Config cfgFileIn cp = do
    cfgFile <- getB9ConfigFile cfgFileIn
    liftIO (writeFile cfgFile (to_string cp))

writeInitialB9Config
    :: MonadIO m
    => Maybe SystemPath -> B9Config -> ConfigParser -> m ()
writeInitialB9Config Nothing cliCfg cpNonGlobal =
    writeInitialB9Config (Just defaultB9ConfigFile) cliCfg cpNonGlobal
writeInitialB9Config (Just cfgPath) cliCfg cpNonGlobal = do
    cfgFile <- resolve cfgPath
    ensureDir cfgFile
    exists <- liftIO $ doesFileExist cfgFile
    unless exists $
        let res = do
                let cp = emptyCP
                    c = defaultB9Config <> cliCfg
                cp1 <- add_section cp cfgFileSection
                cp2 <- setshow cp1 cfgFileSection verbosityK (verbosity c)
                cp3 <- setshow cp2 cfgFileSection logFileK (logFile c)
                cp4 <-
                    setshow cp3 cfgFileSection buildDirRootK (buildDirRoot c)
                cp5 <-
                    setshow cp4 cfgFileSection keepTempDirsK (keepTempDirs c)
                cp6 <- setshow cp5 cfgFileSection execEnvTypeK (execEnvType c)
                cp7 <- setshow cp6 cfgFileSection profileFileK (profileFile c)
                cp8 <- setshow cp7 cfgFileSection envVarsK (envVars c)
                cp9 <-
                    setshow
                        cp8
                        cfgFileSection
                        uniqueBuildDirsK
                        (uniqueBuildDirs c)
                cpA <-
                    setshow
                        cp9
                        cfgFileSection
                        repositoryCacheK
                        (repositoryCache c)
                cpB <- setshow cpA cfgFileSection repositoryK (repository c)
                return $ merge cpB cpNonGlobal
        in case res of
               Left e -> liftIO (throwIO (IniFileException cfgFile e))
               Right cp -> liftIO (writeFile cfgFile (to_string cp))

readB9Config
    :: MonadIO m
    => Maybe SystemPath -> m ConfigParser
readB9Config Nothing = readB9Config (Just defaultB9ConfigFile)
readB9Config (Just cfgFile) = readIniFile cfgFile

parseB9Config :: ConfigParser -> Either String B9Config
parseB9Config cp =
    let getr
            :: (Get_C a, Read a)
            => OptionSpec -> Either CPError a
        getr = get cp cfgFileSection
        getB9Config =
            B9Config <$> getr verbosityK <*> getr logFileK <*>
            getr buildDirRootK <*>
            getr keepTempDirsK <*>
            getr execEnvTypeK <*>
            getr profileFileK <*>
            getr envVarsK <*>
            getr uniqueBuildDirsK <*>
            getr repositoryCacheK <*>
            getr repositoryK <*>
            pure False
    in case getB9Config of
           Left err ->
               Left
                   (printf
                        "Failed to parse B9 configuration file: '%s'"
                        (show err))
           Right x -> Right x
