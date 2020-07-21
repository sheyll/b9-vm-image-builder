{-# LANGUAGE NumericUnderscores #-}

-- |
-- Static B9 configuration. Read, write and merge configurable properties.
-- The properties are independent of specific build targets.
module B9.B9Config
  ( B9Config (..),
    Timeout (..),
    runB9ConfigReader,
    B9ConfigReader,
    getB9Config,
    getConfig,
    getLogVerbosity,
    getProjectRoot,
    getRemoteRepos,
    isInteractive,
    B9ConfigWriter,
    verbosity,
    logFile,
    projectRoot,
    keepTempDirs,
    uniqueBuildDirs,
    repositoryCache,
    repository,
    interactive,
    defaultTimeout,
    libVirtLXCConfigs,
    dockerConfigs,
    podmanConfigs,
    systemdNspawnConfigs,
    remoteRepos,
    timeoutFactor,
    maxLocalSharedImageRevisionsK,
    maxLocalSharedImageRevisions,
    B9ConfigOverride (..),
    noB9ConfigOverride,
    B9ConfigAction (),
    runB9ConfigActionWithOverrides,
    runB9ConfigAction,
    localB9Config,
    modifyPermanentConfig,
    customB9Config,
    customB9ConfigPath,
    customEnvironment,
    customDefaulB9ConfigPath,
    overrideB9ConfigPath,
    overrideDefaultB9ConfigPath,
    overrideB9Config,
    overrideWorkingDirectory,
    overrideDefaultTimeout,
    overrideTimeoutFactor,
    overrideVerbosity,
    overrideKeepBuildDirs,
    defaultB9ConfigFile,
    defaultRepositoryCache,
    defaultB9Config,
    openOrCreateB9Config,
    writeB9CPDocument,
    readB9Config,
    parseB9Config,
    modifyCPDocument,
    b9ConfigToCPDocument,
    LogLevel (..),
    Environment,
    module X,
  )
where

import B9.B9Config.Container as X
import B9.B9Config.Docker as X
import B9.B9Config.LibVirtLXC as X
import B9.B9Config.Podman as X
import B9.B9Config.Repository as X
import B9.B9Config.SystemdNspawn as X
import B9.Environment
import B9.QCUtil (smaller)
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Writer.Lazy
import Control.Exception
import Control.Lens as Lens ((<>~), (?~), (^.), makeLenses, set)
import Control.Monad ((>=>), filterM)
import Control.Monad.IO.Class
import Data.ConfigFile.B9Extras
  ( CPDocument,
    CPError,
    CPGet,
    CPOptionSpec,
    CPReadException (..),
    addSectionCP,
    emptyCP,
    mergeCP,
    readCP,
    readCPDocument,
    setShowCP,
    toStringCP,
  )
import Data.Function (on)
import Data.List (inits)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Semigroup as Semigroup hiding (Last (..))
import Data.Version
import GHC.Stack
import qualified Paths_b9 as My
import System.Directory
import System.FilePath ((<.>))
import System.IO.B9Extras (SystemPath (..), ensureDir, resolve)
import Text.Printf (printf)
import Test.QuickCheck (Arbitrary(arbitrary))
import qualified Test.QuickCheck as QuickCheck

-- | A way to specify a time intervall for example for the timeouts
-- of system commands.
--
-- @since 1.1.0
newtype Timeout = TimeoutMicros Int
  deriving (Show, Eq, Ord, Read)

instance Arbitrary Timeout where
  arbitrary = do 
    QuickCheck.Positive t <- arbitrary 
    pure (TimeoutMicros t)

data LogLevel
  = LogTrace
  | LogDebug
  | LogInfo
  | LogError
  | LogNothing
  deriving (Eq, Show, Ord, Read)

instance Arbitrary LogLevel where 
  arbitrary = QuickCheck.elements [LogTrace, LogDebug, LogInfo, LogError, LogNothing]

data B9Config
  = B9Config
      { _verbosity :: Maybe LogLevel,
        _logFile :: Maybe FilePath,
        _projectRoot :: Maybe FilePath,
        _keepTempDirs :: Bool,
        _uniqueBuildDirs :: Bool,
        _repositoryCache :: Maybe SystemPath,
        _repository :: Maybe String,
        _interactive :: Bool,
        _maxLocalSharedImageRevisions :: Maybe Int,
        _systemdNspawnConfigs :: Maybe SystemdNspawnConfig,
        _podmanConfigs :: Maybe PodmanConfig,
        _dockerConfigs :: Maybe DockerConfig,
        _libVirtLXCConfigs :: Maybe LibVirtLXCConfig,
        _remoteRepos :: [RemoteRepo],
        _defaultTimeout :: Maybe Timeout,
        _timeoutFactor :: Maybe Int
      }
  deriving (Show, Eq)

instance Arbitrary B9Config where 
  arbitrary = 
    B9Config 
      <$> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> (QuickCheck.elements [Nothing, Just (InTempDir "xxx")])
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> pure Nothing
      <*> pure Nothing
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 
      <*> smaller arbitrary 

instance Semigroup B9Config where
  c <> c' =
    B9Config
      { _verbosity = getLast $ on mappend (Last . _verbosity) c c',
        _logFile = getLast $ on mappend (Last . _logFile) c c',
        _projectRoot = getLast $ on mappend (Last . _projectRoot) c c',
        _keepTempDirs = getAny $ on mappend (Any . _keepTempDirs) c c',
        _uniqueBuildDirs = getAll ((mappend `on` (All . _uniqueBuildDirs)) c c'),
        _repositoryCache = getLast $ on mappend (Last . _repositoryCache) c c',
        _repository = getLast ((mappend `on` (Last . _repository)) c c'),
        _interactive = getAny ((mappend `on` (Any . _interactive)) c c'),
        _maxLocalSharedImageRevisions = getLast ((mappend `on` (Last . _maxLocalSharedImageRevisions)) c c'),
        _systemdNspawnConfigs = getLast ((mappend `on` (Last . _systemdNspawnConfigs)) c c'),
        _podmanConfigs = getLast ((mappend `on` (Last . _podmanConfigs)) c c'),
        _dockerConfigs = getLast ((mappend `on` (Last . _dockerConfigs)) c c'),
        _libVirtLXCConfigs = getLast ((mappend `on` (Last . _libVirtLXCConfigs)) c c'),
        _remoteRepos = (mappend `on` _remoteRepos) c c',
        _defaultTimeout = getLast $ on mappend (Last . _defaultTimeout) c c',
        _timeoutFactor = getLast $ on mappend (Last . _timeoutFactor) c c'
      }

instance Monoid B9Config where
  mappend = (<>)
  mempty = B9Config Nothing Nothing Nothing False True Nothing Nothing False Nothing Nothing Nothing Nothing Nothing [] (Just (TimeoutMicros (60 * 60 * 1_000_000))) Nothing

-- | Reader for 'B9Config'. See 'getB9Config' and 'localB9Config'.
--
-- @since 0.5.65
type B9ConfigReader = Reader B9Config

-- | Run a 'B9ConfigReader'.
--
-- @since 0.5.65
runB9ConfigReader :: HasCallStack => B9Config -> Eff (B9ConfigReader ': e) a -> Eff e a
runB9ConfigReader = runReader

-- | Return the runtime configuration, that should be the configuration merged
-- from all configuration sources. This is the configuration to be used during
-- a VM image build.
--
-- @since 0.5.65
getB9Config :: Member B9ConfigReader e => Eff e B9Config
getB9Config = ask

-- | Run an action with an updated runtime configuration.
--
-- @since 0.5.65
localB9Config :: Member B9ConfigReader e => (B9Config -> B9Config) -> Eff e a -> Eff e a
localB9Config = local

-- | An alias for 'getB9Config'.
--
-- @deprecated
--
-- @since 0.5.65
getConfig :: Member B9ConfigReader e => Eff e B9Config
getConfig = getB9Config

-- | Ask whether @stdin@ of the @B9@ process should be redirected to the
-- external commands executed during the build.
--
-- @since 0.5.65
isInteractive :: Member B9ConfigReader e => Eff e Bool
isInteractive = _interactive <$> getB9Config

-- | Ask for the 'RemoteRepo's.
--
-- @since 0.5.65
getRemoteRepos :: Member B9ConfigReader e => Eff e [RemoteRepo]
getRemoteRepos = _remoteRepos <$> getB9Config

-- | Ask for the 'LogLevel'.
--
-- @since 0.5.65
getLogVerbosity :: Member B9ConfigReader e => Eff e (Maybe LogLevel)
getLogVerbosity = _verbosity <$> getB9Config

-- | Ask for the project root directory.
--
-- @since 0.5.65
getProjectRoot :: Member B9ConfigReader e => Eff e FilePath
getProjectRoot = fromMaybe "." . _projectRoot <$> ask

-- | Override b9 configuration items and/or the path of the b9 configuration file.
-- This is useful, i.e. when dealing with command line parameters.
data B9ConfigOverride
  = B9ConfigOverride
      { _customB9ConfigPath :: Maybe SystemPath,
        _customB9Config :: Endo B9Config,
        _customEnvironment :: Environment,
        _customDefaulB9ConfigPath :: Maybe SystemPath
      }

instance Show B9ConfigOverride where
  show x =
    unlines
      [ "config file path:    " ++ show (_customB9ConfigPath x),
        "config modification: " ++ show (appEndo (_customB9Config x) mempty),
        "environment:         " ++ show (_customEnvironment x),
        "default config file: " ++ show (_customDefaulB9ConfigPath x)
      ]

-- | An empty default 'B9ConfigOverride' value, that will neither apply any
-- additional 'B9Config' nor change the path of the configuration file.
noB9ConfigOverride :: B9ConfigOverride
noB9ConfigOverride = B9ConfigOverride Nothing mempty mempty Nothing

makeLenses ''B9Config

makeLenses ''B9ConfigOverride

-- | Convenience utility to override the B9 configuration file path.
overrideB9ConfigPath :: SystemPath -> B9ConfigOverride -> B9ConfigOverride
overrideB9ConfigPath p = customB9ConfigPath ?~ p

-- | Modify the runtime configuration.
overrideB9Config :: (B9Config -> B9Config) -> B9ConfigOverride -> B9ConfigOverride
overrideB9Config e = customB9Config <>~ Endo e

-- | Convenience utility to override the *default* B9 configuration file path.
--
-- @since 1.1.0
overrideDefaultB9ConfigPath :: SystemPath -> B9ConfigOverride -> B9ConfigOverride
overrideDefaultB9ConfigPath p = customDefaulB9ConfigPath ?~ p

-- | Define the current working directory to be used when building.
overrideWorkingDirectory :: FilePath -> B9ConfigOverride -> B9ConfigOverride
overrideWorkingDirectory p = overrideB9Config (projectRoot ?~ p)

-- | Define the default timeout for external commands.
--
-- @since 1.1.0
overrideDefaultTimeout :: Maybe Timeout -> B9ConfigOverride -> B9ConfigOverride
overrideDefaultTimeout = overrideB9Config . Lens.set defaultTimeout

-- | Define the timeout factor for external commands.
--
-- @since 1.1.0
overrideTimeoutFactor :: Maybe Int -> B9ConfigOverride -> B9ConfigOverride
overrideTimeoutFactor = overrideB9Config . Lens.set timeoutFactor

-- | Overwrite the 'verbosity' settings in the configuration with those given.
overrideVerbosity :: LogLevel -> B9ConfigOverride -> B9ConfigOverride
overrideVerbosity = overrideB9Config . Lens.set verbosity . Just

-- | Overwrite the 'keepTempDirs' flag in the configuration with those given.
overrideKeepBuildDirs :: Bool -> B9ConfigOverride -> B9ConfigOverride
overrideKeepBuildDirs = overrideB9Config . Lens.set keepTempDirs

-- | A monad that gives access to the (transient) 'B9Config' to be used at
-- _runtime_ with 'getB9Config' or 'localB9Config', and that allows
-- to write permanent 'B9Config' changes back to the configuration file using
-- 'modifyPermanentConfig'. This is the amalgamation of 'B9ConfigWriter'
-- 'B9ConfigReader' and 'IO'.
--
-- @since 0.5.65
type B9ConfigAction a = Eff '[B9ConfigWriter, B9ConfigReader, EnvironmentReader, Lift IO] a

-- | Accumulate 'B9Config' changes that go back to the config file. See
-- 'B9ConfigAction' and 'modifyPermanentConfig'.
--
-- @since 0.5.65
type B9ConfigWriter = Writer (Semigroup.Endo B9Config)

-- | Add a modification to the permanent configuration file.
modifyPermanentConfig :: (HasCallStack, Member B9ConfigWriter e) => Endo B9Config -> Eff e ()
modifyPermanentConfig = tell

-- | Execute a 'B9ConfigAction'.
-- It will take a 'B9ConfigOverride' as input. The 'B9Config' in that value is
-- treated as the _runtime_ configuration, and the '_customConfigPath' is used
-- as the alternative location of the configuration file.
-- The configuration file is read from either the path in '_customB9ConfigPath'
-- or from 'defaultB9ConfigFile'.
-- Every modification done via 'modifyPermanentConfig' is applied to
-- the **contents** of the configuration file
-- and written back to that file, note that these changes are ONLY reflected
-- in the configuration file and **not** in the _runtime configuration_.
--
-- See also 'runB9ConfigAction', which does not need the 'B9ConfigOverride' parameter.
--
-- @since 0.5.65
runB9ConfigActionWithOverrides :: HasCallStack => B9ConfigAction a -> B9ConfigOverride -> IO a
runB9ConfigActionWithOverrides act cfg = do
  configuredCfgPaths <- traverse resolve (cfg ^. customB9ConfigPath)
  defCfgPath <- resolve (fromMaybe defaultB9ConfigFile (cfg ^. customDefaulB9ConfigPath))
  let (Version myVer _) = My.version
      appendVersionVariations name =
        (\v' -> name <.> showVersion (makeVersion v')) <$> reverse (inits myVer)
      (pathsToTry, pathsToCreate) =
        case configuredCfgPaths of
          Just configuredCfgPath ->
            (appendVersionVariations configuredCfgPath, Nothing)
          Nothing ->
            (appendVersionVariations defCfgPath, Just defCfgPath)
  existingCfgPaths <- filterM doesFileExist pathsToTry
  cfgPath <-
    case existingCfgPaths of
      (cfgPath : _) ->
        return cfgPath
      [] -> do
        putStrLn ("B9 config file resolver: None of these files exists " ++ show pathsToTry)
        case pathsToCreate of
          Just c -> do
            putStrLn ("creating a new config file with defaults at: " ++ c)
            return c
          Nothing ->
            error "Please provide a valid config file path."
  cp <- openOrCreateB9Config cfgPath
  case parseB9Config cp of
    Left e -> error (printf "Internal configuration load error, please report this: %s\n" (show e))
    Right permanentConfigIn -> do
      let runtimeCfg = appEndo (cfg ^. customB9Config) permanentConfigIn
      (res, permanentB9ConfigUpdates) <-
        runLift (runEnvironmentReader (cfg ^. customEnvironment) (runReader runtimeCfg (runMonoidWriter act)))
      let cpExtErr = modifyCPDocument cp <$> permanentB9ConfigUpdateMaybe
          permanentB9ConfigUpdateMaybe =
            if appEndo permanentB9ConfigUpdates permanentConfigIn == permanentConfigIn
              then Nothing
              else Just permanentB9ConfigUpdates
      cpExt <-
        maybe
          (return Nothing)
          (either (error . printf "Internal configuration update error! Please report this: %s\n" . show) (return . Just))
          cpExtErr
      mapM_ (writeB9CPDocument (cfg ^. customB9ConfigPath)) cpExt
      return res

-- | Run a 'B9ConfigAction' using 'noB9ConfigOverride'.
-- See 'runB9ConfigActionWithOverrides' for more details.
--
-- @since 0.5.65
runB9ConfigAction :: HasCallStack => B9ConfigAction a -> IO a
runB9ConfigAction = flip runB9ConfigActionWithOverrides noB9ConfigOverride

-- | Open the configuration file that contains the 'B9Config'.
-- If the configuration does not exist, write a default configuration file,
-- and create a all missing directories.
openOrCreateB9Config :: (HasCallStack, MonadIO m) => FilePath -> m CPDocument
openOrCreateB9Config cfgFile = do
  ensureDir cfgFile
  liftIO $ do
    exists <- doesFileExist cfgFile
    if exists
      then readCPDocument (Path cfgFile)
      else
        let res = b9ConfigToCPDocument defaultB9Config
         in case res of
              Left e -> throwIO (CPReadException cfgFile e)
              Right cp -> writeFile cfgFile (toStringCP cp) >> return cp

-- | Write the configuration in the 'CPDocument' to either the user supplied
-- configuration file path or to 'defaultB9ConfigFile'.
-- Create all missing (parent) directories.
writeB9CPDocument :: (HasCallStack, MonadIO m) => Maybe SystemPath -> CPDocument -> m ()
writeB9CPDocument cfgFileIn cp = do
  cfgFile <- resolve (fromMaybe defaultB9ConfigFile cfgFileIn)
  ensureDir cfgFile
  liftIO (writeFile cfgFile (toStringCP cp))

defaultB9Config :: B9Config
defaultB9Config =
  B9Config
    { _verbosity = Just LogInfo,
      _logFile = Nothing,
      _projectRoot = Nothing,
      _keepTempDirs = False,
      _uniqueBuildDirs = True,
      _repository = Nothing,
      _repositoryCache = Just defaultRepositoryCache,
      _interactive = False,
      _maxLocalSharedImageRevisions = Just 2,
      _systemdNspawnConfigs = Just defaultSystemdNspawnConfig,
      _podmanConfigs = Just defaultPodmanConfig,
      _libVirtLXCConfigs = Just defaultLibVirtLXCConfig,
      _dockerConfigs = Just defaultDockerConfig,
      _remoteRepos = [],
      _defaultTimeout = Just (TimeoutMicros (3_600_000_000)),
      _timeoutFactor = Nothing
    }

defaultRepositoryCache :: SystemPath
defaultRepositoryCache = InB9UserDir "repo-cache"

defaultB9ConfigFile :: SystemPath
defaultB9ConfigFile = InB9UserDir "b9.conf"

verbosityK :: String
verbosityK = "verbosity"

logFileK :: String
logFileK = "log_file"

projectRootK :: String
projectRootK = "build_dir_root"

keepTempDirsK :: String
keepTempDirsK = "keep_temp_dirs"

uniqueBuildDirsK :: String
uniqueBuildDirsK = "unique_build_dirs"

repositoryCacheK :: String
repositoryCacheK = "repository_cache"

maxLocalSharedImageRevisionsK :: String
maxLocalSharedImageRevisionsK = "max_cached_shared_images"

repositoryK :: String
repositoryK = "repository"

defaultTimeoutK :: String
defaultTimeoutK = "default_timeout_seconds"

timeoutFactorK :: String
timeoutFactorK = "timeout_factor"

cfgFileSection :: String
cfgFileSection = "global"

-- | Parse a 'B9Config', modify it, and merge it back to the given 'CPDocument'.
modifyCPDocument :: CPDocument -> Endo B9Config -> Either CPError CPDocument
modifyCPDocument cp f = do
  cfg <- parseB9Config cp
  cp2 <- b9ConfigToCPDocument (appEndo f cfg)
  return (mergeCP cp cp2)

-- | Append a config file section for the 'B9Config' to an empty 'CPDocument'.
b9ConfigToCPDocument :: HasCallStack => B9Config -> Either CPError CPDocument
b9ConfigToCPDocument c = do
  cp1 <- addSectionCP emptyCP cfgFileSection
  cp2 <- setShowCP cp1 cfgFileSection verbosityK (_verbosity c)
  cp3 <- setShowCP cp2 cfgFileSection logFileK (_logFile c)
  cp4 <- setShowCP cp3 cfgFileSection projectRootK (_projectRoot c)
  cp5 <- setShowCP cp4 cfgFileSection keepTempDirsK (_keepTempDirs c)
  cp7 <- setShowCP cp5 cfgFileSection uniqueBuildDirsK (_uniqueBuildDirs c)
  cp8 <- setShowCP cp7 cfgFileSection maxLocalSharedImageRevisionsK (_maxLocalSharedImageRevisions c)
  cp9 <- setShowCP cp8 cfgFileSection repositoryCacheK (_repositoryCache c)
  cpA <- foldr (>=>) return (systemdNspawnConfigToCPDocument <$> _systemdNspawnConfigs c) cp9
  cpB <- foldr (>=>) return (podmanConfigToCPDocument <$> _podmanConfigs c) cpA
  cpC <- foldr (>=>) return (dockerConfigToCPDocument <$> _dockerConfigs c) cpB
  cpD <- foldr (>=>) return (libVirtLXCConfigToCPDocument <$> _libVirtLXCConfigs c) cpC
  cpE <- foldr (>=>) return (remoteRepoToCPDocument <$> _remoteRepos c) cpD
  cpF <- setShowCP cpE cfgFileSection repositoryK (_repository c)
  cpFinal <- setShowCP cpF cfgFileSection defaultTimeoutK (_defaultTimeout c)
  setShowCP cpFinal cfgFileSection timeoutFactorK (_timeoutFactor c)

readB9Config :: (HasCallStack, MonadIO m) => Maybe SystemPath -> m CPDocument
readB9Config cfgFile = readCPDocument (fromMaybe defaultB9ConfigFile cfgFile)

parseB9Config :: HasCallStack => CPDocument -> Either CPError B9Config
parseB9Config cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in B9Config <$> getr verbosityK <*> getr logFileK <*> getr projectRootK <*> getr keepTempDirsK
        <*> getr uniqueBuildDirsK
        <*> getr repositoryCacheK
        <*> getr repositoryK
        <*> pure False
        <*> pure (either (const Nothing) id (getr maxLocalSharedImageRevisionsK))
        <*> pure (either (const Nothing) Just (parseSystemdNspawnConfig cp))
        <*> pure (either (const Nothing) Just (parsePodmanConfig cp))
        <*> pure (either (const Nothing) Just (parseDockerConfig cp))
        <*> pure (either (const Nothing) Just (parseLibVirtLXCConfig cp))
        <*> parseRemoteRepos cp
        <*> pure (either (const Nothing) Just (parseDefaultTimeoutConfig cp))
        <*> pure (either (const Nothing) Just (getr timeoutFactorK))

parseDefaultTimeoutConfig :: CPDocument -> Either CPError Timeout
parseDefaultTimeoutConfig cp = do
  seconds <- readCP cp cfgFileSection defaultTimeoutK
  let mu = seconds * 1_000_000
  return (TimeoutMicros mu)
