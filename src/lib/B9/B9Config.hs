{-|
Static B9 configuration. Read, write and merge configurable properties.
The properties are independent of specific build targets.
-}
module B9.B9Config
  ( B9Config(..)
  , verbosity
  , logFile
  , buildDirRoot
  , keepTempDirs
  , execEnvType
  , profileFile
  , envVars
  , uniqueBuildDirs
  , repositoryCache
  , repository
  , interactive
  , libVirtLXCConfigs
  , remoteRepos
  , maxLocalSharedImageRevisions
  , B9ConfigOverride(..)
  , noB9ConfigOverride
  , B9ConfigAction()
  , execB9ConfigAction
  , invokeB9
  , askRuntimeConfig
  , localRuntimeConfig
  , modifyPermanentConfig
  , customB9Config
  , customB9ConfigPath
  , customLibVirtNetwork
  , overrideB9ConfigPath
  , overrideB9Config
  , overrideWorkingDirectory
  , overrideVerbosity
  , overrideKeepBuildDirs
  , defaultB9ConfigFile
  , defaultRepositoryCache
  , defaultB9Config
  , openOrCreateB9Config
  , writeB9CPDocument
  , readB9Config
  , parseB9Config
  , appendPositionalArguments
  , modifyCPDocument
  , b9ConfigToCPDocument
  , LogLevel(..)
  , ExecEnvType(..)
  , BuildVariables
  , module X
  ) where

import B9.B9Config.LibVirtLXC as X
import B9.B9Config.Repository as X
import Control.Exception
import Control.Lens as Lens ((&), (.~), (?~), (^.), _Just, makeLenses, over, set)
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.ConfigFile.B9Extras
  ( CPDocument
  , CPError
  , CPGet
  , CPOptionSpec
  , CPReadException(..)
  , addSectionCP
  , emptyCP
  , mergeCP
  , readCP
  , readCPDocument
  , setShowCP
  , toStringCP
  )
import Data.Function (on)
import Data.List (partition, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Semigroup as Sem
import System.Directory
import System.IO.B9Extras (SystemPath(..), ensureDir, resolve)
import Text.Printf (printf)

type BuildVariables = [(String, String)]

data ExecEnvType =
  LibVirtLXC
  deriving (Eq, Show, Ord, Read)

data LogLevel
  = LogTrace
  | LogDebug
  | LogInfo
  | LogError
  | LogNothing
  deriving (Eq, Show, Ord, Read)

data B9Config = B9Config
  { _verbosity :: Maybe LogLevel
  , _logFile :: Maybe FilePath
  , _buildDirRoot :: Maybe FilePath
  , _keepTempDirs :: Bool
  , _execEnvType :: ExecEnvType
  , _profileFile :: Maybe FilePath
  , _envVars :: BuildVariables
  , _uniqueBuildDirs :: Bool
  , _repositoryCache :: Maybe SystemPath
  , _repository :: Maybe String
  , _interactive :: Bool
  , _maxLocalSharedImageRevisions :: Maybe Int
  , _libVirtLXCConfigs :: Maybe LibVirtLXCConfig
  , _remoteRepos :: [RemoteRepo]
  } deriving (Show)

instance Sem.Semigroup B9Config where
  c <> c' =
    B9Config
      { _verbosity = getLast $ on mappend (Last . _verbosity) c c'
      , _logFile = getLast $ on mappend (Last . _logFile) c c'
      , _buildDirRoot = getLast $ on mappend (Last . _buildDirRoot) c c'
      , _keepTempDirs = getAny $ on mappend (Any . _keepTempDirs) c c'
      , _execEnvType = LibVirtLXC
      , _profileFile = getLast $ on mappend (Last . _profileFile) c c'
      , _envVars = on mappend _envVars c c'
      , _uniqueBuildDirs = getAll ((mappend `on` (All . _uniqueBuildDirs)) c c')
      , _repositoryCache = getLast $ on mappend (Last . _repositoryCache) c c'
      , _repository = getLast ((mappend `on` (Last . _repository)) c c')
      , _interactive = getAny ((mappend `on` (Any . _interactive)) c c')
      , _maxLocalSharedImageRevisions = getLast ((mappend `on` (Last . _maxLocalSharedImageRevisions)) c c')
      , _libVirtLXCConfigs = getLast ((mappend `on` (Last . _libVirtLXCConfigs)) c c')
      , _remoteRepos = (mappend `on` _remoteRepos) c c'
      }

instance Monoid B9Config where
  mappend = (Sem.<>)
  mempty = B9Config Nothing Nothing Nothing False LibVirtLXC Nothing [] True Nothing Nothing False Nothing Nothing []

-- | Override b9 configuration items and/or the path of the b9 configuration file.
-- This is useful, i.e. when dealing with command line parameters.
data B9ConfigOverride = B9ConfigOverride
  { _customB9ConfigPath :: Maybe SystemPath
  , _customB9Config :: B9Config
  , _customLibVirtNetwork :: Maybe (Maybe String)
  } deriving (Show)

-- | An empty default 'B9ConfigOverride' value, that will neither apply any
-- additional 'B9Config' nor change the path of the configuration file.
noB9ConfigOverride :: B9ConfigOverride
noB9ConfigOverride = B9ConfigOverride Nothing mempty mempty

makeLenses ''B9Config

makeLenses ''B9ConfigOverride

-- | Convenience utility to override the B9 configuration file path.
overrideB9ConfigPath :: SystemPath -> B9ConfigOverride -> B9ConfigOverride
overrideB9ConfigPath p = customB9ConfigPath ?~ p

-- | Modify the runtime configuration.
overrideB9Config :: (B9Config -> B9Config) -> B9ConfigOverride -> B9ConfigOverride
overrideB9Config = over customB9Config

-- | Define the current working directory to be used when building.
overrideWorkingDirectory :: FilePath -> B9ConfigOverride -> B9ConfigOverride
overrideWorkingDirectory p = customB9Config . buildDirRoot ?~ p

-- | Overwrite the 'verbosity' settings in the configuration with those given.
overrideVerbosity :: LogLevel -> B9ConfigOverride -> B9ConfigOverride
overrideVerbosity = overrideB9Config . Lens.set verbosity . Just

-- | Overwrite the 'keepTempDirs' flag in the configuration with those given.
overrideKeepBuildDirs :: Bool -> B9ConfigOverride -> B9ConfigOverride
overrideKeepBuildDirs = overrideB9Config . Lens.set keepTempDirs

-- | A monad that gives access to the (transient) 'B9Config' to be used at
-- _runtime_ with 'askRuntimeConfig' or 'localRuntimeConfig', and that allows
-- to write permanent 'B9Config' changes back to the configuration file using
-- 'modifyPermanentConfig'. Execute a 'B9ConfigAction' by invoking
-- either 'invokeB9' (which is simple) or 'execB9ConfigAction'.
newtype B9ConfigAction m a = B9ConfigAction
  { runB9ConfigAction :: ReaderT B9Config (WriterT [Endo B9Config] m) a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- | Return the runtime configuration, that should be the configuration merged
-- from all configuration sources. This is the configuration to be used during
-- a VM image build.
askRuntimeConfig :: Monad m => B9ConfigAction m B9Config
askRuntimeConfig = B9ConfigAction ask

-- | Run an action with an updated runtime configuration.
localRuntimeConfig :: Monad m => (B9Config -> B9Config) -> B9ConfigAction m a -> B9ConfigAction m a
localRuntimeConfig f = B9ConfigAction . local f . runB9ConfigAction

-- | Add a modification to the permanent configuration file.
modifyPermanentConfig :: Monad m => Endo B9Config -> B9ConfigAction m ()
modifyPermanentConfig f = B9ConfigAction (tell [f])

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
-- See also 'invokeB9', which does not need the 'B9ConfigOverride' parameter.
execB9ConfigAction :: MonadIO m => B9ConfigAction m a -> B9ConfigOverride -> m a
execB9ConfigAction act cfg = do
  let cfgPath = cfg ^. customB9ConfigPath
  cp <- openOrCreateB9Config cfgPath
  case parseB9Config cp of
    Left e -> fail (printf "Internal configuration load error, please report this: %s\n" (show e))
    Right permanentConfig -> do
      let runtimeCfg =
            let rc = permanentConfig Sem.<> (cfg ^. customB9Config)
             in case cfg ^. customLibVirtNetwork of
                  Just overridenNetwork -> rc & libVirtLXCConfigs . _Just . networkId .~ overridenNetwork
                  Nothing -> rc
      (res, permanentB9ConfigUpdates) <- runWriterT (runReaderT (runB9ConfigAction act) runtimeCfg)
      let cpExtErr = modifyCPDocument cp <$> permanentB9ConfigUpdate
          permanentB9ConfigUpdate =
            if null permanentB9ConfigUpdates
              then Nothing
              else Just (mconcat permanentB9ConfigUpdates)
      cpExt <-
        maybe
          (return Nothing)
          (either (fail . printf "Internal configuration update error! Please report this: %s\n" . show) (return . Just))
          cpExtErr
      mapM_ (writeB9CPDocument (cfg ^. customB9ConfigPath)) cpExt
      return res

-- | Run a 'B9ConfigAction' using 'noB9ConfigOverride'.
-- See 'execB9ConfigAction' for more details.
invokeB9 :: MonadIO m => B9ConfigAction m a -> m a
invokeB9 = flip execB9ConfigAction noB9ConfigOverride

-- | Open the configuration file that contains the 'B9Config'.
-- If the configuration does not exist, write a default configuration file,
-- and create a all missing directories.
openOrCreateB9Config :: MonadIO m => Maybe SystemPath -> m CPDocument
openOrCreateB9Config cfgPath = do
  cfgFile <- resolve (fromMaybe defaultB9ConfigFile cfgPath)
  ensureDir cfgFile
  liftIO $ do
    exists <- doesFileExist cfgFile
    if exists
      then readCPDocument (Path cfgFile)
      else let res = b9ConfigToCPDocument defaultB9Config
            in case res of
                 Left e -> throwIO (CPReadException cfgFile e)
                 Right cp -> writeFile cfgFile (toStringCP cp) >> return cp

-- | Write the configuration in the 'CPDocument' to either the user supplied
-- configuration file path or to 'defaultB9ConfigFile'.
-- Create all missing (parent) directories.
writeB9CPDocument :: MonadIO m => Maybe SystemPath -> CPDocument -> m ()
writeB9CPDocument cfgFileIn cp = do
  cfgFile <- resolve (fromMaybe defaultB9ConfigFile cfgFileIn)
  ensureDir cfgFile
  liftIO (writeFile cfgFile (toStringCP cp))

defaultB9Config :: B9Config
defaultB9Config =
  B9Config
    { _verbosity = Just LogInfo
    , _logFile = Nothing
    , _buildDirRoot = Nothing
    , _keepTempDirs = False
    , _execEnvType = LibVirtLXC
    , _profileFile = Nothing
    , _envVars = []
    , _uniqueBuildDirs = True
    , _repository = Nothing
    , _repositoryCache = Just defaultRepositoryCache
    , _interactive = False
    , _maxLocalSharedImageRevisions = Just 2
    , _libVirtLXCConfigs = Just defaultLibVirtLXCConfig
    , _remoteRepos = []
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

maxLocalSharedImageRevisionsK :: String
maxLocalSharedImageRevisionsK = "max_cached_shared_images"

repositoryK :: String
repositoryK = "repository"

cfgFileSection :: String
cfgFileSection = "global"

-- | Parse a 'B9Config', modify it, and merge it back to the given 'CPDocument'.
modifyCPDocument :: CPDocument -> Endo B9Config -> Either CPError CPDocument
modifyCPDocument cp f = do
  cfg <- parseB9Config cp
  cp2 <- b9ConfigToCPDocument (appEndo f cfg)
  return (mergeCP cp cp2)

-- | Append a config file section for the 'B9Config' to an empty 'CPDocument'.
b9ConfigToCPDocument :: B9Config -> Either CPError CPDocument
b9ConfigToCPDocument c = do
  cp1 <- addSectionCP emptyCP cfgFileSection
  cp2 <- setShowCP cp1 cfgFileSection verbosityK (_verbosity c)
  cp3 <- setShowCP cp2 cfgFileSection logFileK (_logFile c)
  cp4 <- setShowCP cp3 cfgFileSection buildDirRootK (_buildDirRoot c)
  cp5 <- setShowCP cp4 cfgFileSection keepTempDirsK (_keepTempDirs c)
  cp6 <- setShowCP cp5 cfgFileSection execEnvTypeK (_execEnvType c)
  cp7 <- setShowCP cp6 cfgFileSection profileFileK (_profileFile c)
  cp8 <- setShowCP cp7 cfgFileSection envVarsK (_envVars c)
  cp9 <- setShowCP cp8 cfgFileSection uniqueBuildDirsK (_uniqueBuildDirs c)
  cpA <- setShowCP cp9 cfgFileSection maxLocalSharedImageRevisionsK (_maxLocalSharedImageRevisions c)
  cpB <- setShowCP cpA cfgFileSection repositoryCacheK (_repositoryCache c)
  cpC <- foldr (>=>) return (libVirtLXCConfigToCPDocument <$> _libVirtLXCConfigs c) cpB
  cpFinal <- foldr (>=>) return (remoteRepoToCPDocument <$> _remoteRepos c) cpC
  setShowCP cpFinal cfgFileSection repositoryK (_repository c)

readB9Config :: MonadIO m => Maybe SystemPath -> m CPDocument
readB9Config cfgFile = readCPDocument (fromMaybe defaultB9ConfigFile cfgFile)

parseB9Config :: CPDocument -> Either CPError B9Config
parseB9Config cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
      getB9Config =
        B9Config <$> getr verbosityK <*> getr logFileK <*> getr buildDirRootK <*> getr keepTempDirsK <*>
        getr execEnvTypeK <*>
        getr profileFileK <*>
        getr envVarsK <*>
        getr uniqueBuildDirsK <*>
        getr repositoryCacheK <*>
        getr repositoryK <*>
        pure False <*>
        pure (either (const Nothing) id (getr maxLocalSharedImageRevisionsK)) <*>
        pure (either (const Nothing) Just (parseLibVirtLXCConfig cp)) <*>
        parseRemoteRepos cp
   in getB9Config

-- | If environment variables @arg_1 .. arg_n@ are bound
-- and a list of @k@ additional values are passed to this function,
-- store them with keys @arg_(n+1) .. arg_(n+k)@.
appendPositionalArguments :: [String] -> B9Config -> B9Config
appendPositionalArguments extraPositional c = c {_envVars = appendVars (_envVars c)}
  where
    appendVars argsOld =
      let (oldPositional, oldOther) = partition (("arg_" ==) . take 4 . fst) argsOld
          oldPositionalSortedByPosition =
            map snd $ sortBy (compare `on` fst) $ map (\(x, y) -> ((read :: String -> Int) $ drop 4 x, y)) oldPositional
          newPositional =
            let xs = oldPositionalSortedByPosition ++ extraPositional
             in [("arg_" ++ show i, a) | (i, a) <- zip [1 :: Int ..] xs]
       in newPositional ++ oldOther