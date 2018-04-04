{-|
Static B9 configuration. Read, write and merge configurable properties.
The properties are independent of specific build targets.
-}
module B9.B9Config ( B9Config(..)
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
                   , B9ConfigOverride(..)
                   , customB9Config
                   , customB9ConfigPath
                   , defaultB9ConfigFile
                   , defaultRepositoryCache
                   , defaultB9Config
                   , openOrCreateB9Config
                   , writeB9ConfigParser
                   , readB9Config
                   , parseB9Config
                   , appendPositionalArguments
                   , modifyConfigParser
                   , b9ConfigToConfigParser
                   , LogLevel(..)
                   , ExecEnvType (..)
                   , BuildVariables
                   , module X
                   ) where

import Data.Maybe (fromMaybe)
import Control.Exception
import Data.Function (on)
import Control.Monad.IO.Class
import System.Directory
import qualified Data.Semigroup as Sem
import Data.Monoid
import Data.List (partition, sortBy)
import Data.ConfigFile.B9Extras
import B9.B9Config.LibVirtLXC as X
import B9.B9Config.Repository as X
import Control.Lens.TH
import Control.Monad ((>=>))

type BuildVariables = [(String,String)]

data ExecEnvType = LibVirtLXC deriving (Eq, Show, Ord, Read)

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord, Read)

data B9Config = B9Config { _verbosity :: Maybe LogLevel
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
                         , _libVirtLXCConfigs :: Maybe LibVirtLXCConfig
                         , _remoteRepos :: [RemoteRepo]
                         } deriving (Show)

instance Sem.Semigroup B9Config where
  c <> c' =
      B9Config { _verbosity = getLast $ on mappend (Last . _verbosity) c c'
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
      , _libVirtLXCConfigs = getLast ((mappend `on` (Last . _libVirtLXCConfigs)) c c')
      , _remoteRepos = (mappend `on` _remoteRepos) c c'
      }

instance Monoid B9Config where
  mappend = (Sem.<>)
  mempty = B9Config Nothing Nothing Nothing False LibVirtLXC Nothing [] True
                    Nothing Nothing False Nothing []

-- | Override b9 configuration items and/or the path of the b9 configuration file.
-- This is useful, i.e. when dealing with command line parameters.
data B9ConfigOverride = B9ConfigOverride
  { _customB9ConfigPath :: Maybe SystemPath
  , _customB9Config     :: B9Config
  }

makeLenses ''B9Config
makeLenses ''B9ConfigOverride

-- | Open the configuration file that contains the 'B9Config'.
-- If the configuration does not exist, write a default configuration file,
-- and create a all missing directories.
openOrCreateB9Config :: MonadIO m => Maybe SystemPath -> m ConfigParser
openOrCreateB9Config cfgPath = do
  cfgFile <- resolve (fromMaybe defaultB9ConfigFile cfgPath)
  ensureDir cfgFile
  liftIO $ do
    exists <- doesFileExist cfgFile
    if exists
      then readIniFile (Path cfgFile)
      else
        let res = b9ConfigToConfigParser defaultB9Config emptyCP
        in  case res of
              Left  e  -> throwIO (IniFileException cfgFile e)
              Right cp -> writeFile cfgFile (to_string cp) >> return cp

-- | Write the configuration in the 'ConfigParser' to either the user supplied
-- configuration file path or to 'defaultB9ConfigFile'.
-- Create all missing (parent) directories.
writeB9ConfigParser :: MonadIO m => Maybe SystemPath -> ConfigParser -> m ()
writeB9ConfigParser cfgFileIn cp = do
  cfgFile <- resolve (fromMaybe defaultB9ConfigFile cfgFileIn)
  ensureDir cfgFile
  liftIO (writeFile cfgFile (to_string cp))


defaultB9Config :: B9Config
defaultB9Config = B9Config
  { _verbosity         = Just LogInfo
  , _logFile           = Nothing
  , _buildDirRoot      = Nothing
  , _keepTempDirs      = False
  , _execEnvType       = LibVirtLXC
  , _profileFile       = Nothing
  , _envVars           = []
  , _uniqueBuildDirs   = True
  , _repository        = Nothing
  , _repositoryCache   = Just defaultRepositoryCache
  , _interactive       = False
  , _libVirtLXCConfigs = Just defaultLibVirtLXCConfig
  , _remoteRepos       = []
  }

defaultRepositoryCache :: SystemPath
defaultRepositoryCache = InB9UserDir "repo-cache"
defaultB9ConfigFile :: SystemPath
defaultB9ConfigFile = InB9UserDir "b9.conf"
verbosityK :: String
verbosityK = "_verbosity"
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
repositoryK = "_repository"
cfgFileSection :: String
cfgFileSection = "global"

-- | Parse a 'B9Config', modify it, and merge it back to the given 'ConfigParser'.
modifyConfigParser :: (B9Config -> B9Config)  -> ConfigParser -> Either CPError ConfigParser
modifyConfigParser f cp =
  do cfg <- parseB9Config cp
     cp2 <- b9ConfigToConfigParser (f cfg) emptyCP
     return (merge cp cp2)

-- | Append a config file section for the 'B9Config' to a 'ConfigParser'.
b9ConfigToConfigParser :: B9Config -> ConfigParser -> Either CPError ConfigParser
b9ConfigToConfigParser c cp = do
  cp1 <- add_section cp cfgFileSection
  cp2 <- setshow cp1 cfgFileSection verbosityK (_verbosity c)
  cp3 <- setshow cp2 cfgFileSection logFileK (_logFile c)
  cp4 <- setshow cp3 cfgFileSection buildDirRootK (_buildDirRoot c)
  cp5 <- setshow cp4 cfgFileSection keepTempDirsK (_keepTempDirs c)
  cp6 <- setshow cp5 cfgFileSection execEnvTypeK (_execEnvType c)
  cp7 <- setshow cp6 cfgFileSection profileFileK (_profileFile c)
  cp8 <- setshow cp7 cfgFileSection envVarsK (_envVars c)
  cp9 <- setshow cp8 cfgFileSection uniqueBuildDirsK (_uniqueBuildDirs c)
  cpA <- setshow cp9 cfgFileSection repositoryCacheK (_repositoryCache c)
  cpB <-
    (foldr (>=>) return
      (libVirtLXCConfigToConfigParser <$> (_libVirtLXCConfigs c)))
    cpA
  cpFinal <-
    (foldr (>=>) return
      (remoteRepoToConfigParser <$> _remoteRepos c))
    cpB
  setshow cpFinal cfgFileSection repositoryK (_repository c)

readB9Config :: MonadIO m => Maybe SystemPath -> m ConfigParser
readB9Config cfgFile = readIniFile (fromMaybe defaultB9ConfigFile cfgFile)

parseB9Config :: ConfigParser -> Either CPError B9Config
parseB9Config cp =
  let getr :: (Get_C a) => OptionSpec -> Either CPError a
      getr = get cp cfgFileSection
      getB9Config =
        B9Config
          <$> getr verbosityK
          <*> getr logFileK
          <*> getr buildDirRootK
          <*> getr keepTempDirsK
          <*> getr execEnvTypeK
          <*> getr profileFileK
          <*> getr envVarsK
          <*> getr uniqueBuildDirsK
          <*> getr repositoryCacheK
          <*> getr repositoryK
          <*> pure False
          <*> Right (either (const Nothing) Just (parseLibVirtLXCConfig cp))
          <*> parseRemoteRepos cp
    in  getB9Config


-- | If environment variables @arg_1 .. arg_n@ are bound
-- and a list of @k@ additional values are passed to this function,
-- store them with keys @arg_(n+1) .. arg_(n+k)@.
appendPositionalArguments :: [String] -> B9Config -> B9Config
appendPositionalArguments extraPositional c = c
  { _envVars = appendVars (_envVars c)
  }
 where
  appendVars argsOld =
    let (oldPositional, oldOther) =
          partition (("arg_" ==) . take 4 . fst) argsOld
        oldPositionalSortedByPosition =
          map snd
            $ sortBy (compare `on` fst)
            $ map (\(x, y) -> ((read :: String -> Int) $ drop 4 $ x, y))
            $ oldPositional
        newPositional =
          let xs = oldPositionalSortedByPosition ++ extraPositional
          in  [ ("arg_" ++ show i, a) | (i, a) <- zip [1 :: Int ..] xs ]
    in  newPositional ++ oldOther