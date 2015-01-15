{-# Language DeriveDataTypeable #-}
module B9.B9Config ( B9Config(..)
                   , defaultB9ConfigFile
                   , defaultB9Config
                   , writeInitialB9Config
                   , readB9Config
                   , parseB9Config
                   , LogLevel(..)
                   , ExecEnvType (..)
                   , BuildVariables
                   ) where

import Data.Monoid
import Control.Monad
import Control.Exception
import Data.Function (on)
import Control.Monad.IO.Class
import System.Directory

import B9.ConfigUtils
import B9.Repository

type BuildVariables = [(String,String)]

data ExecEnvType = LibVirtLXC deriving (Eq, Show, Ord, Read)

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord, Read)

data B9Config = B9Config { verbosity :: Maybe LogLevel
                         , logFile :: Maybe FilePath
                         , buildDirRoot :: Maybe FilePath
                         , keepTempDirs :: Bool
                         , execEnvType :: ExecEnvType
                         , profileFile :: Maybe FilePath
                         , envVars :: BuildVariables
                         , uniqueBuildDirs :: Bool
                         , repositoryCache :: SystemPath
                         , repository :: Maybe String
                         } deriving (Show)

instance Monoid B9Config where
  mempty = defaultB9Config
  mappend c c' =
    B9Config { verbosity = getLast $ on mappend (Last . verbosity) c c'
             , logFile = getLast $ on mappend (Last . logFile) c c'
             , buildDirRoot = getLast $ on mappend (Last . buildDirRoot) c c'
             , keepTempDirs = getAny $ on mappend (Any . keepTempDirs) c c'
             , execEnvType = LibVirtLXC
             , profileFile = getLast $ on mappend (Last . profileFile) c c'
             , envVars = on mappend envVars c c'
             , uniqueBuildDirs = getAll ((mappend `on` (All . uniqueBuildDirs)) c c')
             , repositoryCache = repositoryCache c'
             , repository = getLast ((mappend `on` (Last . repository)) c c')
             }

defaultB9Config :: B9Config
defaultB9Config = B9Config { verbosity = Nothing
                           , logFile = Nothing
                           , buildDirRoot = Nothing
                           , keepTempDirs = False
                           , execEnvType = LibVirtLXC
                           , profileFile = Nothing
                           , envVars = []
                           , uniqueBuildDirs = True
                           , repository = Nothing
                           , repositoryCache = defaultRepositoryCache
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

writeInitialB9Config :: MonadIO m
                        => (Maybe SystemPath)
                        -> B9Config
                        -> ConfigParser
                        -> m ()
writeInitialB9Config Nothing cliCfg cpNonGlobal = writeInitialB9Config
                                                  (Just defaultB9ConfigFile)
                                                  cliCfg
                                                  cpNonGlobal
writeInitialB9Config (Just cfgPath) cliCfg cpNonGlobal = do
  cfgFile <- resolve cfgPath
  ensureDir cfgFile
  exists <- liftIO $ doesFileExist cfgFile
  when (not exists) $
    let res = do
          let cp = emptyCP
              c = cliCfg
          cp1 <- add_section cp cfgFileSection
          cp2 <- setshow cp1 cfgFileSection verbosityK (verbosity c)
          cp3 <- setshow cp2 cfgFileSection logFileK (logFile c)
          cp4 <- setshow cp3 cfgFileSection buildDirRootK (buildDirRoot c)
          cp5 <- setshow cp4 cfgFileSection keepTempDirsK (keepTempDirs c)
          cp6 <- setshow cp5 cfgFileSection execEnvTypeK (execEnvType c)
          cp7 <- setshow cp6 cfgFileSection profileFileK (profileFile c)
          cp8 <- setshow cp7 cfgFileSection envVarsK (envVars c)
          cp9 <- setshow cp8 cfgFileSection uniqueBuildDirsK (uniqueBuildDirs c)
          cpA <- setshow cp9 cfgFileSection repositoryCacheK (repositoryCache c)
          return $ merge cpA cpNonGlobal
    in case res of
     Left e -> liftIO (throwIO (IniFileException cfgFile e))
     Right cp -> liftIO (writeFile cfgFile (to_string cp))

readB9Config :: MonadIO m => (Maybe SystemPath) -> m ConfigParser
readB9Config Nothing = readB9Config (Just defaultB9ConfigFile)
readB9Config (Just cfgFile) = readIniFile cfgFile

parseB9Config :: ConfigParser -> B9Config
parseB9Config cp =
  let geto :: (Get_C a, Read a) => OptionSpec -> a -> a
      geto = getOptionOr cp cfgFileSection
      c = mempty

  in B9Config {
    verbosity = geto verbosityK (verbosity c)
    , logFile = geto logFileK (logFile c)
    , buildDirRoot = geto buildDirRootK (buildDirRoot c)
    , keepTempDirs = geto keepTempDirsK (keepTempDirs c)
    , execEnvType = geto execEnvTypeK (execEnvType c)
    , profileFile = geto profileFileK (profileFile c)
    , envVars = getOption cp cfgFileSection envVarsK <> envVars c
    , uniqueBuildDirs = geto uniqueBuildDirsK (uniqueBuildDirs c)
    , repositoryCache = geto repositoryCacheK (repositoryCache c)
    , repository = getOption cp cfgFileSection repositoryK <> repository c
    }
