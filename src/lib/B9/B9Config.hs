{-# Language DeriveDataTypeable #-}
module B9.B9Config ( B9Config(..)
                   , defaultB9ConfigFile
                   , defaultB9Config
                   , writeInitialB9Config
                   , readB9Config
                   , parseB9Config
                   , LogLevel(..)
                   , ExecEnvType (..)
                   ) where

import Data.Monoid
import Control.Monad
import Control.Exception
import Data.Function (on)
import Control.Monad.IO.Class
import System.Directory

import B9.ConfigUtils
import B9.Repository

data ExecEnvType = LibVirtLXC deriving (Eq, Show, Ord, Read)

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord, Read)

data B9Config = B9Config { verbosity :: Maybe LogLevel
                         , logFile :: Maybe FilePath
                         , buildDirRoot :: Maybe FilePath
                         , keepTempDirs :: Bool
                         , execEnvType :: ExecEnvType
                         , profileFile :: Maybe FilePath
                         , envVars :: [(String, String)]
                         , uniqueBuildDirs :: Bool
                         , repositoryCache :: Maybe String
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
             , repositoryCache = getLast ((mappend `on` (Last . repositoryCache)) c c')
             , repository = getLast ((mappend `on` (Last . repository)) c c')
             }

defaultB9Config = B9Config { verbosity = Nothing
                           , logFile = Nothing
                           , buildDirRoot = Nothing
                           , keepTempDirs = False
                           , execEnvType = LibVirtLXC
                           , profileFile = Nothing
                           , envVars = []
                           , uniqueBuildDirs = True
                           , repository = Nothing
                           , repositoryCache = Just defaultRepositoryCacheId
                           }

defaultRepositoryCacheId = "cache"

defaultRepositoryCache =
  Repository defaultRepositoryCacheId
             (LocalRepo (InB9UserDir defaultRepositoryCacheId))

defaultB9ConfigFile = InB9UserDir "b9.conf"

verbosityK = "verbosity"
logFileK = "log_file"
buildDirRootK = "build_dir_root"
keepTempDirsK = "keep_temp_dirs"
execEnvTypeK = "exec_env"
profileFileK = "profile_file"
envVarsK = "environment_vars"
uniqueBuildDirsK = "unique_build_dirs"
repositoryCacheK = "repository_cache"
repositoryK = "repository"

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
          cp <- writeRepositoryToB9Config defaultRepositoryCache cp
          cp <- add_section cp cfgFileSection
          cp <- setshow cp cfgFileSection verbosityK $ verbosity c
          cp <- setshow cp cfgFileSection logFileK $ logFile c
          cp <- setshow cp cfgFileSection buildDirRootK $ buildDirRoot c
          cp <- setshow cp cfgFileSection keepTempDirsK $ keepTempDirs c
          cp <- setshow cp cfgFileSection execEnvTypeK $ execEnvType c
          cp <- setshow cp cfgFileSection profileFileK $ profileFile c
          cp <- setshow cp cfgFileSection envVarsK $ envVars c
          cp <- setshow cp cfgFileSection uniqueBuildDirsK $ uniqueBuildDirs c
          cp <- setshow cp cfgFileSection repositoryCacheK $ repositoryCache c
          return $ merge cp cpNonGlobal
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
    verbosity = geto verbosityK $ verbosity c
    , logFile = geto logFileK $ logFile c
    , buildDirRoot = geto buildDirRootK $ buildDirRoot c
    , keepTempDirs = geto keepTempDirsK $ keepTempDirs c
    , execEnvType = geto execEnvTypeK $ execEnvType c
    , profileFile = geto profileFileK $ profileFile c
    , envVars = getOption cp cfgFileSection envVarsK <> envVars c
    , uniqueBuildDirs = geto uniqueBuildDirsK $ uniqueBuildDirs c
    , repositoryCache =    getOption cp cfgFileSection repositoryCacheK
                        <> repositoryCache c
    , repository = getOption cp cfgFileSection repositoryK <> repository c
    }
