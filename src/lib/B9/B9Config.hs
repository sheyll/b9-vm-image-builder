{-# Language DeriveDataTypeable #-}
module B9.B9Config ( B9Config(..)
                   , defaultB9ConfigFile
                   , defaultB9Config
                   , writeInitialB9Config
                   , readB9Config
                   , parseB9Config
                   , LogLevel(..)
                   , LogConfig(..)
                   , ExecEnvType (..)
                   ) where

import Data.Monoid
import Data.Typeable
import Control.Monad
import Control.Exception
import Control.Applicative
import Data.Function (on)
import Control.Monad.IO.Class
import System.Directory

import B9.ConfigUtils

data ExecEnvType = LibVirtLXC deriving (Eq, Show, Ord, Read)

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord, Read)

data LogConfig = ToStdOut LogLevel
               | ToStdErr LogLevel
               | ToFile FilePath LogLevel
               | LogTo [LogConfig]
               deriving (Eq, Show, Read)

data B9Config = B9Config { logConfig :: LogConfig
                         , buildDirRoot :: (Maybe FilePath)
                         , keepTempDirs :: Bool
                         , execEnvType :: ExecEnvType
                         , profileFile :: Maybe FilePath
                         , envVars :: [(String, String)]
                         , uniqueBuildDirs :: Bool
                         }

defaultB9Config = B9Config { logConfig = LogTo [ ToStdOut LogTrace
                                               , ToFile "b9-build.log" LogTrace ]
                           , buildDirRoot = Nothing
                           , keepTempDirs = False
                           , execEnvType = LibVirtLXC
                           , profileFile = Just "b9-build.profile"
                           , envVars = []
                           , uniqueBuildDirs = True
                           }

defaultB9ConfigFile = InB9UserDir "b9.conf"

logConfigK = "log_config"
buildDirRootK = "build_dir_root"
keepTempDirsK = "keep_temp_dirs"
execEnvTypeK = "exec_env"
profileFileK = "profile_file"
envVarsK = "environment_vars"
uniqueBuildDirsK = "unique_build_dirs"

cfgFileSection = "global"

writeInitialB9Config :: MonadIO m => (Maybe SystemPath) -> ConfigParser -> m ()
writeInitialB9Config Nothing cpNonGlobal = writeInitialB9Config
                                           (Just defaultB9ConfigFile)
                                           cpNonGlobal
writeInitialB9Config (Just cfgPath) cpNonGlobal = do
  cfgFile <- resolve cfgPath
  ensureDir cfgFile
  exists <- liftIO $ doesFileExist cfgFile
  when (not exists) $
    let res = do
          let cp = emptyCP
              c = defaultB9Config
          cp <- add_section cp cfgFileSection
          cp <- setshow cp cfgFileSection logConfigK $ logConfig c
          cp <- setshow cp cfgFileSection keepTempDirsK $ keepTempDirs c
          cp <- setshow cp cfgFileSection buildDirRootK $ buildDirRoot c
          cp <- setshow cp cfgFileSection execEnvTypeK $ execEnvType c
          cp <- setshow cp cfgFileSection profileFileK $ profileFile c
          cp <- setshow cp cfgFileSection envVarsK $ envVars c
          cp <- setshow cp cfgFileSection uniqueBuildDirsK $ uniqueBuildDirs c
          return $ merge cp cpNonGlobal
    in case res of
     Left e ->
       liftIO $ throwIO (IniFileException cfgFile e)
     Right cp ->
       liftIO $ writeFile cfgFile $ to_string cp

readB9Config :: MonadIO m => (Maybe SystemPath) -> m ConfigParser
readB9Config Nothing = readB9Config (Just defaultB9ConfigFile)
readB9Config (Just cfgFile) = readIniFile cfgFile

parseB9Config :: ConfigParser -> B9Config
parseB9Config cp =
  let geto :: (Get_C a, Read a) => OptionSpec -> a -> a
      geto = getOptionOr cp cfgFileSection
      c = defaultB9Config
  in B9Config {
    logConfig = geto logConfigK $ logConfig c
    , buildDirRoot = geto buildDirRootK $ buildDirRoot c
    , execEnvType = geto execEnvTypeK $ execEnvType c
    , profileFile = geto profileFileK $ profileFile c
    , envVars = getOption cp cfgFileSection envVarsK <> envVars c
    , uniqueBuildDirs = geto uniqueBuildDirsK $ uniqueBuildDirs c
    , keepTempDirs = geto keepTempDirsK $ keepTempDirs c
    }
