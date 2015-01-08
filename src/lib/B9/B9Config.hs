module B9.B9Config ( B9Config(..)
                   , defaultB9Config
                   , LogLevel(..)
                   , LogConfig(..)
                   , ExecEnvType (..)
                   ) where

import B9.ConfigUtils

data B9Config = B9Config { logConfig :: LogConfig
                         , buildDirRoot :: FilePath
                         , keepTempDirs :: Bool
                         , execEnvType :: ExecEnvType
                         , execEnvConfigFile :: Maybe SystemPath
                         , profileFile :: Maybe FilePath
                         , envVars :: [(String, String)]
                         , uniqueBuildDirs :: Bool
                         }

defaultB9Config = B9Config { logConfig = LogTo [ ToStdOut LogTrace
                                               , ToFile "b9-build.log" LogTrace
                                               ]
                           , buildDirRoot = "."
                           , keepTempDirs = False
                           , execEnvType = LibVirtLXC
                           , execEnvConfigFile = Just (InB9UserDir "libvirt_lxc.cfg")
                           , profileFile = Just "b9-build.profile"
                           , envVars = []
                           , uniqueBuildDirs = True
                           }

data ExecEnvType = LibVirtLXC

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord)

data LogConfig = ToStdOut LogLevel
               | ToStdErr LogLevel
               | ToFile FilePath LogLevel
               | LogTo [LogConfig]
               deriving (Eq, Show)
