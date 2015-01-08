module B9.B9Config ( B9Config(..)
                   , defaultB9Config
                   , LogLevel(..)
                   , LogConfig(..)
                   , ExecEnvType (..)
                   ) where

data B9Config = B9Config { logConfig :: LogConfig
                         , buildDirRoot :: FilePath
                         , keepTempDirs :: Bool
                         , execEnvType :: ExecEnvType
                         , profileFile :: Maybe FilePath
                         , envVars :: [(String, String)]
                         , uniqueBuildDirs :: Bool
                         , useSudo :: Bool
                         }

defaultB9Config = B9Config { logConfig = LogTo [ ToStdOut LogDebug
                                               , ToFile "b9-build.log" LogTrace
                                               ]
                             , buildDirRoot = "."
                             , keepTempDirs = False
                             , execEnvType = LibVirtLXC
                             , profileFile = Just "b9-build.profile"
                             , envVars = []
                             , uniqueBuildDirs = True
                             , useSudo = True
                             }

data ExecEnvType = LibVirtLXC

data LogLevel = LogTrace | LogDebug | LogInfo | LogError | LogNothing
              deriving (Eq, Show, Ord)

data LogConfig = ToStdOut LogLevel
               | ToStdErr LogLevel
               | ToFile FilePath LogLevel
               | LogTo [LogConfig]
               deriving (Eq, Show)
