module B9.ExecEnv
       ( ExecEnv (..)
       , Resources (..)
       , noResources
       , SharedDirectory (..)
       , CPUArch (..)
       , RamSize (..)
       ) where

import B9.ShellScript
import B9.DiskImages

data ExecEnv = ExecEnv { envName :: String
                       , envImageMounts :: [Mounted Image]
                       , envSharedDirectories :: [SharedDirectory]
                       , envResources :: Resources
                       }

data SharedDirectory = SharedDirectory FilePath MountPoint
                     deriving (Read, Show)

data Resources = Resources { maxMemory :: RamSize
                           , cpuCount :: Int
                           , cpuArch :: CPUArch
                           } deriving (Read, Show)
noResources :: Resources
noResources = Resources (RamSize 0 MB) 0 I386

data CPUArch = X86_64 | I386  deriving (Read, Show)

data RamSize = RamSize Int SizeUnit  deriving (Read, Show)
