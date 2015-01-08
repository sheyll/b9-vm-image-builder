module B9.ExecEnv
       ( ExecEnv (..)
       , Resources (..)
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

data Resources = Resources { maxMemory :: RamSize
                           , cpuCount :: Int
                           , cpuArch :: CPUArch
                           }

data CPUArch = X86_64 | I386

data RamSize = RamSize Int SizeUnit deriving Show
