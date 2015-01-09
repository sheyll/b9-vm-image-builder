module B9.ExecEnv
       ( ExecEnv (..)
       , Resources (..)
       , noResources
       , SharedDirectory (..)
       , CPUArch (..)
       , RamSize (..)
       ) where

import Data.Monoid
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

instance Monoid Resources where
  mempty = Resources mempty 1 mempty
  mappend (Resources m c a) (Resources m' c' a') =
    Resources (m <> m') (max c c') (a <> a')

noResources :: Resources
noResources = mempty

data CPUArch = X86_64 | I386 deriving (Read, Show)

instance Monoid CPUArch where
  mempty = I386
  I386 `mappend` x = x
  X86_64 `mappend` _ = X86_64

data RamSize = RamSize Int SizeUnit
             | AutomaticRamSize deriving (Eq, Read, Show, Ord)

instance Monoid RamSize where
  mempty = AutomaticRamSize
  AutomaticRamSize `mappend` x = x
  x `mappend` AutomaticRamSize = x
  r `mappend` r' = max r r'
