{-# LANGUAGE DeriveDataTypeable #-}
{-| Data types describing the execution environment
    of virtual machine builds.
    'ExecEnv', 'Resources' and 'SharedDirectory' describe how
    "B9.LibVirtLXC" should configure and execute
    build scripts, as defined in "B9.ShellScript" and "B9.Vm".
    -}
module B9.ExecEnv (
    ExecEnv(..),
    Resources(..),
    noResources,
    SharedDirectory(..),
    CPUArch(..),
    RamSize(..),
    ) where

import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Monoid

import B9.DiskImages
import GHC.Generics (Generic)

data ExecEnv = ExecEnv
    { envName :: String
    , envImageMounts :: [Mounted Image]
    , envSharedDirectories :: [SharedDirectory]
    , envResources :: Resources
    }
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable ExecEnv
instance Binary ExecEnv
instance NFData ExecEnv

data SharedDirectory
    = SharedDirectory FilePath
                      MountPoint
    | SharedDirectoryRO FilePath
                        MountPoint
    | SharedSources MountPoint
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable SharedDirectory
instance Binary SharedDirectory
instance NFData SharedDirectory

data Resources = Resources
    { maxMemory :: RamSize
    , cpuCount :: Int
    , cpuArch :: CPUArch
    } deriving (Eq,Read,Show,Typeable,Data,Generic)

instance Hashable Resources
instance Binary Resources
instance NFData Resources

instance Monoid Resources where
    mempty = Resources mempty 1 mempty
    mappend (Resources m c a) (Resources m' c' a') =
        Resources (m <> m') (max c c') (a <> a')

noResources :: Resources
noResources = mempty

data CPUArch
    = X86_64
    | I386
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable CPUArch
instance Binary CPUArch
instance NFData CPUArch

instance Monoid CPUArch where
    mempty = I386
    I386 `mappend` x = x
    X86_64 `mappend` _ = X86_64

data RamSize
    = RamSize Int
              SizeUnit
    | AutomaticRamSize
    deriving (Eq,Read,Show,Ord,Typeable,Data,Generic)

instance Hashable RamSize
instance Binary RamSize
instance NFData RamSize

instance Monoid RamSize where
    mempty = AutomaticRamSize
    AutomaticRamSize `mappend` x = x
    x `mappend` AutomaticRamSize = x
    r `mappend` r' = max r r'
