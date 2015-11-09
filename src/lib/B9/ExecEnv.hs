{-# LANGUAGE DeriveDataTypeable #-}
{-| Data types describing the execution environment
    of virtual machine builds.
    'ExecEnv', 'Resources' and 'SharedDirectory' describe how
    "B9.LibVirtLXC" should configure and execute
    build scripts, as defined in "B9.ShellScript" and "B9.Vm".
    -}
module B9.ExecEnv where

import B9.DiskImages
import B9.QCUtil
import Control.Lens
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Default
import Data.Hashable
import Data.Monoid
import GHC.Generics (Generic)
import Test.QuickCheck

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

instance Default Resources where
  def = Resources AutomaticRamSize 2 X86_64

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

data ExecEnvType =
    LibVirtLXC
    deriving (Eq,Show,Ord,Read,Generic,Data,Typeable)
instance Hashable ExecEnvType
instance Binary ExecEnvType
instance NFData ExecEnvType

-- | Decribe how a linux container is supposed to be started.
data ExecEnvSpec = ExecEnvSpec
    { _execEnvTitle :: String
    , _execEnvHypervisor :: ExecEnvType
    , _execEnvLimits :: Resources
    } deriving (Read,Show,Generic,Eq,Data,Typeable)

instance Hashable ExecEnvSpec
instance Binary ExecEnvSpec
instance NFData ExecEnvSpec
instance Default ExecEnvSpec where
  def = ExecEnvSpec "exec-env" LibVirtLXC def

makeLenses ''ExecEnvSpec

-- | Instruct an environment to mount a host directory
data HostDirMnt
    = AddMountHostDirRW FilePath
    | AddMountHostDirRO FilePath
    deriving (Read,Show,Eq,Generic,Data,Typeable)

instance Hashable HostDirMnt
instance Binary HostDirMnt
instance NFData HostDirMnt

instance Arbitrary ExecEnvSpec where
    arbitrary =
        ExecEnvSpec <$> smaller arbitraryNiceString <*> pure LibVirtLXC <*>
        smaller arbitrary

instance Arbitrary Resources where
    arbitrary =
        Resources <$> smaller arbitrary <*> smaller arbitrary <*>
        smaller arbitrary

instance Arbitrary RamSize where
    arbitrary = RamSize <$> smaller arbitrary <*> smaller arbitrary

instance Arbitrary CPUArch where
    arbitrary = Test.QuickCheck.elements [X86_64, I386]

instance Arbitrary SharedDirectory where
    arbitrary =
        oneof
            [ SharedDirectory <$> smaller arbitraryFilePath <*>
              (MountPoint <$> smaller arbitraryFilePath)
            , SharedDirectoryRO <$> smaller arbitraryFilePath <*>
              (MountPoint <$> smaller arbitraryFilePath)]
