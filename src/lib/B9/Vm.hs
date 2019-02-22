{-| Definition of 'VmScript' an artifact encapsulating several virtual machines
    disk images that can be mounted in an execution environment like
    "B9.LibVirtLXC". A 'VmScript' is embedded by in an
    'B9.ArtifactGenerator.ArtifactGenerator'. -}
module B9.Vm
  ( VmScript(..)
  , substVmScript
  ) where

import           Control.Parallel.Strategies
import           Data.Binary
import           Data.Data
import           Data.Generics.Aliases       hiding (Generic)
import           Data.Generics.Schemes
import           Data.Hashable
import           GHC.Generics                (Generic)

import           B9.Content.Environment
import           B9.Content.StringTemplate
import           B9.DiskImages
import           B9.ExecEnv
import           B9.ShellScript

-- | Describe a virtual machine, i.e. a set up disk images to create and a shell
-- script to put things together.
data VmScript
  = VmScript CPUArch
             [SharedDirectory]
             Script
  | NoVmScript
  deriving (Read, Show, Typeable, Data, Eq, Generic)

instance Hashable VmScript

instance Binary VmScript

instance NFData VmScript

substVmScript :: Environment -> VmScript -> VmScript
substVmScript env = everywhere gsubst
  where
    gsubst :: Data a => a -> a
    gsubst = mkT substMountPoint `extT` substSharedDir `extT` substScript
    substMountPoint NotMounted     = NotMounted
    substMountPoint (MountPoint x) = MountPoint (sub x)
    substSharedDir (SharedDirectory fp mp)   = SharedDirectory (sub fp) mp
    substSharedDir (SharedDirectoryRO fp mp) = SharedDirectoryRO (sub fp) mp
    substSharedDir s                         = s
    substScript (In fp s)     = In (sub fp) s
    substScript (Run fp args) = Run (sub fp) (map sub args)
    substScript (As fp s)     = As (sub fp) s
    substScript s             = s
    sub = subst env
