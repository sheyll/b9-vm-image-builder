-- | Definition of 'VmScript' an artifact encapsulating several virtual machines
--    disk images that can be mounted in an execution environment like
--    "B9.LibVirtLXC". A 'VmScript' is embedded by in an
--    'B9.Artifact.Generator.ArtifactGenerator'.
module B9.Vm
  ( VmScript (..),
    substVmScript,
  )
where

import B9.Artifact.Content.StringTemplate
import B9.B9Error
import B9.DiskImages
import B9.Environment
import B9.ExecEnv
import B9.ShellScript
import Control.Eff
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Generics.Aliases hiding (Generic)
import Data.Generics.Schemes
import Data.Hashable
import GHC.Generics (Generic)

-- | Describe a virtual machine, i.e. a set up disk images to create and a shell
-- script to put things together.
data VmScript
  = VmScript
      CPUArch
      [SharedDirectory]
      Script
  | NoVmScript
  deriving (Read, Show, Typeable, Data, Eq, Generic)

instance Hashable VmScript

instance Binary VmScript

instance NFData VmScript

substVmScript ::
  forall e.
  (Member EnvironmentReader e, Member ExcB9 e) =>
  VmScript ->
  Eff e VmScript
substVmScript = everywhereM gsubst
  where
    gsubst :: GenericM (Eff e)
    gsubst = mkM substMountPoint `extM` substSharedDir `extM` substScript
    substMountPoint NotMounted = pure NotMounted
    substMountPoint (MountPoint x) = MountPoint <$> substStr x
    substSharedDir (SharedDirectory fp mp) =
      SharedDirectory <$> substStr fp <*> pure mp
    substSharedDir (SharedDirectoryRO fp mp) =
      SharedDirectoryRO <$> substStr fp <*> pure mp
    substSharedDir s = pure s
    substScript (In fp s) = In <$> substStr fp <*> pure s
    substScript (Run fp args) = Run <$> substStr fp <*> mapM substStr args
    substScript (As fp s) = As <$> substStr fp <*> pure s
    substScript s = pure s
