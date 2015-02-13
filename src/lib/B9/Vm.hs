module B9.Vm (VmScript (..)
             ,substVmScript) where

import Data.Data
import Data.Generics.Schemes
import Data.Generics.Aliases

import B9.ShellScript
import B9.DiskImages
import B9.ExecEnv
import B9.Content.StringTemplate

-- | Describe a virtual machine, i.e. a set up disk images to create and a shell
-- script to put things together.
data VmScript = VmScript CPUArch [SharedDirectory] Script | NoVmScript
  deriving (Read, Show, Typeable, Data, Eq)

substVmScript :: [(String,String)] -> VmScript -> VmScript
substVmScript env p = everywhere gsubst p
  where gsubst :: forall a. Data a => a -> a
        gsubst = mkT substMountPoint
                   `extT` substSharedDir
                     `extT` substScript

        substMountPoint NotMounted = NotMounted
        substMountPoint (MountPoint x) = MountPoint (sub x)

        substSharedDir (SharedDirectory fp mp) =
          SharedDirectory (sub fp) mp
        substSharedDir (SharedDirectoryRO fp mp) =
          SharedDirectoryRO (sub fp) mp
        substSharedDir s = s

        substScript (In fp s) = In (sub fp) s
        substScript (Run fp args) = Run (sub fp) (map sub args)
        substScript (As fp s) = As (sub fp) s
        substScript s = s

        sub = subst env
