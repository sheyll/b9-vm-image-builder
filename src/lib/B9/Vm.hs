module B9.Vm (VmScript (..)
             ,substVmScript) where

import Data.Data
import Data.Generics.Schemes
import Data.Generics.Aliases

import B9.ShellScript
import B9.DiskImages
import B9.ExecEnv
import B9.ConfigUtils

-- | Describe a virtual machine, i.e. a set up disk images to create and a shell
-- script to put things together.
data VmScript = VmScript CPUArch [SharedDirectory] Script | NoVmScript
  deriving (Read, Show, Typeable, Data, Eq)

substVmScript :: [(String,String)] -> VmScript -> VmScript
substVmScript env p = everywhere gsubst p
  where gsubst :: forall a. Data a => a -> a
        gsubst = mkT substVmScript_
                  `extT` substMountPoint
                    `extT` substImage
                       `extT` substSharedDir
                         `extT` substScript
                           `extT` substImageSource
                             `extT` substDiskTarget

        substVmScript_ :: VmScript -> VmScript
        substVmScript_ = id

        substMountPoint NotMounted = NotMounted
        substMountPoint (MountPoint x) = MountPoint (sub x)

        substImage (Image fp t fs) = Image (sub fp) t fs

        substSharedDir (SharedDirectory fp mp) =
          SharedDirectory (sub fp) mp
        substSharedDir (SharedDirectoryRO fp mp) =
          SharedDirectoryRO (sub fp) mp
        substSharedDir s = s

        substScript (In fp s) = In (sub fp) s
        substScript (Run fp args) = Run (sub fp) (map sub args)
        substScript (As fp s) = As (sub fp) s
        substScript s = s

        substImageSource (From n s) = From (sub n) s
        substImageSource (EmptyImage l f t s) = EmptyImage (sub l) f t s
        substImageSource s = s

        substDiskTarget (Share n t s) = Share (sub n) t s
        substDiskTarget s = s

        sub = subst env
