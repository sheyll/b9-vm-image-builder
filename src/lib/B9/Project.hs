module B9.Project ( Project(..)
                  , DiskTarget(..)
                  ) where

import B9.ShellScript ( Script )
import B9.ExecEnv
import B9.DiskImages

data Project = Project { projectName :: String
                       , projectDisks :: [Mounted DiskTarget]
                       , projectSharedDirectories :: [SharedDirectory]
                       , projectBuildScript :: Script
                       , projectResources :: Resources
                       }

data DiskTarget = Export Image ImageSource
                | Transient ImageSource
