module B9.Project ( Project(..)
                  , emptyProject
                  , DiskTarget(..)
                  ) where

import B9.ShellScript ( Script(..) )
import B9.ExecEnv
import B9.DiskImages

data Project = Project { projectName :: String
                       , projectDisks :: [Mounted DiskTarget]
                       , projectSharedDirectories :: [SharedDirectory]
                       , projectBuildScript :: Script
                       , projectResources :: Resources
                       } deriving (Read, Show)

emptyProject :: Project
emptyProject = Project [] [] [] (Begin []) noResources

data DiskTarget = Export Image ImageSource
                | Transient ImageSource
                deriving (Read, Show)
