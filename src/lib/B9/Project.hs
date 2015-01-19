module B9.Project ( Project(..)
                  , emptyProject
                  ) where

import Data.Data
import Data.Monoid
import B9.ShellScript ( Script(..))
import B9.ExecEnv
import B9.DiskImages

data Project = Project { projectName :: String
                       , projectDisks :: [ImageTarget]
                       , projectSharedDirectories :: [SharedDirectory]
                       , projectBuildScript :: Script
                       , projectCpuArch :: CPUArch
                       } deriving (Read, Show, Typeable, Data)

instance Monoid Project where
  mempty = Project mempty mempty mempty mempty mempty
  mappend (Project n d s b r) (Project n' d' s' b' r') =
    Project (n <> n') (d <> d') (s <> s') (b <> b') (r <> r')

emptyProject :: Project
emptyProject = mempty
