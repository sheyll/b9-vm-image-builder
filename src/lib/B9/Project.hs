module B9.Project ( Project(..)
                  , emptyProject
                  ) where

import Data.Data
import Data.Monoid
import B9.ShellScript ( Script(..))
import B9.ExecEnv
import B9.DiskImages
import B9.ArtifactGenerator

data Project = Project { projectName :: String
                       , projectDisks :: [ImageTarget]
                       , projectArtifacts :: ArtifactGenerator
                       , projectSharedDirectories :: [SharedDirectory]
                       , projectBuildScript :: Script
                       , projectCpuArch :: CPUArch
                       } deriving (Read, Show, Typeable, Data)

instance Monoid Project where
  mempty = Project mempty mempty mempty mempty mempty mempty
  mappend (Project n d c s b r) (Project n' d' c' s' b' r') =
    Project (n <> n') (d <> d') (c <> c') (s <> s') (b <> b') (r <> r')


emptyProject :: Project
emptyProject = mempty
