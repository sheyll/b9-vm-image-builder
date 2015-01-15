module B9.Project ( Project(..)
                  , emptyProject
                  , DiskTarget(..)
                  ) where

import Data.Data
import Data.Monoid
import B9.ShellScript ( Script(..))
import B9.ExecEnv
import B9.DiskImages

data Project = Project { projectName :: String
                       , projectDisks :: [Mounted DiskTarget]
                       , projectSharedDirectories :: [SharedDirectory]
                       , projectBuildScript :: Script
                       , projectResources :: Resources
                       } deriving (Read, Show, Typeable, Data)

instance Monoid Project where
  mempty = Project mempty mempty mempty mempty mempty
  mappend (Project n d s b r) (Project n' d' s' b' r') =
    Project (n <> n') (d <> d') (s <> s') (b <> b') (r <> r')

emptyProject :: Project
emptyProject = mempty

data DiskTarget = Export Image ImageSource
                | Transient ImageSource
                | Share ImageInfo ImageSource
                deriving (Read, Show, Typeable, Data)
