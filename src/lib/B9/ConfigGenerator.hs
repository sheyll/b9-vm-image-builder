module B9.ConfigGenerator
  (ConfigGenerator(..)
  ,TemplateFiles(..)
  ,InstanceId(..)
  ,ConfigTarget(..)
  ,CloudInitType(..)
  ,ConfigAssembly(..)
  ,AssembledConfig(..)
  ,instanceIdKey
  ,uniqueInstanceIdKey
  ,sharedDirectoriesFromGeneratedConfig
  ) where


import Data.Data
import Data.Monoid
import Control.Monad
import Data.Maybe

import B9.DiskImages
import B9.ConfigUtils
import B9.ExecEnv

  -- | A single config generator specifies howto generate multiple output
-- files/directories. It consists of a netsted set of variable bindings that are replaced inside the text files
data ConfigGenerator =
  FromDirectory SystemPath TemplateFiles [ConfigGenerator]
  | Let [(String, String)] [ConfigGenerator]
  | ConfigInstance InstanceId  [ConfigAssembly]
  deriving (Read, Show, Typeable, Data)

instance Monoid ConfigGenerator where
  mempty = Let [] []
  (Let [] []) `mappend` x = x
  x `mappend` (Let [] []) = x
  x `mappend` y = Let [] [x, y]

newtype TemplateFiles = TemplateFiles [FilePath]
  deriving (Read, Show, Typeable, Data)

newtype InstanceId = IID String
  deriving (Read, Show, Typeable, Data, Eq)

instanceIdKey :: String
instanceIdKey = "instance_id"

uniqueInstanceIdKey :: String
uniqueInstanceIdKey = "unique_instance_id"

data ConfigTarget = CloudInitTarget CloudInitType FilePath
                  | ConfigMount FilePath MountPoint
  deriving (Read, Show, Typeable, Data, Eq)

data ConfigAssembly = CloudInit CloudInitType FilePath
                    | MountDuringBuild FilePath
  deriving (Read, Show, Typeable, Data, Eq)

data AssembledConfig = AssembledConfig InstanceId [ConfigTarget]
  deriving (Read, Show, Typeable, Data, Eq)

data CloudInitType = CI_ISO | CI_VFAT
  deriving (Read, Show, Typeable, Data, Eq)


-- | Generate 'SharedDirectory's from generated configuration by filtering all
-- 'MountDuringBuild' definitions.
sharedDirectoriesFromGeneratedConfig :: [AssembledConfig] -> [SharedDirectory]
sharedDirectoriesFromGeneratedConfig = catMaybes . map toSharedDir
                                       . join . map getTarget
  where
    toSharedDir (ConfigMount hostDir guestMnt) = Just (SharedDirectoryRO hostDir guestMnt)
    toSharedDir _ = Nothing

    getTarget (AssembledConfig _iid targets) = targets
