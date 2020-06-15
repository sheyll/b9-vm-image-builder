module B9.B9Config.Docker
  ( dockerConfigToCPDocument,
    defaultDockerConfig,
    parseDockerConfig,
    DockerConfig (..),
    dockerNetworkId,
    dockerCapabilities,
  )
where

import B9.B9Config.Container
import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras

data DockerConfig
  = DockerConfig
      { _dockerNetworkId :: Maybe String,
        _dockerCapabilities :: [ContainerCapability]
      }
  deriving (Read, Show, Eq)

makeLenses ''DockerConfig

defaultDockerConfig :: DockerConfig
defaultDockerConfig =
  DockerConfig
    Nothing
    [ CAP_MKNOD,
      CAP_SYS_ADMIN,
      CAP_SYS_CHROOT,
      CAP_SETGID,
      CAP_SETUID,
      CAP_NET_BIND_SERVICE,
      CAP_SETPCAP,
      CAP_SYS_PTRACE,
      CAP_SYS_MODULE
    ]

cfgFileSection :: String
cfgFileSection = "docker"

networkIdK :: String
networkIdK = "network"

dockerConfigToCPDocument ::
  DockerConfig -> CPDocument -> Either CPError CPDocument
dockerConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <-
    setShowCP cp1 cfgFileSection networkIdK $
      _dockerNetworkId c
  containerCapsToCPDocument cp2 cfgFileSection $
    _dockerCapabilities c

parseDockerConfig :: CPDocument -> Either CPError DockerConfig
parseDockerConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in DockerConfig
        <$> getr networkIdK
        <*> parseContainerCapabilities cp cfgFileSection
