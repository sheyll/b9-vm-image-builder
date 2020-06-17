module B9.B9Config.Podman
  ( podmanConfigToCPDocument,
    defaultPodmanConfig,
    parsePodmanConfig,
    PodmanConfig (..),
    podmanNetworkId,
    podmanCapabilities,
  )
where

import B9.B9Config.Container
import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras

data PodmanConfig
  = PodmanConfig
      { _podmanNetworkId :: Maybe String,
        _podmanCapabilities :: [ContainerCapability]
      }
  deriving (Read, Show, Eq)

makeLenses ''PodmanConfig

defaultPodmanConfig :: PodmanConfig
defaultPodmanConfig =
  PodmanConfig
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
cfgFileSection = "podman"

networkIdK :: String
networkIdK = "network"

podmanConfigToCPDocument ::
  PodmanConfig -> CPDocument -> Either CPError CPDocument
podmanConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <-
    setShowCP cp1 cfgFileSection networkIdK $
      _podmanNetworkId c
  containerCapsToCPDocument cp2 cfgFileSection $
    _podmanCapabilities c

parsePodmanConfig :: CPDocument -> Either CPError PodmanConfig
parsePodmanConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in PodmanConfig
        <$> getr networkIdK
        <*> parseContainerCapabilities cp cfgFileSection
