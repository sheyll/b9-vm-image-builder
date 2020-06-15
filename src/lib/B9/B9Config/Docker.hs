module B9.B9Config.Docker
  ( dockerConfigToCPDocument,
    defaultDockerConfig,
    parseDockerConfig,
    DockerConfig (..),
    networkId,
    DockerGuestCapability (..),
  )
where

import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras

data DockerConfig
  = DockerConfig
      { _networkId :: Maybe String,
        guestCapabilities :: [DockerGuestCapability]
      }
  deriving (Read, Show, Eq)

-- | Available capabilities for `docker run` containers. This maps directly to the
-- capabilities defined in 'man 7 capabilities'.
data DockerGuestCapability
  = CAP_MKNOD
  | CAP_AUDIT_CONTROL
  | CAP_AUDIT_READ
  | CAP_AUDIT_WRITE
  | CAP_BLOCK_SUSPEND
  | CAP_CHOWN
  | CAP_DAC_OVERRIDE
  | CAP_DAC_READ_SEARCH
  | CAP_FOWNER
  | CAP_FSETID
  | CAP_IPC_LOCK
  | CAP_IPC_OWNER
  | CAP_KILL
  | CAP_LEASE
  | CAP_LINUX_IMMUTABLE
  | CAP_MAC_ADMIN
  | CAP_MAC_OVERRIDE
  | CAP_NET_ADMIN
  | CAP_NET_BIND_SERVICE
  | CAP_NET_BROADCAST
  | CAP_NET_RAW
  | CAP_SETGID
  | CAP_SETFCAP
  | CAP_SETPCAP
  | CAP_SETUID
  | CAP_SYS_ADMIN
  | CAP_SYS_BOOT
  | CAP_SYS_CHROOT
  | CAP_SYS_MODULE
  | CAP_SYS_NICE
  | CAP_SYS_PACCT
  | CAP_SYS_PTRACE
  | CAP_SYS_RAWIO
  | CAP_SYS_RESOURCE
  | CAP_SYS_TIME
  | CAP_SYS_TTY_CONFIG
  | CAP_SYSLOG
  | CAP_WAKE_ALARM
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

guestCapabilitiesK :: String
guestCapabilitiesK = "guest_capabilities"

dockerConfigToCPDocument ::
  DockerConfig -> CPDocument -> Either CPError CPDocument
dockerConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <- setShowCP cp1 cfgFileSection networkIdK $ _networkId c
  setShowCP cp2 cfgFileSection guestCapabilitiesK $ guestCapabilities c

parseDockerConfig :: CPDocument -> Either CPError DockerConfig
parseDockerConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in DockerConfig
        <$> getr networkIdK
        <*> getr guestCapabilitiesK

