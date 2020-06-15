module B9.B9Config.Container
  ( parseContainerConfig,
    ContainerConfig (..),
    ContainerCapability(..),
  )
where

import Data.ConfigFile.B9Extras

-- | Available capabilities for Linux containers. This maps directly to the
-- capabilities defined in 'man 7 capabilities'.
data ContainerCapability
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

containerCapabilitiesK :: String
containerCapabilitiesK = "container_capibilities"


containerCapsToCPDocument ::
  ContainerCapability -> CPDocument -> Either CPError CPDocument
containerCapsToCPDocument c cp = do
  setShowCP cp2 cfgFileSection containerCapabilitiesK $ containerCapabilities c

parseContainerCapabilities :: CPDocument -> Either CPError DockerConfig
parseContainerCapabilities =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in DockerConfig
        <$> getr networkIdK
        <*> getr containerCapabilitiesK

