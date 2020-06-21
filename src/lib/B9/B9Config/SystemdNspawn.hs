module B9.B9Config.SystemdNspawn
  ( systemdNspawnConfigToCPDocument,
    defaultSystemdNspawnConfig,
    parseSystemdNspawnConfig,
    SystemdNspawnConfig (..),
    systemdNspawnCapabilities,
  )
where

import B9.B9Config.Container
import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras

data SystemdNspawnConfig
  = SystemdNspawnConfig
      { _systemdNspawnCapabilities :: [ContainerCapability]
      }
  deriving (Read, Show, Eq)

makeLenses ''SystemdNspawnConfig

defaultSystemdNspawnConfig :: SystemdNspawnConfig
defaultSystemdNspawnConfig =
  SystemdNspawnConfig
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
cfgFileSection = "systemdNspawn"

systemdNspawnConfigToCPDocument ::
  SystemdNspawnConfig -> CPDocument -> Either CPError CPDocument
systemdNspawnConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  containerCapsToCPDocument cp1 cfgFileSection $
    _systemdNspawnCapabilities c

parseSystemdNspawnConfig :: CPDocument -> Either CPError SystemdNspawnConfig
parseSystemdNspawnConfig cp =
  SystemdNspawnConfig <$> parseContainerCapabilities cp cfgFileSection
