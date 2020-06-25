module B9.B9Config.SystemdNspawn
  ( systemdNspawnConfigToCPDocument,
    defaultSystemdNspawnConfig,
    parseSystemdNspawnConfig,
    SystemdNspawnConfig (..),
    systemdNspawnCapabilities,
    systemdNspawnUseSudo,
  )
where

import B9.B9Config.Container
import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras

data SystemdNspawnConfig
  = SystemdNspawnConfig
      { _systemdNspawnCapabilities :: [ContainerCapability]
      , _systemdNspawnUseSudo :: Bool
      }
  deriving (Read, Show, Eq)

makeLenses ''SystemdNspawnConfig

useSudoK :: String
useSudoK = "use_sudo"

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
    True

cfgFileSection :: String
cfgFileSection = "systemdNspawn"

systemdNspawnConfigToCPDocument ::
  SystemdNspawnConfig -> CPDocument -> Either CPError CPDocument
systemdNspawnConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <- setShowCP cp1 cfgFileSection useSudoK $ _systemdNspawnUseSudo c
  containerCapsToCPDocument cp2 cfgFileSection $
    _systemdNspawnCapabilities c

parseSystemdNspawnConfig :: CPDocument -> Either CPError SystemdNspawnConfig
parseSystemdNspawnConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
  in SystemdNspawnConfig <$> parseContainerCapabilities cp cfgFileSection  <*> getr useSudoK
