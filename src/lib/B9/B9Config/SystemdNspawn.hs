module B9.B9Config.SystemdNspawn
  ( systemdNspawnConfigToCPDocument,
    defaultSystemdNspawnConfig,
    parseSystemdNspawnConfig,
    SystemdNspawnConfig (..),
    SystemdNspawnConsole (..),
    systemdNspawnCapabilities,
    systemdNspawnUseSudo,
    systemdNspawnMaxLifetimeSeconds,
    systemdNspawnExtraArgs,
    systemdNspawnExecutable,
    systemdNspawnConsole,
  )
where

import B9.B9Config.Container
import Control.Lens (makeLenses)
import Data.ConfigFile.B9Extras
import Text.Read

-- TODO document b9 config file
data SystemdNspawnConfig
  = SystemdNspawnConfig
      { _systemdNspawnCapabilities :: [ContainerCapability],
        _systemdNspawnUseSudo :: Bool,
        _systemdNspawnMaxLifetimeSeconds :: Maybe Int,
        _systemdNspawnExtraArgs :: Maybe String,
        _systemdNspawnExecutable :: Maybe FilePath,
        _systemdNspawnConsole :: SystemdNspawnConsole
      }
  deriving (Read, Show, Eq)

data SystemdNspawnConsole
  = SystemdNspawnInteractive
  | SystemdNspawnReadOnly
  | SystemdNspawnPassive
  | SystemdNspawnPipe
  deriving (Eq)

instance Show SystemdNspawnConsole where
  show x = case x of
    SystemdNspawnInteractive -> "interactive"
    SystemdNspawnReadOnly -> "read-only"
    SystemdNspawnPassive -> "passive"
    SystemdNspawnPipe -> "pipe"

instance Read SystemdNspawnConsole where
  readPrec =
    do
      Ident "interactive" <- lexP
      return SystemdNspawnInteractive
      +++ do
        Ident "read-only" <- lexP
        return SystemdNspawnReadOnly
      +++ do
        Ident "passive" <- lexP
        return SystemdNspawnPassive
      +++ do
        Ident "pipe" <- lexP
        return SystemdNspawnReadOnly

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
    True
    (Just (4 * 3600))
    Nothing
    Nothing
    SystemdNspawnReadOnly

cfgFileSection :: String
cfgFileSection = "systemdNspawn"

useSudoK :: String
useSudoK = "use_sudo"

maxLifetimeSecondsK :: String
maxLifetimeSecondsK = "max_lifetime_seconds"

extraArgsK :: String
extraArgsK = "extra_args"

executableK :: String
executableK = "executable"

consoleK :: String
consoleK = "console"

systemdNspawnConfigToCPDocument ::
  SystemdNspawnConfig -> CPDocument -> Either CPError CPDocument
systemdNspawnConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <-
    containerCapsToCPDocument cp1 cfgFileSection $
      _systemdNspawnCapabilities c
  cp3 <- setShowCP cp2 cfgFileSection useSudoK $ _systemdNspawnUseSudo c
  cp4 <- setShowCP cp3 cfgFileSection maxLifetimeSecondsK $ _systemdNspawnMaxLifetimeSeconds c
  cp5 <- setShowCP cp4 cfgFileSection extraArgsK $ _systemdNspawnExtraArgs c
  cp6 <- setShowCP cp5 cfgFileSection executableK $ _systemdNspawnExecutable c
  setShowCP cp6 cfgFileSection consoleK $ _systemdNspawnConsole c

parseSystemdNspawnConfig :: CPDocument -> Either CPError SystemdNspawnConfig
parseSystemdNspawnConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in SystemdNspawnConfig
        <$> parseContainerCapabilities cp cfgFileSection
        <*> getr useSudoK
        <*> getr maxLifetimeSecondsK
        <*> getr extraArgsK
        <*> getr executableK
        <*> getr consoleK
