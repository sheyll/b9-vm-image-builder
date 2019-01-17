module B9.B9Config.LibVirtLXC
    ( libVirtLXCConfigToCPDocument
    , defaultLibVirtLXCConfig
    , parseLibVirtLXCConfig
    , LibVirtLXCConfig(..)
    , networkId
    , LXCGuestCapability(..)
    )
where

import           B9.DiskImages
import           B9.ExecEnv
import           Data.ConfigFile.B9Extras
import           Control.Lens                   ( makeLenses )


data LibVirtLXCConfig = LibVirtLXCConfig { useSudo :: Bool
                                         , virshPath :: FilePath
                                         , emulator :: FilePath
                                         , virshURI :: FilePath
                                         , _networkId :: Maybe String
                                         , guestCapabilities :: [LXCGuestCapability]
                                         , guestRamSize :: RamSize
                                         } deriving (Read, Show)

-- | Available linux capabilities for lxc containers. This maps directly to the
-- capabilities defined in 'man 7 capabilities'.
data LXCGuestCapability = CAP_MKNOD
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
  deriving (Read, Show)

makeLenses ''LibVirtLXCConfig

defaultLibVirtLXCConfig :: LibVirtLXCConfig
defaultLibVirtLXCConfig = LibVirtLXCConfig
    True
    "/usr/bin/virsh"
    "/usr/lib/libvirt/libvirt_lxc"
    "lxc:///"
    Nothing
    [ CAP_MKNOD
    , CAP_SYS_ADMIN
    , CAP_SYS_CHROOT
    , CAP_SETGID
    , CAP_SETUID
    , CAP_NET_BIND_SERVICE
    , CAP_SETPCAP
    , CAP_SYS_PTRACE
    , CAP_SYS_MODULE
    ]
    (RamSize 1 GB)

cfgFileSection :: String
cfgFileSection = "libvirt-lxc"
useSudoK :: String
useSudoK = "use_sudo"
virshPathK :: String
virshPathK = "virsh_path"
emulatorK :: String
emulatorK = "emulator_path"
virshURIK :: String
virshURIK = "connection"
networkIdK :: String
networkIdK = "network"
guestCapabilitiesK :: String
guestCapabilitiesK = "guest_capabilities"
guestRamSizeK :: String
guestRamSizeK = "guest_ram_size"

libVirtLXCConfigToCPDocument
    :: LibVirtLXCConfig -> CPDocument -> Either CPError CPDocument
libVirtLXCConfigToCPDocument c cp = do
    cp1 <- addSectionCP cp cfgFileSection
    cp2 <- setShowCP cp1 cfgFileSection useSudoK $ useSudo c
    cp3 <- setCP cp2 cfgFileSection virshPathK $ virshPath c
    cp4 <- setCP cp3 cfgFileSection emulatorK $ emulator c
    cp5 <- setCP cp4 cfgFileSection virshURIK $ virshURI c
    cp6 <- setShowCP cp5 cfgFileSection networkIdK $ _networkId c
    cp7 <- setShowCP cp6 cfgFileSection guestCapabilitiesK $ guestCapabilities c
    setShowCP cp7 cfgFileSection guestRamSizeK $ guestRamSize c

parseLibVirtLXCConfig :: CPDocument -> Either CPError LibVirtLXCConfig
parseLibVirtLXCConfig cp =
    let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
        getr = readCP cp cfgFileSection
    in  LibVirtLXCConfig
        <$> (getr useSudoK)
        <*> (getr virshPathK)
        <*> (getr emulatorK)
        <*> (getr virshURIK)
        <*> (getr networkIdK)
        <*> (getr guestCapabilitiesK)
        <*> (getr guestRamSizeK)
