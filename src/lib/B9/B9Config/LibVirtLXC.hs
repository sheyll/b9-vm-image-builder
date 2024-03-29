module B9.B9Config.LibVirtLXC
  ( libVirtLXCConfigToCPDocument,
    defaultLibVirtLXCConfig,
    parseLibVirtLXCConfig,
    LibVirtLXCConfig (..),
    networkId,
    getEmulatorPath,
  )
where

import B9.B9Config.Container
import B9.DiskImages
import B9.ExecEnv
import Control.Lens (makeLenses)
import Control.Monad.IO.Class
import Data.ConfigFile.B9Extras
import Data.Maybe (fromMaybe)
import System.Environment.Blank as SysIO
import Test.QuickCheck (Arbitrary(arbitrary),oneof,listOf1)
import B9.QCUtil (smaller, arbitraryFilePath, arbitraryLetter)

data LibVirtLXCConfig
  = LibVirtLXCConfig
      { useSudo :: Bool,
        emulator :: Maybe FilePath,
        virshURI :: FilePath,
        _networkId :: Maybe String,
        guestCapabilities :: [ContainerCapability],
        guestRamSize :: RamSize,
        imageFileNameShortenerBasePath :: Maybe FilePath
      }
  deriving (Read, Show, Eq)

instance Arbitrary LibVirtLXCConfig where
  arbitrary = 
    LibVirtLXCConfig <$>
    smaller arbitrary <*>
    smaller (oneof [pure Nothing, Just <$> arbitraryFilePath]) <*>
    smaller arbitraryFilePath <*>
    smaller (oneof [pure Nothing, Just <$> listOf1 arbitraryLetter]) <*>
    smaller arbitrary <*>
    pure (RamSize 4 GB) <*>
    smaller (oneof [pure Nothing, Just <$> arbitraryFilePath])

makeLenses ''LibVirtLXCConfig

defaultLibVirtLXCConfig :: LibVirtLXCConfig
defaultLibVirtLXCConfig =
  LibVirtLXCConfig
    True
    (Just "/usr/lib/libvirt/libvirt_lxc")
    "lxc:///"
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
    (RamSize 1 GB)
    Nothing

cfgFileSection :: String
cfgFileSection = "libvirt-lxc"

useSudoK :: String
useSudoK = "use_sudo"

emulatorK :: String
emulatorK = "emulator_path"

-- NOTE: This variable name is also specified in the NIX build
-- in @default.nix@.
emulatorEnvVar :: String
emulatorEnvVar = "B9_LIBVIRT_LXC"

virshURIK :: String
virshURIK = "connection"

networkIdK :: String
networkIdK = "network"

guestRamSizeK :: String
guestRamSizeK = "guest_ram_size"

imageFileNamesShortenerBasePathK :: String
imageFileNamesShortenerBasePathK = "image_file_names_shortener_base_path"

libVirtLXCConfigToCPDocument ::
  LibVirtLXCConfig -> CPDocument -> Either CPError CPDocument
libVirtLXCConfigToCPDocument c cp = do
  cp1 <- addSectionCP cp cfgFileSection
  cp2 <- setShowCP cp1 cfgFileSection useSudoK $ useSudo c
  cp3 <- setShowCP cp2 cfgFileSection emulatorK $ emulator c
  cp4 <- setCP cp3 cfgFileSection virshURIK $ virshURI c
  cp5 <- setShowCP cp4 cfgFileSection networkIdK $ _networkId c
  cp6 <- containerCapsToCPDocument cp5 cfgFileSection $ guestCapabilities c
  cp7 <- setShowCP cp6 cfgFileSection guestRamSizeK $ guestRamSize c
  cpFinal <- setShowCP cp7 cfgFileSection imageFileNamesShortenerBasePathK $ imageFileNameShortenerBasePath c
  return cpFinal

parseLibVirtLXCConfig :: CPDocument -> Either CPError LibVirtLXCConfig
parseLibVirtLXCConfig cp =
  let getr :: (CPGet a) => CPOptionSpec -> Either CPError a
      getr = readCP cp cfgFileSection
   in LibVirtLXCConfig
        <$> getr useSudoK
        <*> getr emulatorK
        <*> getr virshURIK
        <*> getr networkIdK
        <*> parseContainerCapabilities cp cfgFileSection
        <*> getr guestRamSizeK
        <*> getr imageFileNamesShortenerBasePathK

-- | Return the path to @/usr/lib/libvirt/libexec/libvirt_lxc@
--  the 'emulatorK' field from the config file, or set the path
-- in the environment variable named like the value in 'emulatorEnvVar'
-- dictates.
--
-- @since 0.5.66
getEmulatorPath :: MonadIO m => LibVirtLXCConfig -> m FilePath
getEmulatorPath cfg =
  liftIO (SysIO.getEnvDefault emulatorEnvVar fromCfgOrDefault)
  where
    fromCfgOrDefault = fromMaybe "/usr/lib/libexec/libvirt_lxc" (emulator cfg)
