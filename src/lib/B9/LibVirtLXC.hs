{-| Implementation of an execution environment that uses "libvirt-lxc". -}
module B9.LibVirtLXC ( runInEnvironment
                     , supportedImageTypes
                     , setDefaultConfig
                     ) where

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import System.Directory
import System.FilePath
import Text.Printf ( printf )
import Data.Char (toLower)

import B9.ShellScript
import B9.B9Monad
import B9.DiskImages
import B9.ExecEnv
import B9.ConfigUtils

lxcDefaultRamSize :: RamSize
lxcDefaultRamSize = RamSize 1 GB

supportedImageTypes :: [ImageType]
supportedImageTypes = [Raw]

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env scriptIn =
  if emptyScript scriptIn
     then return True
     else setUp >>= execute
  where
    setUp = do
      cfg <- configureLibVirtLXC
      buildId <- getBuildId
      buildDir <- getBuildDir
      uuid <- randomUUID
      let scriptDirHost = buildDir </> "init-script"
          scriptDirGuest = "/" ++ buildId
          domain = createDomain cfg env buildId uuid' scriptDirHost
                   scriptDirGuest
          uuid' = printf "%U" uuid
          script = Begin [scriptIn, successMarkerCmd scriptDirGuest]
      domainFile <- (</> domainConfig) <$> getBuildDir
      liftIO $ do createDirectoryIfMissing True scriptDirHost
                  writeSh (scriptDirHost </> initScript) script
                  writeFile domainFile domain
      return $ Context scriptDirHost uuid domainFile cfg

    successMarkerCmd scriptDirGuest =
      As "root" [In scriptDirGuest [Run "touch" [successMarkerFile]]]

    execute (Context scriptDirHost uuid domainFile cfg) = do
      let virsh = virshCommand cfg
      cmd $ printf "%s create '%s'" virsh domainFile
      cmd $ printf "%s console %U" virsh uuid
      checkSuccessMarker scriptDirHost

    checkSuccessMarker scriptDirHost =
      liftIO (doesFileExist $ scriptDirHost </> successMarkerFile)

    successMarkerFile = "SUCCESS"

    virshCommand :: LibVirtLXCConfig -> String
    virshCommand cfg = printf "%s%s -c %s" useSudo' virshPath' virshURI'
      where useSudo' = if useSudo cfg then "sudo " else ""
            virshPath' = virshPath cfg
            virshURI' = virshURI cfg

data Context = Context FilePath UUID FilePath LibVirtLXCConfig

data LibVirtLXCConfig = LibVirtLXCConfig { useSudo :: Bool
                                         , virshPath :: FilePath
                                         , emulator :: FilePath
                                         , virshURI :: FilePath
                                         , networkId :: Maybe String
                                         , guestCapabilities :: [LXCGuestCapability]
                                         } deriving (Read, Show)

-- | Available linux capabilities for lxc containers. This maps directly to the
-- capabilities defined in 'man 7 capabilities'.
data LXCGuestCapability =
       CAP_MKNOD
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

defaultLibVirtLXCConfig :: LibVirtLXCConfig
defaultLibVirtLXCConfig = LibVirtLXCConfig
                          True
                          "/usr/bin/virsh"
                          "/usr/lib/libvirt/libvirt_lxc"
                          "lxc:///"
                          Nothing
                          [CAP_MKNOD
                          ,CAP_SYS_ADMIN
                          ,CAP_SYS_CHROOT
                          ,CAP_SETGID
                          ,CAP_SETUID
                          ,CAP_NET_BIND_SERVICE
                          ,CAP_SETPCAP
                          ,CAP_SYS_PTRACE
                          ,CAP_SYS_MODULE]

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

configureLibVirtLXC :: B9 LibVirtLXCConfig
configureLibVirtLXC = do
  c <- readLibVirtConfig
  traceL $ printf "USING LibVirtLXCConfig: %s" (show c)
  return c

setDefaultConfig :: ConfigParser
setDefaultConfig = either (error . show) id eitherCp
  where
    eitherCp = do
      let cp = emptyCP
          c = defaultLibVirtLXCConfig
      cp1 <- add_section cp cfgFileSection
      cp2 <- setshow cp1 cfgFileSection useSudoK $ useSudo c
      cp3 <- set cp2 cfgFileSection virshPathK $ virshPath c
      cp4 <- set cp3 cfgFileSection emulatorK $ emulator c
      cp5 <- set cp4 cfgFileSection virshURIK $ virshURI c
      cp6 <- setshow cp5 cfgFileSection networkIdK $ networkId c
      setshow cp6 cfgFileSection guestCapabilitiesK $ guestCapabilities c

readLibVirtConfig :: B9 LibVirtLXCConfig
readLibVirtConfig = do
  cp <- getConfigParser
  let geto :: (Get_C a, Read a) => OptionSpec -> a -> a
      geto = getOptionOr cp cfgFileSection
  return $ LibVirtLXCConfig {
    useSudo = geto useSudoK $ useSudo defaultLibVirtLXCConfig
    , virshPath = geto virshPathK $ virshPath defaultLibVirtLXCConfig
    , emulator = geto emulatorK $ emulator defaultLibVirtLXCConfig
    , virshURI = geto virshURIK $ virshURI defaultLibVirtLXCConfig
    , networkId = geto networkIdK $ networkId defaultLibVirtLXCConfig
    , guestCapabilities = geto guestCapabilitiesK $
                          guestCapabilities defaultLibVirtLXCConfig
    }

initScript :: String
initScript = "init.sh"

domainConfig :: String
domainConfig = "domain.xml"

createDomain :: LibVirtLXCConfig
             -> ExecEnv
             -> String
             -> String
             -> FilePath
             -> FilePath
             -> String
createDomain cfg e buildId uuid scriptDirHost scriptDirGuest =
  "<domain type='lxc'>\n\
  \  <name>" ++ buildId ++ "</name>\n\
  \  <uuid>" ++ uuid ++ "</uuid>\n\
  \  <memory unit='" ++ memoryUnit e ++ "'>" ++ memoryAmount e ++ "</memory>\n\
  \  <currentMemory unit='" ++ memoryUnit e ++ "'>" ++ memoryAmount e ++ "</currentMemory>\n\
  \  <vcpu placement='static'>" ++ cpuCountStr e ++ "</vcpu>\n\
  \  <features>\n\
  \   <capabilities policy='default'>\n\
  \     "++ renderGuestCapabilityEntries cfg  ++"\n\
  \   </capabilities>\n\
  \  </features>\n\
  \  <os>\n\
  \    <type arch='" ++ osArch e ++ "'>exe</type>\n\
  \    <init>" ++ scriptDirGuest </> initScript ++ "</init>\n\
  \  </os>\n\
  \  <clock offset='utc'/>\n\
  \  <on_poweroff>destroy</on_poweroff>\n\
  \  <on_reboot>restart</on_reboot>\n\
  \  <on_crash>destroy</on_crash>\n\
  \  <devices>\n\
  \    <emulator>" ++ emulator cfg ++ "</emulator>\n"
  ++ unlines (libVirtNetwork (networkId cfg) ++
              (fsImage <$> (envImageMounts e)) ++
              (fsSharedDir <$> (envSharedDirectories e))) ++ "\n" ++
  "    <filesystem type='mount'>\n\
  \      <source dir='" ++ scriptDirHost ++ "'/>\n\
  \      <target dir='" ++ scriptDirGuest ++ "'/>\n\
  \    </filesystem>\n\
  \    <console>\n\
  \      <target type='lxc' port='0'/>\n\
  \    </console>\n\
  \  </devices>\n\
  \</domain>\n"

renderGuestCapabilityEntries :: LibVirtLXCConfig -> String
renderGuestCapabilityEntries = unlines . map render . guestCapabilities
  where
    render :: LXCGuestCapability -> String
    render cap = let capStr = toLower <$> drop (length "CAP_") (show cap)
                 in printf "<%s state='on'/>" capStr

osArch :: ExecEnv -> String
osArch e = case cpuArch (envResources e) of
            X86_64 -> "x86_64"
            I386 -> "i686"

libVirtNetwork :: Maybe String -> [String]
libVirtNetwork Nothing = []
libVirtNetwork (Just n) =
  [ "<interface type='network'>"
  , "  <source network='" ++ n ++ "'/>"
  , "</interface>" ]

fsImage :: (Image, MountPoint) -> String
fsImage (img, mnt) =
  case fsTarget mnt of
    Just mntXml ->
      "<filesystem type='file' accessmode='passthrough'>\n  " ++
      fsImgDriver img ++ "\n  " ++ fsImgSource img ++ "\n  " ++ mntXml ++
      "\n</filesystem>"
    Nothing ->
      ""
  where
    fsImgDriver (Image _img fmt _fs) =
      printf "<driver %s %s/>" driver fmt'
      where
        (driver, fmt') = case fmt of
          Raw -> ("type='loop'", "format='raw'")
          QCow2 -> ("type='nbd'", "format='qcow2'")
          Vmdk -> ("type='nbd'", "format='vmdk'")

    fsImgSource (Image src _fmt _fs) = "<source file='" ++ src ++ "'/>"

fsSharedDir :: SharedDirectory -> String
fsSharedDir (SharedDirectory hostDir mnt) =
  case fsTarget mnt of
    Just mntXml ->
      "<filesystem type='mount'>\n  " ++
      "<source dir='" ++ hostDir ++ "'/>" ++ "\n  " ++ mntXml ++
      "\n</filesystem>"
    Nothing ->
      ""
fsSharedDir (SharedDirectoryRO hostDir mnt) =
  case fsTarget mnt of
    Just mntXml ->
      "<filesystem type='mount'>\n  " ++
      "<source dir='" ++ hostDir ++ "'/>" ++ "\n  " ++ mntXml ++
      "\n  <readonly />\n</filesystem>"
    Nothing ->
      ""
fsSharedDir (SharedSources _) =
  error "Unreachable code reached!"

fsTarget :: MountPoint -> Maybe String
fsTarget (MountPoint dir) = Just $ "<target dir='" ++ dir ++ "'/>"
fsTarget _ = Nothing

memoryUnit :: ExecEnv -> String
memoryUnit = toUnit . maxMemory . envResources
  where
    toUnit AutomaticRamSize = toUnit lxcDefaultRamSize
    toUnit (RamSize _ u) = case u of
                            GB -> "GiB"
                            MB -> "MiB"
                            KB -> "KiB"
                            B -> "B"
memoryAmount :: ExecEnv -> String
memoryAmount = show . toAmount . maxMemory . envResources
  where
    toAmount AutomaticRamSize = toAmount lxcDefaultRamSize
    toAmount (RamSize n _) = n

cpuCountStr :: ExecEnv -> String
cpuCountStr = show . cpuCount . envResources
