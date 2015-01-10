module B9.LibVirtLXC ( runInEnvironment
                     , supportedImageTypes
                     , setDefaultConfig
                     ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( bracket )
import Control.Monad.IO.Class ( liftIO )
import Data.Monoid
import Data.Maybe
import Data.List ( intercalate )
import System.Directory ( getTemporaryDirectory
                        , getPermissions
                        , setPermissions
                        , setOwnerExecutable
                        , createDirectoryIfMissing
                        , doesFileExist
                        , removeDirectoryRecursive )
import System.FilePath ( (</>), (<.>) )
import System.IO ( writeFile )
import System.Process ( callCommand )
import Text.Printf ( printf )

import B9.ShellScript
import B9.B9Config
import B9.B9Monad
import B9.DiskImages
import B9.ExecEnv
import B9.ConfigUtils

supportedImageTypes = [Raw, QCow2, Vmdk]

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env scriptIn = setUp >>= execute
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
                                         } deriving (Read, Show)
defaultLibVirtLXCConfig = LibVirtLXCConfig
                          True
                          "/usr/bin/virsh"
                          "/usr/lib/libvirt/libvirt_lxc"
                          "lxc:///"
                          Nothing

cfgFileSection = "libvirt-lxc"
useSudoK = "use_sudo"
virshPathK = "virsh_path"
emulatorK = "emulator_path"
virshURIK = "connection"
networkIdK = "network"

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
      cp <- add_section cp cfgFileSection
      cp <- setshow cp cfgFileSection useSudoK $ useSudo c
      cp <- set cp cfgFileSection virshPathK $ virshPath c
      cp <- set cp cfgFileSection emulatorK $ emulator c
      cp <- set cp cfgFileSection virshURIK $ virshURI c
      setshow cp cfgFileSection networkIdK $ networkId c

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
    }

initScript = "init.sh"
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

osArch e = case cpuArch (envResources e) of
            X86_64 -> "x86_64"
            I386 -> "i686"

libVirtNetwork Nothing = []
libVirtNetwork (Just networkId) =
  [ "<interface type='network'>"
  , "  <source network='" ++ networkId ++ "'/>"
  , "</interface>" ]


fsImage (img, mnt) =
  "<filesystem type='file' accessmode='passthrough'>\n  " ++
  fsImgDriver img ++ "\n  " ++ fsImgSource img ++ "\n  " ++ fsTarget mnt ++
  "\n</filesystem>"
  where
    fsImgDriver (Image _img fmt) =
      printf "<driver %s %s/>" driver fmt'
      where
        (driver, fmt') = case fmt of
          Raw -> ("type='loop'", "format='raw'")
          QCow2 -> ("type='nbd'", "format='qcow2'")
          Vmdk -> ("type='nbd'", "format='vmdk'")

    fsImgSource (Image img _fmt) = "<source file='" ++ img ++ "'/>"

fsSharedDir (SharedDirectory hostDir mnt) =
  "<filesystem type='mount'>\n  " ++
  fsSharedDirSource hostDir ++ "\n  " ++ fsTarget mnt ++
  "\n</filesystem>"
  where
    fsSharedDirSource hostDir = "<source dir='" ++ hostDir ++ "'/>"

fsTarget (MountPoint dir) = "<target dir='" ++ dir ++ "'/>"

memoryUnit = toUnit . maxMemory . envResources
  where
    toUnit (RamSize _ u) = case u of
                            GB -> "GiB"
                            MB -> "MiB"
                            KB -> "KiB"
                            B -> "B"

memoryAmount = show . toAmount . maxMemory . envResources
  where
    toAmount (RamSize n _) = n

cpuCountStr = show . cpuCount . envResources
