module B9.LibVirtLXC ( runInEnvironment, supportedImageTypes ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( bracket )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( intercalate )
import Data.Word ( Word16, Word32 )
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
import System.Random ( randomIO )
import Text.Printf ( printf )

import B9.B9Config ( useSudo )
import B9.ShellScript
import B9.B9Monad
import B9.DiskImages
import B9.ExecEnv

type UUID = String

data Context = Context FilePath UUID FilePath

supportedImageTypes = [Raw, QCow2, Vmdk]

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env scriptIn = setUp >>= execute
  where
    setUp = do
      buildId <- getBuildId
      buildDir <- getBuildDir
      uuid <- liftIO $ createUUID
      let scriptDirHost = buildDir </> "init-script"
          scriptDirGuest = "/" ++ buildId
          domain = createDomain env uuid scriptDirHost scriptDirGuest
          script = Begin [scriptIn, successMarkerCmd scriptDirGuest]
      domainFile <- (</> domainConfig) <$> getBuildDir
      liftIO $ do createDirectoryIfMissing True scriptDirHost
                  writeSh (scriptDirHost </> initScript) script
                  writeFile domainFile domain
      return $ Context scriptDirHost uuid domainFile

    successMarkerCmd scriptDirGuest =
      As "root" [In scriptDirGuest [Run "touch" [successMarkerFile]]]

    execute (Context scriptDirHost uuid domainFile) = do
      cfg <- getConfig
      let sudo = if useSudo cfg then "sudo " else ""
      cmd $ printf "%svirsh -c lxc:/// create '%s'" sudo domainFile
      cmd $ printf "%svirsh -c lxc:/// console %s" sudo uuid
      checkSuccessMarker scriptDirHost

    checkSuccessMarker scriptDirHost =
      liftIO (doesFileExist $ scriptDirHost </> successMarkerFile)

    successMarkerFile = "SUCCESS"

createUUID = printf "%08x-%04x-%04x-%04x-%08x%04x"
             <$> (randomIO :: IO Word32)
             <*> (randomIO :: IO Word16)
             <*> (randomIO :: IO Word16)
             <*> (randomIO :: IO Word16)
             <*> (randomIO :: IO Word32)
             <*> (randomIO :: IO Word16)

initScript = "init.sh"
domainConfig = "domain.xml"

createDomain :: ExecEnv -> String -> FilePath -> FilePath -> String
createDomain e uuid scriptDirHost scriptDirGuest =
  "<domain type='lxc'>\n\
  \  <name>" ++ envName e ++ "</name>\n\
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
  \    <emulator>/usr/lib/libvirt/libvirt_lxc</emulator>\n"
  ++ unlines (libVirtNetwork (networkId (envResources e)) ++
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
