{-| Implementation of an execution environment that uses "libvirt-lxc". -}
module B9.LibVirtLXC ( runInEnvironment
                     , supportedImageTypes
                     , logLibVirtLXCConfig
                     , module X
                     ) where

import Control.Monad.IO.Class ( liftIO )
import System.Directory
import System.FilePath
import Text.Printf ( printf )
import Data.Char (toLower)
import Control.Lens (view)

import B9.ShellScript
import B9.B9Monad
import B9.B9Config (libVirtLXCConfigs)
import B9.DiskImages
import B9.ExecEnv
import B9.B9Config.LibVirtLXC as X
import Data.ConfigFile.B9Extras


logLibVirtLXCConfig :: LibVirtLXCConfig -> B9 ()
logLibVirtLXCConfig c = traceL $ printf "USING LibVirtLXCConfig: %s" (show c)

supportedImageTypes :: [ImageType]
supportedImageTypes = [Raw]

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env scriptIn = if emptyScript scriptIn
  then return True
  else setUp >>= execute
 where
  setUp = do
    mcfg         <- view libVirtLXCConfigs <$> getConfig
    cfg          <- maybe (fail "No LibVirtLXC Configuration!") return mcfg
    buildId      <- getBuildId
    buildBaseDir <- getBuildDir
    uuid         <- randomUUID
    let
      scriptDirHost  = buildDir </> "init-script"
      scriptDirGuest = "/" ++ buildId
      domainFile     = buildBaseDir </> uuid' <.> domainConfig
      domain = createDomain cfg env buildId uuid' scriptDirHost scriptDirGuest
      uuid'          = printf "%U" uuid
      setupEnv       = Begin
        [ Run "export" ["HOME=/root"]
        , Run "export" ["USER=root"]
        , Run "source" ["/etc/profile"]
        ]
      script   = Begin [setupEnv, scriptIn, successMarkerCmd scriptDirGuest]
      buildDir = buildBaseDir </> uuid'
    liftIO $ do
      createDirectoryIfMissing True                           scriptDirHost
      writeSh                  (scriptDirHost </> initScript) script
      writeFile                domainFile                     domain
    return $ Context scriptDirHost uuid domainFile cfg

  successMarkerCmd scriptDirGuest =
    In scriptDirGuest [Run "touch" [successMarkerFile]]

  execute (Context scriptDirHost _uuid domainFile cfg) = do
    let virsh = virshCommand cfg
    cmd $ printf "%s create '%s' --console --autodestroy" virsh domainFile
    -- cmd $ printf "%s console %U" virsh uuid
    liftIO (doesFileExist $ scriptDirHost </> successMarkerFile)

  successMarkerFile = "SUCCESS"

  virshCommand :: LibVirtLXCConfig -> String
  virshCommand cfg = printf "%s%s -c %s" useSudo' virshPath' virshURI'
   where
    useSudo'   = if useSudo cfg then "sudo " else ""
    virshPath' = virshPath cfg
    virshURI'  = virshURI cfg

data Context = Context FilePath UUID FilePath LibVirtLXCConfig


initScript :: String
initScript = "init.sh"

domainConfig :: String
domainConfig = "domain.xml"

createDomain
  :: LibVirtLXCConfig
  -> ExecEnv
  -> String
  -> String
  -> FilePath
  -> FilePath
  -> String
createDomain cfg e buildId uuid scriptDirHost scriptDirGuest =
  "<domain type='lxc'>\n  <name>"
    ++  buildId
    ++  "</name>\n  <uuid>"
    ++  uuid
    ++  "</uuid>\n  <memory unit='"
    ++  memoryUnit cfg e
    ++  "'>"
    ++  memoryAmount cfg e
    ++  "</memory>\n  <currentMemory unit='"
    ++  memoryUnit cfg e
    ++  "'>"
    ++  memoryAmount cfg e
    ++  "</currentMemory>\n  <vcpu placement='static'>"
    ++  cpuCountStr e
    ++  "</vcpu>\n  <features>\n   <capabilities policy='default'>\n     "
    ++  renderGuestCapabilityEntries cfg
    ++  "\n   </capabilities>\n  </features>\n  <os>\n    <type arch='"
    ++  osArch e
    ++  "'>exe</type>\n    <init>"
    ++  scriptDirGuest
    </> initScript
    ++ "</init>\n  </os>\n  <clock offset='utc'/>\n  <on_poweroff>destroy</on_poweroff>\n  <on_reboot>restart</on_reboot>\n  <on_crash>destroy</on_crash>\n  <devices>\n    <emulator>"
    ++  emulator cfg
    ++  "</emulator>\n"
    ++  unlines
          (  libVirtNetwork (networkId cfg)
          ++ (fsImage <$> envImageMounts e)
          ++ (fsSharedDir <$> envSharedDirectories e)
          )
    ++  "\n"
    ++  "    <filesystem type='mount'>\n      <source dir='"
    ++  scriptDirHost
    ++  "'/>\n      <target dir='"
    ++  scriptDirGuest
    ++ "'/>\n    </filesystem>\n    <console>\n      <target type='lxc' port='0'/>\n    </console>\n  </devices>\n</domain>\n"

renderGuestCapabilityEntries :: LibVirtLXCConfig -> String
renderGuestCapabilityEntries = unlines . map render . guestCapabilities
 where
  render :: LXCGuestCapability -> String
  render cap =
    let capStr = toLower <$> drop (length "CAP_") (show cap)
    in  printf "<%s state='on'/>" capStr

osArch :: ExecEnv -> String
osArch e = case cpuArch (envResources e) of
  X86_64 -> "x86_64"
  I386   -> "i686"

libVirtNetwork :: Maybe String -> [String]
libVirtNetwork Nothing = []
libVirtNetwork (Just n) =
  [ "<interface type='network'>"
  , "  <source network='" ++ n ++ "'/>"
  , "</interface>"
  ]

fsImage :: (Image, MountPoint) -> String
fsImage (img, mnt) = case fsTarget mnt of
  Just mntXml ->
    "<filesystem type='file' accessmode='passthrough'>\n  "
      ++ fsImgDriver img
      ++ "\n  "
      ++ fsImgSource img
      ++ "\n  "
      ++ mntXml
      ++ "\n</filesystem>"
  Nothing -> ""
 where
  fsImgDriver (Image _img fmt _fs) = printf "<driver %s %s/>" driver fmt'
   where
    (driver, fmt') = case fmt of
      Raw   -> ("type='loop'", "format='raw'")
      QCow2 -> ("type='nbd'", "format='qcow2'")
      Vmdk  -> ("type='nbd'", "format='vmdk'")

  fsImgSource (Image src _fmt _fs) = "<source file='" ++ src ++ "'/>"

fsSharedDir :: SharedDirectory -> String
fsSharedDir (SharedDirectory hostDir mnt) = case fsTarget mnt of
  Just mntXml ->
    "<filesystem type='mount'>\n  "
      ++ "<source dir='"
      ++ hostDir
      ++ "'/>"
      ++ "\n  "
      ++ mntXml
      ++ "\n</filesystem>"
  Nothing -> ""
fsSharedDir (SharedDirectoryRO hostDir mnt) = case fsTarget mnt of
  Just mntXml ->
    "<filesystem type='mount'>\n  "
      ++ "<source dir='"
      ++ hostDir
      ++ "'/>"
      ++ "\n  "
      ++ mntXml
      ++ "\n  <readonly />\n</filesystem>"
  Nothing -> ""
fsSharedDir (SharedSources _) = error "Unreachable code reached!"

fsTarget :: MountPoint -> Maybe String
fsTarget (MountPoint dir) = Just $ "<target dir='" ++ dir ++ "'/>"
fsTarget _                = Nothing

memoryUnit :: LibVirtLXCConfig -> ExecEnv -> String
memoryUnit cfg = toUnit . maxMemory . envResources
 where
  toUnit AutomaticRamSize = toUnit (guestRamSize cfg)
  toUnit (RamSize _ u)    = case u of
    GB -> "GiB"
    MB -> "MiB"
    KB -> "KiB"
    B  -> "B"
memoryAmount :: LibVirtLXCConfig -> ExecEnv -> String
memoryAmount cfg = show . toAmount . maxMemory . envResources
 where
  toAmount AutomaticRamSize = toAmount (guestRamSize cfg)
  toAmount (RamSize n _)    = n

cpuCountStr :: ExecEnv -> String
cpuCountStr = show . cpuCount . envResources


