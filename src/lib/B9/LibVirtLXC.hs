-- | Implementation of an execution environment that uses "libvirt-lxc".
module B9.LibVirtLXC
  ( LibVirtLXC (..),
    module X,
  )
where

import B9.B9Config
  ( B9ConfigReader,
    ContainerCapability,
    getB9Config,
    libVirtLXCConfigs,
  )
import B9.B9Config.LibVirtLXC as X
import B9.B9Error
import B9.B9Exec
import B9.B9Logging
import B9.BuildInfo
import B9.Container
import B9.DiskImages
import B9.ExecEnv
import B9.ShellScript
import Control.Eff
import Control.Lens (view)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Char (toLower)
import System.Directory
import System.FilePath
import System.IO.B9Extras
  ( UUID (),
    randomUUID,
  )
import Text.Printf (printf)

newtype LibVirtLXC = LibVirtLXC LibVirtLXCConfig

instance Backend LibVirtLXC where
  getBackendConfig _ =
    fmap LibVirtLXC . view libVirtLXCConfigs <$> getB9Config
  supportedImageTypes _ = [Raw]
  runInEnvironment (LibVirtLXC cfgIn) env scriptIn =
    if emptyScript scriptIn
      then return True
      else setUp >>= execute
    where
      setUp = do
        buildId <- getBuildId
        buildBaseDir <- getBuildDir
        uuid <- randomUUID
        let scriptDirHost = buildDir </> "init-script"
            scriptDirGuest = "/" ++ buildId
            domainFile = buildBaseDir </> uuid' <.> domainConfig
            mkDomain =
              createDomain cfgIn env buildId uuid' scriptDirHost scriptDirGuest
            uuid' = printf "%U" uuid
            setupEnv =
              Begin
                [ Run "export" ["HOME=/root"],
                  Run "export" ["USER=root"],
                  Run "source" ["/etc/profile"]
                ]
            script = Begin [setupEnv, scriptIn, successMarkerCmd scriptDirGuest]
            buildDir = buildBaseDir </> uuid'
        liftIO $ do
          createDirectoryIfMissing True scriptDirHost
          writeSh (scriptDirHost </> initScript) script
          domain <- mkDomain
          writeFile domainFile domain
        return $ Context scriptDirHost uuid domainFile cfgIn

successMarkerCmd :: FilePath -> Script
successMarkerCmd scriptDirGuest =
  In scriptDirGuest [Run "touch" [successMarkerFile]]

successMarkerFile :: [Char]
successMarkerFile = "SUCCESS"

execute :: (Member ExcB9 e, CommandIO e, Member B9ConfigReader e) => Context -> Eff e Bool
execute (Context scriptDirHost _uuid domainFile cfg) = do
  cmd $ printf "%s create '%s' --console --autodestroy" virshCommand domainFile
  -- cmd $ printf "%s console %U" virsh uuid
  liftIO (doesFileExist $ scriptDirHost </> successMarkerFile)
  where
    virshCommand :: String
    virshCommand = printf "%svirsh -c %s" useSudo' virshURI'
      where
        useSudo' = if useSudo cfg then "sudo " else ""
        virshURI' = virshURI cfg

data Context
  = Context
      FilePath
      UUID
      FilePath
      LibVirtLXCConfig

initScript :: String
initScript = "init.sh"

domainConfig :: String
domainConfig = "domain.xml"

createDomain ::
  MonadIO m =>
  LibVirtLXCConfig ->
  ExecEnv ->
  String ->
  String ->
  FilePath ->
  FilePath ->
  m String
createDomain cfg e buildId uuid scriptDirHost scriptDirGuest = do
  emulatorPath <- getEmulatorPath cfg
  pure
    ( "<domain type='lxc'>\n  <name>"
        ++ buildId
        ++ "</name>\n  <uuid>"
        ++ uuid
        ++ "</uuid>\n  <memory unit='"
        ++ memoryUnit cfg e
        ++ "'>"
        ++ memoryAmount cfg e
        ++ "</memory>\n  <currentMemory unit='"
        ++ memoryUnit cfg e
        ++ "'>"
        ++ memoryAmount cfg e
        ++ "</currentMemory>\n  <vcpu placement='static'>"
        ++ cpuCountStr e
        ++ "</vcpu>\n  <features>\n   <capabilities policy='default'>\n     "
        ++ renderGuestCapabilityEntries cfg
        ++ "\n   </capabilities>\n  </features>\n  <os>\n    <type arch='"
        ++ osArch e
        ++ "'>exe</type>\n    <init>"
        ++ scriptDirGuest
        </> initScript
        ++ "</init>\n  </os>\n  <clock offset='utc'/>\n  <on_poweroff>destroy</on_poweroff>\n  <on_reboot>restart</on_reboot>\n  <on_crash>destroy</on_crash>\n  <devices>\n    <emulator>"
        ++ emulatorPath
        ++ "</emulator>\n"
        ++ unlines
          ( libVirtNetwork (_networkId cfg)
              ++ (fsImage <$> envImageMounts e)
              ++ (fsSharedDir <$> envSharedDirectories e)
          )
        ++ "\n"
        ++ "    <filesystem type='mount'>\n      <source dir='"
        ++ scriptDirHost
        ++ "'/>\n      <target dir='"
        ++ scriptDirGuest
        ++ "'/>\n    </filesystem>\n    <console>\n      <target type='lxc' port='0'/>\n    </console>\n  </devices>\n</domain>\n"
    )

renderGuestCapabilityEntries :: LibVirtLXCConfig -> String
renderGuestCapabilityEntries = unlines . map render . guestCapabilities
  where
    render :: ContainerCapability -> String
    render cap =
      let capStr = toLower <$> drop (length "CAP_") (show cap)
       in printf "<%s state='on'/>" capStr

osArch :: ExecEnv -> String
osArch e = case cpuArch (envResources e) of
  X86_64 -> "x86_64"
  I386 -> "i686"

libVirtNetwork :: Maybe String -> [String]
libVirtNetwork Nothing = []
libVirtNetwork (Just n) =
  [ "<interface type='network'>",
    "  <source network='" ++ n ++ "'/>",
    "</interface>"
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
          Raw -> ("type='loop'", "format='raw'")
          QCow2 -> ("type='nbd'", "format='qcow2'")
          Vmdk -> ("type='nbd'", "format='vmdk'")
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
fsTarget _ = Nothing

memoryUnit :: LibVirtLXCConfig -> ExecEnv -> String
memoryUnit cfg = toUnit . maxMemory . envResources
  where
    toUnit AutomaticRamSize = toUnit (guestRamSize cfg)
    toUnit (RamSize _ u) = case u of
      GB -> "GiB"
      MB -> "MiB"
      KB -> "KiB"

memoryAmount :: LibVirtLXCConfig -> ExecEnv -> String
memoryAmount cfg = show . toAmount . maxMemory . envResources
  where
    toAmount AutomaticRamSize = toAmount (guestRamSize cfg)
    toAmount (RamSize n _) = n

cpuCountStr :: ExecEnv -> String
cpuCountStr = show . cpuCount . envResources
