-- | Implementation of an execution environment that uses /systemdNspawn/.
module B9.SystemdNspawn
  ( SystemdNspawn (..),
  )
where

import B9.B9Config
  ( getB9Config,
    systemdNspawnConfigs,
  )
import B9.B9Config.SystemdNspawn as X
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
import Control.Monad (when)
import Control.Monad.IO.Class
  ( liftIO,
  )
import Data.Foldable (traverse_)
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe, maybe)
import System.Directory
import System.FilePath
import Text.Printf (printf)

newtype SystemdNspawn = SystemdNspawn SystemdNspawnConfig

type SudoPrepender = String -> String

instance Backend SystemdNspawn where
  getBackendConfig _ =
    fmap SystemdNspawn . view systemdNspawnConfigs <$> getB9Config

  supportedImageTypes _ = [Raw]

  runInEnvironment (SystemdNspawn dCfg) env scriptIn =
    if emptyScript scriptIn
      then return True
      else do
        let sudo = if _systemdNspawnUseSudo dCfg then ("sudo " ++) else id
        containerBuildDirs <- createContainerBuildRootDir
        containerMounts <- mountLoopbackImages sudo env containerBuildDirs
        finallyB9
          ( do
              bootScript <- prepareBootScript containerBuildDirs scriptIn
              execBuild sudo containerMounts (envSharedDirectories env) bootScript dCfg
          )
          ( do
              umountLoopbackImages sudo containerMounts
              removeContainerBuildRootDir sudo containerBuildDirs
          )

createContainerBuildRootDir ::
  (Member BuildInfoReader e, Member ExcB9 e, CommandIO e) => Eff e ContainerBuildDirectories
createContainerBuildRootDir = do
  buildD <- getBuildDir
  let loopbackMountDir = root </> "loopback_mounts"
      root = buildD </> "container_build_root"
  liftIO $ do
    createDirectoryIfMissing True root
    createDirectoryIfMissing True loopbackMountDir
  let res = ContainerBuildDirectories {containerBuildRoot = root, containerLoopbackMountRoot = loopbackMountDir}
  traceL ("Created container build directories: " ++ show res)
  return res

data ContainerBuildDirectories
  = ContainerBuildDirectories
      { containerBuildRoot :: FilePath,
        containerLoopbackMountRoot :: FilePath
      }
  deriving (Show)

mountLoopbackImages ::
  (Member BuildInfoReader e, Member ExcB9 e, CommandIO e) =>
  SudoPrepender ->
  ExecEnv ->
  ContainerBuildDirectories ->
  Eff e ContainerMounts
mountLoopbackImages sudo e containerDirs = do
  let imgMounts0 = [(img, mountPoint) | (img, MountPoint mountPoint) <- envImageMounts e]
      imgMounts = [(imgPath, mountPoint) | (Image imgPath _ _, mountPoint) <- imgMounts0]
      invalidImages = [x | x@(Image _ t _, _) <- imgMounts0, t /= Raw]
  when
    (not (null invalidImages))
    (throwB9Error ("Internal Error: Only 'raw' disk images can be used for container builds, and these images were supposed to be automatically converted: " ++ show invalidImages))
  case partition ((== "/") . snd) imgMounts of
    ([rootImg], otherImgs) -> do
      rootMount <- mountLoopback rootImg
      otherMounts <- traverse mountLoopback otherImgs
      return (ContainerMounts (Right rootMount) otherMounts)
    ([], _) ->
      throwB9Error "A containerized build requires that a disk image for the root-, i.e. the '/' directory is configured."
    (rootImgs, _) ->
      throwB9Error ("A containerized build requires that only one disk image for the root-, i.e. the '/' directory, instead these were given: " ++ show rootImgs)
  where
    mountLoopback (imgPath, containerMountPoint) = do
      let hostMountPoint =
            containerLoopbackMountRoot containerDirs
              </> printHash (imgPath, containerMountPoint)
      liftIO $ createDirectoryIfMissing True hostMountPoint
      hostCmd (sudo (printf "mount -o loop '%s' '%s'" imgPath hostMountPoint)) timeoutFastCmd
      return
        ( LoopbackMount
            { loopbackHost = hostMountPoint,
              loopbackContainer = containerMountPoint
            }
        )

newtype ContainerRootImage
  = ContainerRootImage FilePath
  deriving (Show)

data ContainerMounts
  = ContainerMounts
      { containerRootImage :: Either ContainerRootImage LoopbackMount,
        containerLoopbackMounts :: [LoopbackMount]
      }
  deriving (Show)

data LoopbackMount = LoopbackMount {loopbackHost :: FilePath, loopbackContainer :: FilePath}
  deriving (Show)

prepareBootScript ::
  (Member ExcB9 e, CommandIO e) =>
  ContainerBuildDirectories ->
  Script ->
  Eff e BootScript
prepareBootScript containerDirs script = do
  let bs =
        BootScript
          { bootScriptHostDir = containerBuildRoot containerDirs </> "boot_script",
            bootScriptContainerDir = "/mnt/boot_script",
            bootScriptContainerCommand = bootScriptContainerDir bs </> scriptFile
          }
      scriptFile = "run.sh"
      scriptEnv =
        Begin
          [ Run "export" ["HOME=/root"],
            Run "export" ["USER=root"],
            -- IgnoreErrors True [Run "source" ["/etc/profile"]],
            script
          ]
  liftIO $ do
    createDirectoryIfMissing True (bootScriptHostDir bs)
    writeSh (bootScriptHostDir bs </> scriptFile) scriptEnv
  traceL ("wrote script: \n" ++ show scriptEnv)
  traceL ("created boot-script: " ++ show bs)
  return bs

data BootScript
  = BootScript
      { bootScriptHostDir :: FilePath,
        bootScriptContainerDir :: FilePath,
        bootScriptContainerCommand :: String
      }
  deriving (Show)

execBuild ::
  (Member ExcB9 e, Member BuildInfoReader e, CommandIO e) =>
  SudoPrepender ->
  ContainerMounts ->
  [SharedDirectory] ->
  BootScript ->
  SystemdNspawnConfig ->
  Eff e Bool
execBuild sudo containerMounts sharedDirs bootScript dCfg = do
  let systemdCmd =
        unwords
          ( systemdNspawnExe
              ++ consoleOptions
              ++ rootImageOptions
              ++ capabilityOptions
              ++ bindMounts
              ++ extraArgs
              ++ execOptions
          )
      systemdNspawnExe =
        [fromMaybe "systemd-nspawn" (_systemdNspawnExecutable dCfg)]
      consoleOptions =
        ["--console=" ++ show (_systemdNspawnConsole dCfg)]
      rootImageOptions =
        case containerRootImage containerMounts of
          Left (ContainerRootImage imgPath) ->
            ["-i", imgPath]
          Right loopbackMounted ->
            ["-D", loopbackHost loopbackMounted]
      capabilityOptions =
        case _systemdNspawnCapabilities dCfg of
          [] -> []
          caps -> ["--capability=" ++ intercalate "," (map show caps)]
      bindMounts =
        map mkBind loopbackMounts
          ++ map mkBind sharedDirMounts
          ++ map mkBindRo sharedDirMountsRo
          ++ [mkBindRo (bootScriptHostDir bootScript, bootScriptContainerDir bootScript)]
        where
          mkBind (hostDir, containerDir) = "--bind=" ++ hostDir ++ ":" ++ containerDir
          mkBindRo (hostDir, containerDir) = "--bind-ro=" ++ hostDir ++ ":" ++ containerDir
          loopbackMounts =
            [ (h, c)
              | LoopbackMount {loopbackHost = h, loopbackContainer = c} <-
                  containerLoopbackMounts containerMounts
            ]
          sharedDirMounts = [(h, c) | SharedDirectory h (MountPoint c) <- sharedDirs]
          sharedDirMountsRo = [(h, c) | SharedDirectoryRO h (MountPoint c) <- sharedDirs]
      extraArgs = maybe [] (: []) (_systemdNspawnExtraArgs dCfg)
      execOptions = ["/bin/sh", bootScriptContainerCommand bootScript]
      timeout = (TimeoutMicros . (* 1000000)) <$> _systemdNspawnMaxLifetimeSeconds dCfg
  traceL ("executing systemd-nspawn container build")
  interactiveAction <- isInteractive
  let 
    runInteractively =
      case _systemdNspawnConsole dCfg of
        SystemdNspawnInteractive ->
          True
        _ ->
          interactiveAction
  if runInteractively 
    then
      hostCmdStdIn HostCommandInheritStdin (sudo systemdCmd) Nothing
    else  
      hostCmd (sudo systemdCmd) timeout

umountLoopbackImages ::
  forall e.
  (Member ExcB9 e, CommandIO e) =>
  SudoPrepender ->
  ContainerMounts ->
  Eff e ()
umountLoopbackImages sudo c = do
  case containerRootImage c of
    Left _ -> return ()
    Right r -> umount r
  traverse_ umount (containerLoopbackMounts c)
  where
    umount :: LoopbackMount -> Eff e ()
    umount l = do
      traceL $ "unmounting: " ++ show l
      res <- hostCmd (sudo (printf "umount '%s'" (loopbackHost l))) timeoutFastCmd
      when (not res) (errorL ("failed to unmount: " ++ show l))

removeContainerBuildRootDir ::
  forall e.
  (Member ExcB9 e, CommandIO e) =>
  SudoPrepender ->
  ContainerBuildDirectories ->
  Eff e ()
removeContainerBuildRootDir sudo containerBuildDirs = do
  let target = containerBuildRoot containerBuildDirs
  traceL $ "removing: " ++ target
  res <- hostCmd (sudo (printf "rm -rf '%s'" target)) timeoutFastCmd
  when (not res) (errorL ("failed to remove: " ++ target))

timeoutFastCmd :: Maybe Timeout
timeoutFastCmd = Just (TimeoutMicros 10000000)
