-- | Implementation of an execution environment that uses /systemdNspawn/.
module B9.SystemdNspawn
  ( SystemdNspawn (..),
  )
where

import B9.B9Config
  ( ContainerCapability,
    getB9Config,
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

newtype SystemdNspawn = SystemdNspawn SystemdNspawnConfig

instance Backend SystemdNspawn where
  getBackendConfig _ =
    fmap SystemdNspawn . view systemdNspawnConfigs <$> getB9Config

  supportedImageTypes _ = [Raw]

  runInEnvironment (SystemdNspawn dCfg) env scriptIn =
    if emptyScript scriptIn
      then return True
      else do
        containerMounts <- prepareContainerBuild env
        finallyB9
          ( do
              bootScript <- prepareBootScript containerMounts scriptIn
              execBuild containerMounts bootScript dCfg
          )
          (cleanupContainerBuild containerMounts)

prepareContainerBuild ::
  (Member BuildInfoReader e, Member ExcB9 e, CommandIO e) =>
  ExecEnv ->
  Eff e ContainerMounts
prepareContainerBuild e = do
  -- Create the directories etc 
  let imgMounts = envImageMounts e
  case filter ((== MountPoint "/") . snd) imgMounts of
    [(rootImg,_)] ->     
      error "TODO"
    [] ->
      throwB9Error "A containerized build requires that a disk image for the root-, i.e. the '/' directory is configured."     
    rootImgs ->
      throwB9Error ("A containerized build requires that only one disk image for the root-, i.e. the '/' directory, instead these were given: " ++ show rootImgs)
     
      

prepareBootScript :: 
  (Member ExcB9 e, CommandIO e) => 
  ContainerMounts -> 
  Script -> 
  Eff e BootScript
prepareBootScript = error "TODO"

execBuild :: 
  (Member ExcB9 e, CommandIO e) => 
  ContainerMounts -> 
  BootScript -> 
  w -> 
  Eff e Bool
execBuild containerMounts bootScript dCfg = error "TODO"

cleanupContainerBuild :: CommandIO e => ContainerMounts -> Eff e ()
cleanupContainerBuild = error "TODO"

data BootScript = BootScript

data ContainerMounts
  = MkContainerMounts
      { containerRootImage :: Either LoopbackMount Image,
        containerLoopbackMounts :: [LoopbackMount]
      }
  deriving (Show)

newtype LoopbackMount = LoopbackMount (Mounted (Mounted Image))
  deriving (Show)

loopbackMount :: FilePath -> FilePath -> Image -> LoopbackMount
loopbackMount containerMountPoint hostLoopbackMountPoint rawImg =
  LoopbackMount ((rawImg, MountPoint hostLoopbackMountPoint), MountPoint containerMountPoint)

rootLoopbackMount :: FilePath -> Image -> LoopbackMount
rootLoopbackMount hostLoopbackMountPoint rawImg =
  loopbackMount "/" hostLoopbackMountPoint rawImg
