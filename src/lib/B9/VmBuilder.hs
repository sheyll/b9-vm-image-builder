-- | Effectful functions to execute and build virtual machine images using
--    an execution environment like e.g. libvirt-lxc.
module B9.VmBuilder
  ( buildWithVm,
    buildWithVmPostFix
  )
where

import B9.Artifact.Readable
import B9.B9Error
import B9.B9Logging
import B9.B9Monad
import B9.BuildInfo
import B9.Container
import B9.DiskImageBuilder
import B9.DiskImages
import qualified B9.Docker as Docker
import B9.ExecEnv
import qualified B9.LibVirtLXC as LXC
import qualified B9.SystemdNspawn as SystemdNspawn
import B9.Vm
import Control.Eff
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Proxy
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
  )
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

buildWithVm ::
  IsB9 e => InstanceId -> [ImageTarget] -> FilePath -> VmScript -> Eff e Bool
buildWithVm iid imageTargets instanceDir vmScript = buildWithVmImpl iid imageTargets instanceDir vmScript NoVmScript

buildWithVmPostFix ::
  IsB9 e => InstanceId -> [ImageTarget] -> FilePath -> VmScript -> VmScript -> Eff e Bool
buildWithVmPostFix = buildWithVmImpl

buildWithVmImpl ::
  IsB9 e => InstanceId -> [ImageTarget] -> FilePath -> VmScript -> VmScript -> Eff e Bool
buildWithVmImpl iid imageTargets instanceDir vmScript postFix = do
  res <- withBackend (buildWithBackend iid imageTargets instanceDir vmScript postFix)
  case res of
    Nothing ->
      errorExitL "No container configured."
    Just success ->
      return success

buildWithBackend :: forall backendCfg e. (Backend backendCfg, IsB9 e) => InstanceId -> [ImageTarget] -> FilePath -> VmScript -> VmScript -> backendCfg -> Eff e Bool
buildWithBackend iid imageTargets instanceDir vmScript postFix backendCfg = do
  let vmBuildSupportedImageTypes = supportedImageTypes (Proxy :: Proxy backendCfg)
  buildImages <- createBuildImages imageTargets vmBuildSupportedImageTypes
  success1 <- runVmScript backendCfg iid imageTargets buildImages instanceDir vmScript
  when success1 (resizeDestinationImages buildImages imageTargets)
  success2 <- runVmScript backendCfg iid imageTargets buildImages instanceDir postFix
  when success2 (exportDestinationImages buildImages imageTargets)
  return (success1 && success2)

createBuildImages :: IsB9 e => [ImageTarget] -> [ImageType] -> Eff e [Image]
createBuildImages imageTargets vmBuildSupportedImageTypes = do
  dbgL "creating build images"
  traceL (ppShow imageTargets)
  buildImages <- mapM createBuildImage imageTargets
  infoL "CREATED BUILD IMAGES"
  traceL (ppShow buildImages)
  return buildImages
  where
    createBuildImage (ImageTarget dest imageSource _mnt) = do
      buildDir <- getBuildDir
      destTypes <- preferredDestImageTypes imageSource
      let buildImgType =
            head
              ( destTypes
                  `intersect` preferredSourceImageTypes dest
                  `intersect` vmBuildSupportedImageTypes
              )
      srcImg <- resolveImageSource imageSource
      let buildImg =
            changeImageFormat buildImgType (changeImageDirectory buildDir srcImg)
      buildImgAbsolutePath <- ensureAbsoluteImageDirExists buildImg
      materializeImageSource imageSource buildImg
      return buildImgAbsolutePath

runVmScript ::
  forall e backendCfg.
  (Backend backendCfg, IsB9 e) =>
  backendCfg ->
  InstanceId ->
  [ImageTarget] ->
  [Image] ->
  FilePath ->
  VmScript ->
  Eff e Bool
runVmScript _ _ _ _ _ NoVmScript = return True
runVmScript backendCfg (IID iid) imageTargets buildImages instanceDir vmScript = do
  dbgL (printf "starting vm script with instanceDir '%s'" instanceDir)
  traceL (ppShow vmScript)
  execEnv <- setUpExecEnv
  let (VmScript _ _ script) = vmScript
  result <- runExcB9 $ runInEnvironment backendCfg execEnv script
  handleErrors (either (Left . show) Right result)
  where
    handleErrors :: IsB9 e => Either String Bool -> Eff e Bool
    handleErrors (Right False) = do
      errorL "The containerized build failed!"
      return False
    handleErrors (Right True) = do
      traceL "The containerized build was successful."
      return True
    handleErrors (Left err) =
      errorExitL ("Failed to complete the containerized build: " ++ show err)
    setUpExecEnv :: IsB9 e => Eff e ExecEnv
    setUpExecEnv = do
      let (VmScript cpu shares _) = vmScript
      let mountedImages = buildImages `zip` (itImageMountPoint <$> imageTargets)
      sharesAbs <- createSharedDirs instanceDir shares
      return
        (ExecEnv iid mountedImages sharesAbs (Resources AutomaticRamSize 8 cpu))

createSharedDirs ::
  IsB9 e => FilePath -> [SharedDirectory] -> Eff e [SharedDirectory]
createSharedDirs instanceDir = mapM createSharedDir
  where
    createSharedDir (SharedDirectoryRO d m) = do
      d' <- createAndCanonicalize d
      return $ SharedDirectoryRO d' m
    createSharedDir (SharedDirectory d m) = do
      d' <- createAndCanonicalize d
      return $ SharedDirectory d' m
    createSharedDir (SharedSources mp) = do
      d' <- createAndCanonicalize instanceDir
      return $ SharedDirectoryRO d' mp
    createAndCanonicalize d = liftIO $ do
      createDirectoryIfMissing True d
      canonicalizePath d

resizeDestinationImages :: IsB9 e => [Image] -> [ImageTarget] -> Eff e ()
resizeDestinationImages buildImages imageTargets = do
  dbgL "resizing build images to the output images sizes"
  let pairsToConvert =
        buildImages `zip` (itImageDestination `map` imageTargets)
  traceL (ppShow pairsToConvert)
  mapM_ (uncurry resizeDestinationImage) pairsToConvert
  infoL "RESIZED BUILD- TO OUTPUT IMAGES"

exportDestinationImages :: IsB9 e => [Image] -> [ImageTarget] -> Eff e ()
exportDestinationImages buildImages imageTargets = do
  dbgL "converting build- to output images"
  let pairsToConvert =
        buildImages `zip` (itImageDestination `map` imageTargets)
  traceL (ppShow pairsToConvert)
  mapM_ (uncurry exportDestinationImage) pairsToConvert
  infoL "CONVERTED BUILD- TO OUTPUT IMAGES"

withBackend :: IsB9 e => (forall x. Backend x => x -> Eff e a) -> Eff e (Maybe a)
withBackend k = do
  lxcCfg <- getBackendConfig (Proxy :: Proxy LXC.LibVirtLXC)
  case lxcCfg of
    Just cfg ->
      Just <$> k cfg
    Nothing -> do
      dockerCfg <- getBackendConfig (Proxy :: Proxy Docker.Docker)
      case dockerCfg of
        Just cfg ->
          Just <$> k cfg
        Nothing -> do
          systemdNspawnCfg <- getBackendConfig (Proxy :: Proxy SystemdNspawn.SystemdNspawn)
          case systemdNspawnCfg of
            Just cfg ->
              Just <$> k cfg
            Nothing ->
              return Nothing
