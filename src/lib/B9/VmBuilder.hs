{-| Effectful functions to execute and build virtual machine images using
    an execution environment like e.g. libvirt-lxc. -}
module B9.VmBuilder
  ( buildWithVm
  ) where

import Control.Eff
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

import B9.Artifact.Readable
import B9.B9Config
import B9.B9Logging
import B9.B9Monad
import B9.BuildInfo
import B9.DiskImageBuilder
import B9.DiskImages
import B9.ExecEnv
import qualified B9.LibVirtLXC as LXC
import B9.ShellScript
import B9.Vm

buildWithVm :: IsB9 e => InstanceId -> [ImageTarget] -> FilePath -> VmScript -> Eff e Bool
buildWithVm iid imageTargets instanceDir vmScript = do
  vmBuildSupportedImageTypes <- getVmScriptSupportedImageTypes vmScript
  buildImages <- createBuildImages imageTargets vmBuildSupportedImageTypes
  success <- runVmScript iid imageTargets buildImages instanceDir vmScript
  when success (createDestinationImages buildImages imageTargets)
  return success

getVmScriptSupportedImageTypes :: IsB9 e => VmScript -> Eff e [ImageType]
getVmScriptSupportedImageTypes NoVmScript = return [QCow2, Raw, Vmdk]
getVmScriptSupportedImageTypes _ = supportedImageTypes <$> getExecEnvType

supportedImageTypes :: ExecEnvType -> [ImageType]
supportedImageTypes LibVirtLXC = LXC.supportedImageTypes

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
            head (destTypes `intersect` preferredSourceImageTypes dest `intersect` vmBuildSupportedImageTypes)
      srcImg <- resolveImageSource imageSource
      let buildImg = changeImageFormat buildImgType (changeImageDirectory buildDir srcImg)
      buildImgAbsolutePath <- ensureAbsoluteImageDirExists buildImg
      materializeImageSource imageSource buildImg
      return buildImgAbsolutePath

runVmScript ::
     forall e. IsB9 e
  => InstanceId
  -> [ImageTarget]
  -> [Image]
  -> FilePath
  -> VmScript
  -> Eff e Bool
runVmScript _ _ _ _ NoVmScript = return True
runVmScript (IID iid) imageTargets buildImages instanceDir vmScript = do
  dbgL (printf "starting vm script with instanceDir '%s'" instanceDir)
  traceL (ppShow vmScript)
  execEnv <- setUpExecEnv
  let (VmScript _ _ script) = vmScript
  success <- runInEnvironment execEnv script
  if success
    then infoL "EXECUTED BUILD SCRIPT"
    else errorL "BUILD SCRIPT FAILED"
  return success
  where
    setUpExecEnv :: IsB9 e => Eff e ExecEnv
    setUpExecEnv = do
      let (VmScript cpu shares _) = vmScript
      let mountedImages = buildImages `zip` (itImageMountPoint <$> imageTargets)
      sharesAbs <- createSharedDirs instanceDir shares
      return (ExecEnv iid mountedImages sharesAbs (Resources AutomaticRamSize 8 cpu))

createSharedDirs :: IsB9 e => FilePath -> [SharedDirectory] -> Eff e [SharedDirectory]
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
    createAndCanonicalize d =
      liftIO $ do
        createDirectoryIfMissing True d
        canonicalizePath d

createDestinationImages :: IsB9 e => [Image] -> [ImageTarget] -> Eff e ()
createDestinationImages buildImages imageTargets = do
  dbgL "converting build- to output images"
  let pairsToConvert = buildImages `zip` (itImageDestination `map` imageTargets)
  traceL (ppShow pairsToConvert)
  mapM_ (uncurry createDestinationImage) pairsToConvert
  infoL "CONVERTED BUILD- TO OUTPUT IMAGES"

runInEnvironment :: IsB9 e => ExecEnv -> Script -> Eff e Bool
runInEnvironment env script = do
  t <- getExecEnvType
  case t of
    LibVirtLXC -> LXC.runInEnvironment env script