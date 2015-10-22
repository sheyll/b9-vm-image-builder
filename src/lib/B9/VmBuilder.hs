{-| Effectful functions to execute and build virtual machine images using
    an execution environment like e.g. libvirt-lxc. -}
module B9.VmBuilder (buildWithVm) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Directory (createDirectoryIfMissing, canonicalizePath)
import           Text.Printf ( printf )
import           Text.Show.Pretty (ppShow)
import           B9.B9Monad
import           B9.DiskImages
import           B9.DiskImageBuilder
import           B9.ExecEnv
import           B9.B9Config
import           B9.Vm
import           B9.ArtifactGenerator
import           B9.ShellScript
import qualified B9.LibVirtLXC as LXC
import           Data.Maybe

buildWithVm :: InstanceId -> [ImageTarget] -> FilePath -> VmScript -> B9 Bool
buildWithVm iid imageTargets instanceDir vmScript = do
    vmBuildSupportedImageTypes <- getVmScriptSupportedImageTypes vmScript
    mBuildImages <- createBuildImages imageTargets vmBuildSupportedImageTypes
    case mBuildImages of
        Nothing -> do
            errorL "COULD NOT CREATE ALL BUILD IMAGES"
            return False
        Just buildImages -> do
            success <-
                runVmScript iid imageTargets buildImages instanceDir vmScript
            when success (createDestinationImages buildImages imageTargets)
            return success

getVmScriptSupportedImageTypes :: VmScript -> B9 [ImageType]
getVmScriptSupportedImageTypes NoVmScript =
  return [QCow2, Raw, Vmdk]
getVmScriptSupportedImageTypes _ = do
  envType <- getExecEnvType
  return (supportedImageTypes envType)

supportedImageTypes :: ExecEnvType -> [ImageType]
supportedImageTypes LibVirtLXC = LXC.supportedImageTypes

-- | Decide on a build image format.
selectBuildImgFormat
    :: (ImageType, FileSystem)
    -> Maybe (ImageType, FileSystem)
    -> [ImageType]
    -> [FileSystem]
    -> Maybe (ImageType, FileSystem)
selectBuildImgFormat src mdst supportedT supportedFS =
    listToMaybe $
    maybe id filterValidSources mdst $
    filterValidDestinations src $
    [(t, f) | t <- supportedT
            , f <- supportedFS]
  where
    filterValidDestinations (fmt,fs) = filter ((fmt, fs) `canConvertTo`)
    filterValidSources (fmt,fs) = filter (`canConvertTo` (fmt, fs))

createBuildImages :: [ImageTarget] -> [ImageType] -> B9 (Maybe [Image])
createBuildImages imageTargets vmBuildSupportedImageTypes = do
    dbgL "creating build images"
    traceL (ppShow imageTargets)
    buildImages <- sequenceA <$> mapM createBuildImage imageTargets
    infoL "CREATED BUILD IMAGES"
    traceL (ppShow buildImages)
    return buildImages
  where
    createBuildImage it@(ImageTarget imageDest imageSource _mnt) = do
        buildDir <- getBuildDir
        srcImg <- resolveImageSource imageSource
        case selectBuildImgFormat
                 (imageFormat srcImg)
                 (imageDestFormat imageDest)
                 vmBuildSupportedImageTypes
                 [Ext4] -- TODO make supported File systems configurable
              of
            Just (buildImgType, _buildImgFs) -> do -- TODO make use of
                                                   -- buildImgFs
                let buildImg =
                        changeImageFormat
                            buildImgType
                            (changeImageDirectory buildDir srcImg)
                buildImgAbsolutePath <-
                    liftIO (ensureAbsoluteImageDirExists buildImg)
                materializeImageSource imageSource buildImg
                return (Just buildImgAbsolutePath)
            Nothing -> do
                errorL ("CANNOT CONVERT TO BUILD IMAGE: " ++ ppShow it)
                return Nothing

runVmScript :: InstanceId
            -> [ImageTarget]
            -> [Image]
            -> FilePath
            -> VmScript
            -> B9 Bool
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
    setUpExecEnv :: B9 ExecEnv
    setUpExecEnv = do
      let (VmScript cpu shares _) = vmScript
      let mountedImages = buildImages `zip` (itImageMountPoint <$> imageTargets)
      sharesAbs <- createSharedDirs instanceDir shares
      return (ExecEnv iid
                      mountedImages
                      sharesAbs
                      (Resources AutomaticRamSize 8 cpu))

createSharedDirs :: FilePath -> [SharedDirectory] -> B9 [SharedDirectory]
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

createDestinationImages :: [Image] -> [ImageTarget] -> B9 ()
createDestinationImages buildImages imageTargets = do
  dbgL "converting build- to output images"
  let pairsToConvert = buildImages `zip` (itImageDestination `map` imageTargets)
  traceL (ppShow pairsToConvert)
  mapM_ (uncurry createDestinationImage) pairsToConvert
  infoL "CONVERTED BUILD- TO OUTPUT IMAGES"

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env script = do
  t <- getExecEnvType
  case t of
   LibVirtLXC -> LXC.runInEnvironment env script
