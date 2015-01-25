module B9.Builder ( module B9.B9Monad
                  , module B9.ConfigUtils
                  , module B9.B9Config
                  , module B9.Project
                  , module B9.ExecEnv
                  , module B9.DiskImages
                  , module B9.DiskImageBuilder
                  , module B9.ShellScript
                  , module B9.Repository
                  , module B9.RepositoryIO
                  , module B9.ArtifactGenerator
                  , module B9.ArtifactGeneratorImpl
                  , buildProject
                  , generateConfig
                  , printProject
                  , runInEnvironment
                  , createBuildImages
                  , createSharedDirs
                  ) where
import Data.Data
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import Data.Generics.Schemes
import Data.Generics.Aliases
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)

import B9.B9Monad
import B9.ConfigUtils
import B9.B9Config
import B9.Project
import B9.ExecEnv
import B9.DiskImages
import B9.DiskImageBuilder
import B9.ShellScript
import B9.Repository
import B9.RepositoryIO
import B9.ArtifactGenerator
import B9.ArtifactGeneratorImpl
import qualified B9.LibVirtLXC as LXC

buildProject :: Project -> ConfigParser -> B9Config -> IO Bool
buildProject projectTemplate cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg -> do
    let project = substProject (envVars cfg) projectTemplate
    run (projectName project) cfgParser cfg $ do
      infoL "START BUILD"
      getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
      traceL $ printf "USING PROJECT TEMPLATE: %s" (ppShow projectTemplate)
      traceL $ printf "RESULTING IN PROJECT: %s" (ppShow project)
      buildImgs <- createBuildImages (projectDisks project)
      infoL "DISK IMAGES CREATED"
      acs <- assemble (projectArtifacts project)
      let sharedDirsCfg = sharedDirectoriesFromGeneratedArtifact acs
      infoL "CONFIG GENERATED"
      sharedDirsPrj <- createSharedDirs (projectSharedDirectories project)
      let execEnv = ExecEnv (projectName project)
                            mountedBuildImgs
                            (sharedDirsCfg ++ sharedDirsPrj)
                            (Resources AutomaticRamSize
                                       8
                                       (projectCpuArch project))
          mountedBuildImgs = zip buildImgs (itImageMountPoint
                                            <$> projectDisks project)
          script = projectBuildScript project
      success <- runInEnvironment execEnv script
      if success
        then do infoL "BUILD SCRIPT SUCCESSULLY EXECUTED IN CONTAINER"
                mapM_ (uncurry createDestinationImage)
                      (zip buildImgs (itImageDestination <$>
                                        (projectDisks project)))
                infoL "BUILD FINISHED"
                return True

        else do errorL "FAILED TO EXECUTE COMMANDS"
                return False

generateConfig :: Project -> ConfigParser -> B9Config -> IO Bool
generateConfig projectTemplate cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg -> do
    let project = substProject (envVars cfg) projectTemplate
    run (projectName project) cfgParser cfg $ do
      infoL "START CONFIG GENERATION"
      getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
      traceL $ printf "USING PROJECT TEMPLATE: %s" (ppShow projectTemplate)
      traceL $ printf "RESULTING IN PROJECT: %s" (ppShow project)
      assemble (projectArtifacts project)
      return True

printProject :: Project -> ConfigParser -> B9Config -> IO Bool
printProject projectTemplate cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg -> do
    let project = substProject (envVars cfg) projectTemplate
    putStrLn (printf "\n>>> Interpolated B9-config: \n%s\n\n"
                     (ppShow cfg))
    putStrLn (printf "\n>>> Effective project template: \n%s\n\n"
                     (ppShow projectTemplate))
    putStrLn (printf "\n>>> Effective project: \n%s\n\n"
                     (ppShow project))
    return True

createBuildImages :: [ImageTarget] -> B9 [Image]
createBuildImages disks = mapM create disks
  where
    create (ImageTarget dest imageSource _mnt) = do
      envType <- getExecEnvType
      buildDir <- getBuildDir
      destTypes <- preferredDestImageTypes imageSource
      let buildImgType = head (destTypes
                               `intersect`
                               preferredSourceImageTypes dest
                               `intersect`
                               (supportedImageTypes envType))
      srcImg <- resolveImageSource imageSource
      let buildImg = changeImageFormat buildImgType
                                       (changeImageDirectory buildDir srcImg)
      buildImgAbsolutePath <- liftIO (ensureAbsoluteImageDirExists buildImg)
      materializeImageSource imageSource buildImg
      return buildImgAbsolutePath

createSharedDirs :: [SharedDirectory] -> B9 [SharedDirectory]
createSharedDirs sharedDirsIn = mapM createSharedDir sharedDirsIn
  where
    createSharedDir (SharedDirectoryRO d m) = liftIO $ do
      createDirectoryIfMissing True d
      d' <- canonicalizePath d
      return $ SharedDirectoryRO d' m
    createSharedDir (SharedDirectory d m) = liftIO $ do
      createDirectoryIfMissing True d
      d' <- canonicalizePath d
      return $ SharedDirectory d' m

supportedImageTypes :: ExecEnvType -> [ImageType]
supportedImageTypes LibVirtLXC = LXC.supportedImageTypes

runInEnvironment :: ExecEnv -> Script -> B9 Bool
runInEnvironment env script = do
  t <- getExecEnvType
  case t of
   LibVirtLXC -> LXC.runInEnvironment env script

substProject :: [(String,String)] -> Project -> Project
substProject env p = everywhere gsubst p
  where gsubst :: forall a. Data a => a -> a
        gsubst = mkT substProject_
                  `extT` substMountPoint
                    `extT` substImage
                       `extT` substSharedDir
                         `extT` substScript
                           `extT` substImageSource
                             `extT` substDiskTarget
        substProject_ prj = prj { projectName = sub (projectName p)}

        substMountPoint NotMounted = NotMounted
        substMountPoint (MountPoint x) = MountPoint (sub x)

        substImage (Image fp t fs) = Image (sub fp) t fs

        substSharedDir (SharedDirectory fp mp) =
          SharedDirectory (sub fp) mp
        substSharedDir (SharedDirectoryRO fp mp) =
          SharedDirectoryRO (sub fp) mp

        substScript (In fp s) = In (sub fp) s
        substScript (Run fp args) = Run (sub fp) (map sub args)
        substScript (As fp s) = As (sub fp) s
        substScript s = s

        substImageSource (From n s) = From (sub n) s
        substImageSource (EmptyImage l f t s) = EmptyImage (sub l) f t s
        substImageSource s = s

        substDiskTarget (Share n t s) = Share (sub n) t s
        substDiskTarget s = s

        sub = subst env

withB9Config :: ConfigParser
             -> B9Config
             -> (B9Config -> IO Bool)
             -> IO Bool
withB9Config cfgParser cliCfg f = do
  let parsedCfg' = parseB9Config cfgParser
  case parsedCfg' of
    Left e -> do
      putStrLn (printf "B9 Failed to start: %s" e)
      return False
    Right parsedCfg ->
      let cfg = defaultB9Config <> parsedCfg <> cliCfg
          in f cfg
