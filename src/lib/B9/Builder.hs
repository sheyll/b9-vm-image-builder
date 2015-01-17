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
                  , buildProject
                  , printProject
                  , runInEnvironment
                  , createBuildImages
                  , createSharedDirs
                  ) where
import Data.Data
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Data.List ( nub )
import Data.Generics.Schemes
import Data.Generics.Aliases
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.FilePath
import Text.Printf ( printf )
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
import qualified B9.LibVirtLXC as LXC
import Text.Show.Pretty (ppShow)

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
      sharedDirs <- createSharedDirs (projectSharedDirectories project)

      let execEnv = ExecEnv (projectName project)
                            buildImgs
                            sharedDirs
                            (projectResources project)
          script = projectBuildScript project

      success <- runInEnvironment execEnv script
      if success
        then do infoL "BUILD SCRIPT SUCCESSULLY EXECUTED IN CONTAINER"
                mapM_ export (zip buildImgs (projectDisks project))
                infoL "BUILD FINISHED"
                return True

        else do errorL "FAILED TO EXECUTE COMMANDS"
                return False
      where
        export ((imgI, _), (Export imgO _, _)) =
          exportImage imgI imgO
        export ((imgI, _), (Share name _ _, _)) =
          void (shareImage imgI (SharedImageName name))
        export _ = return ()

createBuildImages :: [Mounted DiskTarget] -> B9 [Mounted Image]
createBuildImages disks = mapM create $ zip [0..] disks
  where
    supportedImageTypes LibVirtLXC = LXC.supportedImageTypes
    create (diskIndex, (disk, m)) = do
      buildDir <- getBuildDir
      envType <- getExecEnvType
      let (src, dest) =
            case disk of
             Share biName biDestFmt biSrc ->
               let biDest = Image biDestFile biDestFmt
                   biDestFile = buildDir
                                </> biName
                                <.> (show biDestFmt)
               in (biSrc, biDest)

             Export dest'@(Image _ destFmt') expSrc ->
               let expDest = changeImageDirectory buildDir
                             $ changeImageFormat expDestFmt dest'
                   srcCompatible = compatibleImageTypes src
                   expDestFmt = head
                                $ filter (`elem` allowedTypes)
                                $ filter (`elem` srcCompatible)
                                $ nub
                                $ destFmt' : srcCompatible
               in (expSrc, expDest)

             Transient tSrc ->
               let tDest = Image tDestFile tDestFmt
                   tDestFile = buildDir
                               </> ("disk_" ++ show diskIndex)
                               <.> (show tDestFmt)
                   tDestFmt = head
                              $ filter (`elem` allowedTypes)
                              $ compatibleImageTypes src
               in (tSrc, tDest)
          allowedTypes = supportedImageTypes envType
      srcAbs <- liftIO (ensureAbsoluteImageSourceDirExists src)
      destAbs <- liftIO (ensureAbsoluteImageDirExists dest)
      createdImg <- createImage srcAbs destAbs
      return (createdImg, m)

createSharedDirs :: [SharedDirectory] -> B9 [SharedDirectory]
createSharedDirs sharedDirsIn = mapM createSharedDir sharedDirsIn
  where
    createSharedDir (SharedDirectory d m) = liftIO $ do
      createDirectoryIfMissing True d
      d' <- canonicalizePath d
      return $ SharedDirectory d' m

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
                    `extT` substDiskImage
                       `extT` substSharedDir
                         `extT` substScript
                           `extT` substImageSource
                             `extT` substDiskTarget
        substProject_ prj = prj { projectName = sub (projectName p)}

        substMountPoint (MountPoint x) = MountPoint (sub x)

        substDiskImage (Image fp t) = Image (sub fp) t

        substSharedDir (SharedDirectory fp mp) =
          SharedDirectory (sub fp) mp

        substScript (In fp s) = In (sub fp) s
        substScript (Run fp args) = Run (sub fp) (map sub args)
        substScript (As fp s) = As (sub fp) s
        substScript s = s

        substImageSource (From n s) = From (sub n) s
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
