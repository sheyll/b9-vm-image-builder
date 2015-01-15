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
                  , module B9.ImageShareing
                  , buildProject
                  , showProject
                  , runInEnvironment
                  , createBuildImages
                  , createSharedDirs
                  ) where
import Data.Data
import Data.Monoid
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( liftIO )
import           Data.List ( nub )
import Data.Generics.Schemes
import Data.Generics.Aliases
import Data.Maybe ( catMaybes )
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.FilePath ( takeDirectory
                       , (</>)
                       , (<.>) )
import Text.Printf ( printf )
import B9.B9Monad
import B9.ConfigUtils
import B9.B9Config
import B9.Project
import B9.ExecEnv
import B9.DiskImages
import B9.DiskImageBuilder
import B9.ImageShareing
import B9.ShellScript
import B9.Repository
import B9.RepositoryIO
import qualified B9.LibVirtLXC as LXC
import Text.Show.Pretty (ppShow)

showProject :: Project -> ConfigParser -> B9Config -> IO Bool
showProject projectTemplate cfgParser cliCfg = do
  putStrLn $ printf "\n>>> Merged project template: \n%s\n\n\
                    \>>> Configuration merged from config file and cli: \n\n%s\n\n\
                    \>>> Interpolated project:  \n\n%s\n\n"
                    (ppShow projectTemplate)
                    (ppShow cfg)
                    (ppShow project)
  return True
  where
    cfg = parseB9Config cfgParser <> cliCfg
    project = substProject (envVars cfg) projectTemplate

buildProject :: Project -> ConfigParser -> B9Config -> IO Bool
buildProject projectTemplate cfgParser cliCfg =
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
    cfg = parseB9Config cfgParser <> cliCfg

    project = substProject (envVars cfg) projectTemplate

    export ((imgI, _), (Export imgO _, _)) = exportImage imgI imgO
    export ((imgI, _), (Share info _, _)) = shareImage imgI info
    export _ = return ()

createBuildImages :: [Mounted DiskTarget] -> B9 [Mounted Image]
createBuildImages disks = mapM create $ zip [0..] disks
  where
    supportedImageTypes LibVirtLXC = LXC.supportedImageTypes
    create (diskIndex, (disk, m)) = do
      buildDir <- getBuildDir
      envType <- getExecEnvType
      let (src, dest) = case disk of
                         Share (ImageInfo biName) biSrc ->
                                let biDest = Image biDestFile biDestFmt
                                    biDestFile = buildDir
                                                 </> biName
                                                 <.> (show biDestFmt)
                                    biDestFmt = QCow2
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
      createImage srcAbs destAbs
      return (destAbs, m)

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
        substProject_ prj = prj { projectName = sub (projectName p)}

        substMountPoint (MountPoint x) = MountPoint (sub x)

        substDiskImage (Image fp t) = Image (sub fp) t

        substSharedDir (SharedDirectory fp mp) =
          SharedDirectory (sub fp) mp

        substScript (In fp s) = In (sub fp) s
        substScript (Run fp args) = Run (sub fp) (map sub args)
        substScript (As fp s) = As (sub fp) s
        substScript s = s

        sub = subst env
