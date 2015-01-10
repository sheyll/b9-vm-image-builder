module B9.Builder ( module B9.B9Monad
                  , module B9.ConfigUtils
                  , module B9.B9Config
                  , module B9.Project
                  , module B9.ExecEnv
                  , module B9.DiskImages
                  , module B9.ShellScript
                  , buildProject
                  , runInEnvironment
                  , createBuildImages
                  , createSharedDirs
                  ) where

import           Control.Applicative ( (<$>) )
import           Control.Exception ( bracket )
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( liftIO )
import           Data.List ( nub )
import           Data.Maybe ( isJust, fromJust, catMaybes )
import           Data.Word ( Word32 )
import System.Directory ( createDirectoryIfMissing
                        , createDirectory
                        , setCurrentDirectory
                        , getCurrentDirectory
                        , canonicalizePath
                        , renameFile
                        , removeFile
                        , copyFile
                        , removeDirectoryRecursive
                        )
import System.Exit ( exitWith
                   , ExitCode (..) )
import System.FilePath ( takeDirectory
                       , takeFileName
                       , replaceExtension
                       , (</>)
                       , (<.>) )
import           System.Process ( callCommand )
import           System.Random ( randomIO )
import           Text.Printf ( printf )

import           B9.B9Monad
import           B9.ConfigUtils
import           B9.B9Config
import           B9.Project
import           B9.ExecEnv
import           B9.DiskImages
import           B9.ShellScript
import qualified B9.LibVirtLXC as LXC

buildProject :: Project -> ConfigParser -> B9Config -> [String] -> IO Bool
buildProject p cfgParser cliCfg args =
  run (projectName p) cfgParser cliCfg args $ do
  infoL "START BUILD"
  buildImgs <- createBuildImages (projectDisks p)
  infoL "DISK IMAGES CREATED"
  sharedDirs <- createSharedDirs (projectSharedDirectories p)

  let execEnv = ExecEnv (projectName p) buildImgs sharedDirs (projectResources p)
      script = projectBuildScript p

  success <- runInEnvironment execEnv script
  if success
    then do infoL "BUILD SCRIPT SUCCESSULLY EXECUTED IN CONTAINER"
            exported <- mapM exportImage (zip buildImgs (projectDisks p))
            when (not (null (catMaybes exported)))
              (infoL $ "DISK IMAGES SUCCESSFULLY EXPORTED")
            infoL "BUILD FINISHED"
            return True

    else do errorL "FAILED TO EXECUTE COMMANDS"
            return False
  where
    exportImage ((imgI, _), (Export imgO@(Image imgOFile _) _, _)) = do
      liftIO $ createDirectoryIfMissing True $ takeDirectory imgOFile
      convert True imgI imgO
      return $ Just imgOFile
    exportImage _ = return Nothing

createBuildImages :: [Mounted DiskTarget] -> B9 [Mounted Image]
createBuildImages disks = mapM create $ zip [0..] disks
  where
    supportedImageTypes LibVirtLXC = LXC.supportedImageTypes
    create (diskIndex, (disk, m)) = do
      buildDir <- getBuildDir
      envType <- getExecEnvType
      let (src, dest) = case disk of
                         Export dest'@(Image _ destFmt') src ->
                           let dest = changeImageDirectory buildDir
                                      $ changeImageFormat destFmt dest'
                               srcCompatible = compatibleImageTypes src
                               destFmt = head
                                         $ filter (`elem` allowedTypes)
                                         $ filter (`elem` srcCompatible)
                                         $ nub
                                         $ destFmt' : srcCompatible
                           in (src, dest)

                         Transient src ->
                           let dest = Image destFile destFmt
                               destFile = buildDir
                                          </> ("disk_" ++ show diskIndex)
                                          <.> (show destFmt)
                               destFmt = head
                                         $ filter (`elem` allowedTypes)
                                         $ compatibleImageTypes src
                           in (src, dest)
          allowedTypes = supportedImageTypes envType
      srcAbs <- liftIO $ ensureAbsoluteImageSourceDirExists src
      destAbs <- liftIO $ ensureAbsoluteImageDirExists dest
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
  execEnvType <- getExecEnvType
  case execEnvType of
   LibVirtLXC -> LXC.runInEnvironment env script
