{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Applicative ( (<$>) )
import           Control.Exception ( bracket )
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( liftIO )
import           Data.List ( nub )
import           Data.Maybe ( isJust, fromJust )
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
import           B9.B9Config
import           B9.Project
import           B9.ExecEnv
import           B9.DiskImages
import           B9.ShellScript
import qualified B9.LibVirtLXC as LXC

testImageArchlinux64 = Image "../archlinux_x86_64_2014.12.01.raw" Raw
testImageFedora32 = Image "../Fedora-i386-20-20131211.1-sda.qcow2" QCow2

testProject1 = Project
               { projectName = "test-b7-1"
               , projectDisks =
                 [ ( Export (Image "/home/sven/tmp/b7-tests/fedora.raw" Raw)
                     (SourceImage testImageFedora32 (Partition 1) KeepSize)
                   , MountPoint "/")
                 , ( Transient (SourceImage testImageFedora32 (Partition 1) KeepSize)
                   , MountPoint "/mnt/test1" )
                 , ( Transient
                     (CopyOnWrite testImageArchlinux64)
                   , MountPoint "/mnt/test2" )
                 , ( Export (Image "/home/sven/tmp/b7-tests/archBacked.vmdk" Vmdk)
                     (CopyOnWrite testImageArchlinux64)
                   , MountPoint "/mnt/test3" )
                 , ( Transient (FileSystem Ext4 (DiskSize 32 MB))
                   , MountPoint "/mnt/test4" )
                 , ( Export (Image "/home/sven/tmp/b7-tests/testEmpty.vmdk" Vmdk)
                     (FileSystem Ext4 (DiskSize 64 MB))
                   , MountPoint "/mnt/test5" ) ]
               , projectSharedDirectories =
                 [ SharedDirectory "." (MountPoint "/home/beqemu/Test-WORKSPACE") ]
               , projectBuildScript =
                 Begin [ Run "/usr/bin/mount" []
                       , Run "/usr/bin/df" ["-h"]
                       ]
               , projectResources = Resources { maxMemory = RamSize 8 GB
                                              , cpuCount = 4
                                              , cpuArch = I386
                                              , networkId = Nothing
                                              }
               }

archBuildImg = Image "/home/sven/.beqemu/machines/\
                     \svox-pico_builder-archlinux-mem2048-smp4\
                     \/archlinux_devel_base_img/img.qcow2" QCow2

testProject2 = Project
               { projectName = "test-b7-2"
               , projectDisks =
                 [(Transient (CopyOnWrite archBuildImg), MountPoint "/")]
               , projectSharedDirectories =
                 [ SharedDirectory
                   "/home/sven/MRF/mrf_third_party/custom-pkgs/lbm_pjproject"
                   (MountPoint "/home/beqemu/BASE")
                 , SharedDirectory
                   "./OUT"
                   (MountPoint "/home/beqemu/OUT")
                 ]
               , projectBuildScript =
                   Verbosity OnlyStdErr
                   [ Run "dhcpcd" []
                   , As "beqemu"
                     [ In "/home/beqemu"
                       [ Run "mkdir" ["-p", "build"] ]
                     , In "/home/beqemu/BASE"
                       [ Run "cp" [ "archlinux/PKGBUILD"
                                  , "archlinux/QBUILD"
                                  , "backtrace.patch"
                                  , "/home/beqemu/build" ] ]
                     , In "/home/beqemu/build"
                       [ Run "./QBUILD" ["2.1.0", "3"]
                       , Run "mv" ["*.pkg.*", "/home/beqemu/OUT"] ]
                     ]
                   ]
               , projectResources = Resources { maxMemory = RamSize 8 GB
                                              , cpuCount = 4
                                              , cpuArch = X86_64
                                              , networkId = (Just "host_bridge_br0")
                                              }
               }

testImageArchDevelBase = Image "../archlinux_devel_base_img.qcow2" QCow2

testImageArchQCow2 = Image "/home/sven/tmp/ArchXXX.qcow2" QCow2

testImageArchVmdk = Image "/home/sven/tmp/ArchXXX.vmdk" Vmdk

testProject3 = Project
               { projectName = "testProject3"
               , projectDisks =
                 [ ( Export testImageArchQCow2
                     (SourceImage testImageArchDevelBase NoPT KeepSize)
                   , MountPoint "/") ]
               , projectSharedDirectories =  [ ]
               , projectBuildScript =
                 Verbosity Debug
                 [ Run "dhcpcd" []
                 , Run "ls" ["-la", "/"] ]
               , projectResources = Resources { maxMemory = RamSize 8 GB
                                              , cpuCount = 4
                                              , cpuArch = X86_64
                                              , networkId = Nothing
                                              }
               }

main :: IO ()
main = do
  let p = testProject2
      name = projectName p
      cfg = defaultB9Config
      args = []
  success <- run name cfg args $ buildProject p
  when (not success) $ exitWith (ExitFailure 128)

buildProject :: Project -> B9 Bool
buildProject p = do
  infoL "START BUILD"
  buildImgs <- createBuildImages (projectDisks p)
  infoL "IMAGES CREATED"
  sharedDirs <- createSharedDirs (projectSharedDirectories p)
  let execEnv = ExecEnv (projectName p) buildImgs sharedDirs (projectResources p)
      script = projectBuildScript p
  execEnvType <- getExecEnvType
  success <- runInEnvironment execEnvType execEnv script
  if success
    then do infoL "COMMANDS EXECUTED"
            mapM_ exportImage (zip buildImgs (projectDisks p))
            infoL "BUILD FINISHED"
            return True

    else do errorL "FAILED TO EXECUTE COMMANDS"
            return False
  where
    exportImage ((imgI, _), (Export imgO@(Image imgOFile _) _, _)) = do
      liftIO $ createDirectoryIfMissing True $ takeDirectory imgOFile
      convert True imgI imgO
    exportImage _ = return ()

createBuildImages :: [Mounted DiskTarget] -> B9 [Mounted Image]
createBuildImages disks = mapM create $ zip [0..] disks
  where
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

supportedImageTypes :: ExecEnvType -> [ImageType]
supportedImageTypes LibVirtLXC = LXC.supportedImageTypes

runInEnvironment :: ExecEnvType -> ExecEnv -> Script -> B9 Bool
runInEnvironment LibVirtLXC = LXC.runInEnvironment
