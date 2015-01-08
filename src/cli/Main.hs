module Main where

import B9

main :: IO ()
main = do
  let p = testProject2
      cfg = defaultB9Config
      args = []
  success <- buildProject p cfg args
  when (not success) $ exitWith (ExitFailure 128)

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
