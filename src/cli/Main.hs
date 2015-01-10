module Main where

import System.Environment
import Options.Applicative
import Options.Applicative.Help.Pretty
import B9

main :: IO ()
main = do
  opts <- parseCommandLine
  conf <- configure (configFile opts) (cliB9Config opts)
  prjs <- mapM load (projectFiles opts)
  code <- build (mconcat prjs) conf (cliB9Config opts) (extraArgs opts)
  exit code

exit success = when (not success) $ exitWith (ExitFailure 128)

data CliOpts = CliOpts { configFile :: Maybe SystemPath
                       , projectFiles :: [FilePath]
                       , extraArgs :: [String]
                       , cliB9Config :: B9Config
                       }

parseCommandLine :: IO CliOpts
parseCommandLine =
  execParser (info (helper <*> cliArgParser)
               (fullDesc
                <> progDesc "Build and run VM-Images inside LXC containers.\
                            \ Custom arguments follow after '--' and are\
                            \ accessable in many strings in project configs \
                            \ trough mustache templates, i.e. '{{N}}' referes to\
                            \ positional argument $N."
                <> headerDoc (Just helpHeader)))

helpHeader = linebreak <> b9AsciiArt <> linebreak
             <> text "B9 - a benign VM-Image build tool"

b9AsciiArt = string "\
\                      @!#?@!\n\
\               ,----, /\n\
\              /   9 9\\__\n\
\       B   E  |       _ \\   I   G   N\n\
\.  .  .  .  . \\      / \\N' .  .  .  .  .  .  .  .  .  .  .  .  .\n\
\ .   .   .   . `||-||.   .   .   .   .   .   .   .   .   .   .   .\n\
\   .    .    .  |L_|L_ .    .    .    .    .    .    .    .    .\n\
\=================================================================="

cliArgParser = toCliOpts
               <$> some (strOption
                         (help "A project to build, specify more than once to\
                                \ compose multiple projects"
                          <> short 'f'
                          <> long "project-file"
                          <> metavar "FILENAME"
                          <> noArgError (ErrorMsg "No project file specified!")))
               <*> optional (strOption
                             (help "Path to users b9-configuration"
                             <> short 'c'
                             <> long "configuration-file"
                             <> metavar "FILENAME"))
               <*> switch (help "Log everything that happens to stdout"
                             <> short 'v'
                             <> long "verbose")
               <*> switch (help "Suppress non-error output"
                             <> short 'q'
                             <> long "quiet")
               <*> optional (strOption
                             (help "Path to a logfile"
                             <> short 'l'
                             <> long "log-file"
                             <> metavar "FILENAME"))
               <*> optional (strOption
                             (help "Output file for a command/timing profile"
                             <> short 'p'
                             <> long "profile-file"
                             <> metavar "FILENAME"))
               <*> optional (strOption
                             (help "Root directory for build directories"
                             <> short 'b'
                             <> long "build-root-dir"
                             <> metavar "DIRECTORY"))
               <*> switch (help "Keep build directories after exit"
                             <> short 'k'
                             <> long "keep-build-dir")
               <*> switch (help "Predictable build directory names"
                             <> short 'u'
                             <> long "predictable-build-dir")
               <*> many (strArgument idm)

  where
    toCliOpts :: [FilePath]
              -> Maybe FilePath
              -> Bool
              -> Bool
              -> Maybe FilePath
              -> Maybe FilePath
              -> Maybe FilePath
              -> Bool
              -> Bool
              -> [String]
              -> CliOpts
    toCliOpts ps cfg verbose quiet logF profF buildRoot keep notUnique args =
      let minLogLevel = if verbose then Just LogTrace else
                          if quiet then Just LogError else Nothing
      in CliOpts { configFile = (Path <$> cfg) <|> pure defaultB9ConfigFile
                 , projectFiles = ps
                 , extraArgs = args
                 , cliB9Config = mempty { verbosity = minLogLevel
                                        , logFile = logF
                                        , profileFile = profF
                                        , buildDirRoot = buildRoot
                                        , keepTempDirs = keep
                                        , uniqueBuildDirs = not notUnique
                                        }
                 }

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
                                              }
               }
