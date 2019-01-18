module Main
    ( main
    )
where

import           Options.Applicative     hiding ( action )
import           Options.Applicative.Help.Pretty
                                         hiding ( (</>) )
import           B9
import           Control.Lens                   ( (.~)
                                                , (&)
                                                , over
                                                , (^.)
                                                )

main :: IO ()
main = do
    b9Opts <- parseCommandLine
    applyB9RunParameters b9Opts


-- | A data structure that contains the `B9Invokation`
-- as well as build parameters.
data B9RunParameters =
  B9RunParameters
            B9ConfigOverride
            Cmd
            BuildVariables

type Cmd = B9ConfigAction IO ()

hostNetworkMagicValue :: String
hostNetworkMagicValue = "host"

applyB9RunParameters :: B9RunParameters -> IO ()
applyB9RunParameters (B9RunParameters overrides act vars) =
    let cfg =
            noB9ConfigOverride
                &  customB9Config
                %~ (over envVars (++ vars) . const (overrides ^. customB9Config)
                   )
                &  customLibVirtNetwork
                .~ (overrides ^. customLibVirtNetwork)
                &  maybe id
                         overrideB9ConfigPath
                         (overrides ^. customB9ConfigPath)
    in  execB9ConfigAction act cfg

parseCommandLine :: IO B9RunParameters
parseCommandLine = execParser
    (info
        (helper <*> (B9RunParameters <$> globals <*> cmds <*> buildVars))
        (  fullDesc
        <> progDesc
               "Build and run VM-Images inside LXC containers. Custom arguments follow after '--' and are accessable in many strings in build files  trough shell like variable references, i.e. '${arg_N}' referes to positional argument $N.\n\nRepository names passed to the command line are looked up in the B9 configuration file, which is per default located in: '~/.b9/b9.conf'. The current working directory is used as ${buildDirRoot} if not otherwise specified in the config file, or via the '-b' option."
        <> headerDoc (Just b9HelpHeader)
        )
    )
  where
    b9HelpHeader = linebreak
        <> text ("B9 - a benign VM-Image build tool v. " ++ b9VersionString)


globals :: Parser B9ConfigOverride
globals =
    toGlobalOpts
        <$> optional
                (strOption
                    (  help
                          "Path to user's b9-configuration (default: ~/.b9/b9.conf)"
                    <> short 'c'
                    <> long "configuration-file"
                    <> metavar "FILENAME"
                    )
                )
        <*> switch
                (  help "Log everything that happens to stdout"
                <> short 'v'
                <> long "verbose"
                )
        <*> switch
                (help "Suppress non-error output" <> short 'q' <> long "quiet")
        <*> optional
                (strOption
                    (  help "Path to a logfile"
                    <> short 'l'
                    <> long "log-file"
                    <> metavar "FILENAME"
                    )
                )
        <*> optional
                (strOption
                    (  help "Output file for a command/timing profile"
                    <> long "profile-file"
                    <> metavar "FILENAME"
                    )
                )
        <*> optional
                (strOption
                    (  help
                          "Root directory for build directories. If not specified '.'. The path will be canonicalized and stored in ${buildDirRoot}."
                    <> short 'b'
                    <> long "build-root-dir"
                    <> metavar "DIRECTORY"
                    )
                )
        <*> switch
                (help "Keep build directories after exit" <> short 'k' <> long
                    "keep-build-dir"
                )
        <*> switch
                (help "Predictable build directory names" <> short 'u' <> long
                    "predictable-build-dir"
                )
        <*> optional
                (strOption
                    (  help
                          "Cache directory for shared images, default: '~/.b9/repo-cache'"
                    <> long "repo-cache"
                    <> metavar "DIRECTORY"
                    )
                )
        <*> optional
                (strOption
                    (  help "Remote repository to share image to"
                    <> short 'r'
                    <> long "repo"
                    <> metavar "REPOSITORY_ID"
                    )
                )
        <*> optional
                (strOption
                    (  help
                          ("Override the libvirt-lxc network setting.\nThe special value '"
                          ++ hostNetworkMagicValue
                          ++ "' disables restricted container networking."
                          )
                    <> long "network"
                    <> metavar "NETWORK_ID"
                    )
                )
  where
    toGlobalOpts
        :: Maybe FilePath
        -> Bool
        -> Bool
        -> Maybe FilePath
        -> Maybe FilePath
        -> Maybe FilePath
        -> Bool
        -> Bool
        -> Maybe FilePath
        -> Maybe String
        -> Maybe String
        -> B9ConfigOverride
    toGlobalOpts cfg verbose quiet logF profF buildRoot keep predictableBuildDir mRepoCache repo libvirtNet
        = let
              minLogLevel = if verbose
                  then Just LogTrace
                  else if quiet then Just LogError else Nothing
              b9cfg =
                  mempty
                      &  verbosity
                      .~ minLogLevel
                      &  logFile
                      .~ logF
                      &  profileFile
                      .~ profF
                      &  buildDirRoot
                      .~ buildRoot
                      &  keepTempDirs
                      .~ keep
                      &  uniqueBuildDirs
                      .~ not predictableBuildDir
                      &  repository
                      .~ repo
                      &  repositoryCache
                      .~ (Path <$> mRepoCache)
          in
              B9ConfigOverride
                  { _customB9ConfigPath   = Path <$> cfg
                  , _customB9Config       = b9cfg
                  , _customLibVirtNetwork =
                      (\n -> if n == hostNetworkMagicValue
                              then Nothing
                              else Just n
                          )
                          <$> libvirtNet
                  }

cmds :: Parser Cmd
cmds = subparser
    (  command
          "version"
          (info (pure runShowVersion)
                (progDesc "Show program version and exit.")
          )
    <> command
           "build"
           (info
               (void . runBuildArtifacts <$> buildFileParser)

               (progDesc "Merge all build file and generate all artifacts.")
           )
    <> command
           "run"
           (info
               ((\x y -> void (runRun x y)) <$> sharedImageNameParser <*> many
                   (strArgument idm)
               )
               (progDesc
                   "Run a command on the lastest version of the specified shared image."
               )
           )
    <> command
           "push"
           (info
               (runPush <$> sharedImageNameParser)
               (progDesc
                   "Push the lastest shared image from cache to the selected  remote repository."
               )
           )
    <> command
           "pull"
           (info
               (runPull <$> optional sharedImageNameParser)
               (progDesc
                   "Either pull shared image meta data from all repositories, or only from just a selected one. If additionally the name of a shared images was specified, pull the newest version from either the selected repo, or from the repo with the most recent version."
               )
           )
    <> command
           "clean-local"
           (info
               (pure runGcLocalRepoCache)
               (progDesc
                   "Remove old versions of shared images from the local cache."
               )
           )
    <> command
           "clean-remote"
           (info
               (pure runGcRemoteRepoCache)
               (progDesc
                   "Remove cached meta-data of a remote repository. If no '-r' is given, clean the meta data of ALL remote repositories."
               )
           )
    <> command
           "list"
           (info (pure (void runListSharedImages))
                 (progDesc "List shared images.")
           )
    <> command
           "add-repo"
           (info (runAddRepo <$> remoteRepoParser)
                 (progDesc "Add a remote repo.")
           )
    <> command
           "reformat"
           (info (runFormatBuildFiles <$> buildFileParser)
                 (progDesc "Re-Format all build files.")
           )
    )

buildFileParser :: Parser [FilePath]
buildFileParser = helper <*> some
    (strOption
        (  help
              "Build file to load, specify multiple build files (each witch '-f') to build them all in a single run."
        <> short 'f'
        <> long "project-file"
        <> metavar "FILENAME"
        <> noArgError (ErrorMsg "No build file specified!")
        )
    )

buildVars :: Parser BuildVariables
buildVars =
    zip (("arg_" ++) . show <$> ([1 ..] :: [Int])) <$> many (strArgument idm)

remoteRepoParser :: Parser RemoteRepo
remoteRepoParser =
    helper
        <*> (   RemoteRepo
            <$> strArgument
                    (  help "The name of the remmote repository."
                    <> metavar "NAME"
                    )
            <*> strArgument
                    (  help "The (remote) repository root path."
                    <> metavar "REMOTE_DIRECTORY"
                    )
            <*> (SshPrivKey <$> strArgument
                    (  help
                            "Path to the SSH private key file used for  authorization."
                    <> metavar "SSH_PRIV_KEY_FILE"
                    )
                )
            <*> (   SshRemoteHost
                <$> (   (,)
                    <$> strArgument
                            (  help "Repo hostname or IP"
                            <> metavar "HOST"
                            )
                    <*> argument
                            auto
                            (  help "SSH-Port number"
                            <> value 22
                            <> showDefault
                            <> metavar "PORT"
                            )
                    )
                )
            <*> (SshRemoteUser <$> strArgument
                    (help "SSH-User to login" <> metavar "USER")
                )
            )

sharedImageNameParser :: Parser SharedImageName
sharedImageNameParser =
    helper
        <*> (   SharedImageName
            <$> strArgument (help "Shared image name" <> metavar "NAME")
            )
