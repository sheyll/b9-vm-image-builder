module Main
  ( main,
  )
where

import B9
import Control.Lens ((&), (.~), _Just, set)
import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty hiding ((</>))

main :: IO ()
main = do
  b9Opts <- parseCommandLine
  applyB9RunParameters b9Opts

-- | A data structure that contains the `B9Invocation`
-- as well as build parameters.
data B9RunParameters
  = B9RunParameters
      B9ConfigOverride
      (B9ConfigAction ())
      Environment

hostNetworkMagicValue :: String
hostNetworkMagicValue = "host"

applyB9RunParameters :: B9RunParameters -> IO ()
applyB9RunParameters (B9RunParameters overrides act vars) =
  let cfg = overrides & customEnvironment .~ vars
   in runB9ConfigActionWithOverrides act cfg

parseCommandLine :: IO B9RunParameters
parseCommandLine =
  execParser
    ( info
        (helper <*> (B9RunParameters <$> globals <*> cmds <*> buildVars))
        ( fullDesc
            <> progDesc
              "Build and run VM-Images inside docker/system-nspawn/LXC containers. Custom arguments follow after '--' and are accessable in many strings in build files  trough shell like variable references, i.e. '${arg_N}' referes to positional argument $N.\n\nRepository names passed to the command line are looked up in the B9 configuration file, which is per default located in: '~/.b9/b9.conf'. The current working directory is used as ${projectRoot} if not otherwise specified in the config file, or via the '-b' option."
            <> headerDoc (Just b9HelpHeader)
        )
    )
  where
    b9HelpHeader = linebreak <> text ("B9 - a benign VM-Image build tool v. " ++ b9VersionString)

globals :: Parser B9ConfigOverride
globals =
  toGlobalOpts
    <$> optional
      ( strOption
          ( help "Path to user's b9-configuration (default: ~/.b9/b9.conf)" <> short 'c' <> long "configuration-file"
              <> metavar "FILENAME"
          )
      )
    <*> switch (help "Log everything that happens to stdout" <> short 'v' <> long "verbose")
    <*> switch (help "Suppress non-error output" <> short 'q' <> long "quiet")
    <*> optional (strOption (help "Path to a logfile" <> short 'l' <> long "log-file" <> metavar "FILENAME"))
    <*> optional
      ( strOption
          ( help
              "Root directory for build directories. If not specified '.'. The path will be canonicalized and stored in ${projectRoot}."
              <> short 'b'
              <> long "build-root-dir"
              <> metavar "DIRECTORY"
          )
      )
    <*> switch (help "Keep build directories after exit" <> short 'k' <> long "keep-build-dir")
    <*> switch (help "Predictable build directory names" <> short 'u' <> long "predictable-build-dir")
    <*> optional
      ( strOption
          ( help "Cache directory for shared images, default: '~/.b9/repo-cache'" <> long "repo-cache"
              <> metavar "DIRECTORY"
          )
      )
    <*> optional
      (strOption (help "Remote repository to share image to" <> short 'r' <> long "repo" <> metavar "REPOSITORY_ID"))
    <*> optional
      ( strOption
          ( help
              ( "Override the container network setting.\nThe special value '"
                  ++ hostNetworkMagicValue
                  ++ "' disables restricted container networking."
              )
              <> long "network"
              <> metavar "NETWORK_ID"
          )
      )
  where
    toGlobalOpts ::
      Maybe FilePath ->
      Bool ->
      Bool ->
      Maybe FilePath ->
      Maybe FilePath ->
      Bool ->
      Bool ->
      Maybe FilePath ->
      Maybe String ->
      Maybe String ->
      B9ConfigOverride
    toGlobalOpts cfg verbose quiet logF buildRoot keep predictableBuildDir mRepoCache repo containerNetworking =
      let minLogLevel
            | verbose = Just LogTrace
            | quiet = Just LogError
            | otherwise = Nothing
          b9cfg =
            Endo (verbosity .~ minLogLevel) <> Endo (logFile .~ logF) <> Endo (projectRoot .~ buildRoot)
              <> Endo (keepTempDirs .~ keep)
              <> Endo (uniqueBuildDirs .~ not predictableBuildDir)
              <> Endo (repository .~ repo)
              <> Endo (repositoryCache .~ (Path <$> mRepoCache))
              <> case containerNetworking of
                Just n | n /= hostNetworkMagicValue -> Endo (set (libVirtLXCConfigs . _Just . networkId) (Just n))
                Just n | n == hostNetworkMagicValue -> Endo (set (libVirtLXCConfigs . _Just . networkId) Nothing)
                _ -> mempty
              <> case containerNetworking of
                Just n | n /= hostNetworkMagicValue -> Endo (set (dockerConfigs . _Just . dockerNetworkId) (Just n))
                Just n | n == hostNetworkMagicValue -> Endo (set (dockerConfigs . _Just . dockerNetworkId) Nothing)
                _ -> mempty
       in B9ConfigOverride {_customB9ConfigPath = Path <$> cfg, _customB9Config = b9cfg, _customEnvironment = mempty}

cmds :: Parser (B9ConfigAction ())
cmds =
  subparser
    ( command "version" (info (pure runShowVersion) (progDesc "Show program version and exit."))
        <> command
          "build"
          ( info
              (void . runBuildArtifacts <$> buildFileParser)
              (progDesc "Merge all build file and generate all artifacts.")
          )
        <> command
          "run"
          ( info
              ((\x y -> void (runRun x y)) <$> sharedImageNameParser <*> many (strArgument idm))
              (progDesc "Run a command on the lastest version of the specified shared image.")
          )
        <> command
          "push"
          ( info
              (runPush <$> sharedImageNameParser)
              (progDesc "Push the lastest shared image from cache to the selected  remote repository.")
          )
        <> command
          "pull"
          ( info
              (runPull <$> optional sharedImageNameParser)
              ( progDesc
                  "Either pull shared image meta data from all repositories, or only from just a selected one. If additionally the name of a shared images was specified, pull the newest version from either the selected repo, or from the repo with the most recent version."
              )
          )
        <> command
          "clean-local"
          (info (pure runGcLocalRepoCache) (progDesc "Remove old versions of shared images from the local cache."))
        <> command
          "clean-remote"
          ( info
              (pure runGcRemoteRepoCache)
              ( progDesc
                  "Remove cached meta-data of a remote repository. If no '-r' is given, clean the meta data of ALL remote repositories."
              )
          )
        <> command "list" (info (pure (void runListSharedImages)) (progDesc "List shared images."))
        <> command "add-repo" (info (runAddRepo <$> remoteRepoParser) (progDesc "Add a remote repo."))
        <> command "reformat" (info (runFormatBuildFiles <$> buildFileParser) (progDesc "Re-Format all build files."))
    )

buildFileParser :: Parser [FilePath]
buildFileParser =
  helper
    <*> some
      ( strOption
          ( help "Build file to load, specify multiple build files (each witch '-f') to build them all in a single run."
              <> short 'f'
              <> long "project-file"
              <> metavar "FILENAME"
              <> noArgError (ErrorMsg "No build file specified!")
          )
      )

buildVars :: Parser Environment
buildVars = flip addPositionalArguments mempty <$> many (strArgument idm)

remoteRepoParser :: Parser RemoteRepo
remoteRepoParser =
  helper
    <*> ( RemoteRepo <$> strArgument (help "The name of the remmote repository." <> metavar "NAME")
            <*> strArgument (help "The (remote) repository root path." <> metavar "REMOTE_DIRECTORY")
            <*> ( SshPrivKey
                    <$> strArgument (help "Path to the SSH private key file used for  authorization." <> metavar "SSH_PRIV_KEY_FILE")
                )
            <*> ( SshRemoteHost
                    <$> ( (,) <$> strArgument (help "Repo hostname or IP" <> metavar "HOST")
                            <*> argument auto (help "SSH-Port number" <> value 22 <> showDefault <> metavar "PORT")
                        )
                )
            <*> (SshRemoteUser <$> strArgument (help "SSH-User to login" <> metavar "USER"))
        )

sharedImageNameParser :: Parser SharedImageName
sharedImageNameParser = helper <*> (SharedImageName <$> strArgument (help "Shared image name" <> metavar "NAME"))
