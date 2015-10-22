module Main where

import Options.Applicative             hiding (action)
import Options.Applicative.Help.Pretty hiding ((</>))

import Control.Exception
import Data.Function                   (on)
import Data.Maybe
import Data.Version
import Paths_b9
import System.Directory
import System.IO.Error                 hiding (isDoesNotExistErrorType)
#if !MIN_VERSION_base(4,8,0)
import System.IO.Error                 hiding (catch)
import Prelude                         hiding (catch)
#endif
import B9

main :: IO ()
main = do
  b9Opts <- parseCommandLine
  result <- runB9 b9Opts
  exit result
  where
    exit success = when (not success) (exitWith (ExitFailure 128))

parseCommandLine :: IO B9Options
parseCommandLine =
    execParser
        (info
             (helper <*> (B9Options <$> globals <*> cmds <*> buildVars))
             (fullDesc <>
              progDesc
                  "Build and run VM-Images inside LXC containers. Custom arguments follow after '--' and are accessable in many strings in build files  trough shell like variable references, i.e. '${arg_N}' referes to positional argument $N.\n\nRepository names passed to the command line are looked up in the B9 configuration file, which is per default located in: '~/.b9/b9.conf'" <>
              headerDoc (Just helpHeader)))
  where
    helpHeader =
        linebreak <>
        text ("B9 - a benign VM-Image build tool v. " ++ b9Version)

data B9Options =
    B9Options GlobalOpts
              BuildAction
              BuildVariables

data GlobalOpts = GlobalOpts
    { configFile :: Maybe SystemPath
    , cliB9Config :: B9Config
    }

type BuildAction = Maybe SystemPath -> ConfigParser -> B9Config -> IO Bool

runB9 :: B9Options -> IO Bool
runB9 (B9Options globalOpts action vars) = do
    let cfgWithArgs =
            cfgCli
            { envVars = envVars cfgCli ++ vars
            }
        cfgCli = cliB9Config globalOpts
        cfgFile = configFile globalOpts
    cp <- configure cfgFile cfgCli
    action cfgFile cp cfgWithArgs

runShowVersion :: BuildAction
runShowVersion _cfgFile _cp _conf = do
    putStrLn b9Version
    return True

runBuildArtifacts :: [FilePath] ->  BuildAction
runBuildArtifacts buildFiles _cfgFile cp conf = do
    generators <- mapM consult buildFiles
    buildArtifacts (mconcat generators) cp conf

runFormatBuildFiles :: [FilePath] ->  BuildAction
runFormatBuildFiles buildFiles _cfgFile _cp _conf = do
    generators <- mapM consult buildFiles
    let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
    putStrLn `mapM_` (generatorsFormatted)
    (uncurry writeFile) `mapM_` (buildFiles `zip` generatorsFormatted)
    return True

runPush :: SharedImageName -> BuildAction
runPush name _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    impl =
        run
            cp
            conf'
            (if not (isJust (repository conf'))
                 then do
                     errorL
                         "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
                     return False
                 else do
                     pushSharedImageLatestVersion name
                     return True)

runPull :: Maybe SharedImageName -> BuildAction
runPull mName _cfgFile cp conf =
    run cp conf' (pullRemoteRepos >> maybePullImage)
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    maybePullImage = maybe (return True) pullLatestImage mName

runRun :: SharedImageName -> [String] -> BuildAction
runRun (SharedImageName name) cmdAndArgs _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        , interactive = True
        }
    impl = buildArtifacts runCmdAndArgs cp conf'
      where
        runCmdAndArgs =
            Artifact
                (IID ("run-" ++ name))
                (VmImages
                     [ ImageTarget
                           Transient
                           (From name KeepSize)
                           (MountPoint "/")]
                     (VmScript
                          X86_64
                          [SharedDirectory "." (MountPoint "/mnt/CWD")]
                          (Run (head cmdAndArgs') (tail cmdAndArgs'))))
        cmdAndArgs' =
            if null cmdAndArgs
                then ["/usr/bin/zsh"]
                else cmdAndArgs


runGcLocalRepoCache :: BuildAction
runGcLocalRepoCache _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    impl =
        run cp conf' $
        do toDelete <-
               (obsoleteSharedmages . map snd) <$>
               lookupSharedImages (== Cache) (const True)
           imgDir <- getSharedImagesCacheDir
           let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
               infoFiles = sharedImageFileName <$> toDelete
               imgFiles = (imageFileName . sharedImageImage) <$> toDelete
           if null filesToDelete
               then liftIO $
                    do putStrLn "\n\nNO IMAGES TO DELETE\n"
                       return True
               else liftIO $
                    do putStrLn "DELETING FILES:"
                       putStrLn (unlines filesToDelete)
                       mapM_ removeIfExists filesToDelete
                       return True
    obsoleteSharedmages :: [SharedImage] -> [SharedImage]
    obsoleteSharedmages =
        concatMap (tail . reverse) .
        filter ((> 1) . length) . groupBy ((==) `on` siName)
    removeIfExists :: FilePath -> IO ()
    removeIfExists fileName = removeFile fileName `catch` handleExists
      where
        handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

runGcRemoteRepoCache :: BuildAction
runGcRemoteRepoCache _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    impl =
        run cp conf' $
        do repos <- getSelectedRepos
           cache <- getRepoCache
           mapM_ (cleanRemoteRepo cache) repos
           return True

runListSharedImages :: BuildAction
runListSharedImages _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    impl =
        run cp conf' $
        do remoteRepo <- getSelectedRemoteRepo
           let repoPred =
                   maybe (== Cache) ((==) . toRemoteRepository) remoteRepo
           allRepos <- getRemoteRepos
           if isNothing remoteRepo
               then liftIO $
                    do putStrLn "Showing local shared images only."
                       putStrLn $
                           "\nTo view the contents of a remote repo add \n\
                        \the '-r' switch with one of the remote \n\
                        \repository ids."
               else liftIO $
                    putStrLn
                        ("Showing shared images on: " ++
                         remoteRepoRepoId (fromJust remoteRepo))
           when (not (null allRepos)) $
               liftIO $
               do putStrLn "\nAvailable remote repositories:"
                  mapM_ (putStrLn . (" * " ++) . remoteRepoRepoId) allRepos
           imgs <- lookupSharedImages repoPred (const True)
           if null imgs
               then liftIO $ putStrLn "\n\nNO SHARED IMAGES\n"
               else liftIO $
                    do putStrLn ""
                       putStrLn $ prettyPrintSharedImages $ map snd imgs
           return True


runAddRepo :: RemoteRepo -> BuildAction
runAddRepo repo cfgFile cp _conf = do
    repo' <- remoteRepoCheckSshPrivKey repo
    case writeRemoteRepoConfig repo' cp of
        Left er ->
            error
                (printf
                     "Failed to add remote repo '%s' to b9 configuration. The error was: \"%s\"."
                     (show repo)
                     (show er))
        Right cpWithRepo -> writeB9Config cfgFile cpWithRepo
    return True

globals :: Parser GlobalOpts
globals =
    toGlobalOpts <$>
    optional
        (strOption
             (help "Path to user's b9-configuration (default: ~/.b9/b9.conf)" <>
              short 'c' <>
              long "configuration-file" <>
              metavar "FILENAME")) <*>
    switch
        (help "Log everything that happens to stdout" <> short 'v' <>
         long "verbose") <*>
    switch (help "Suppress non-error output" <> short 'q' <> long "quiet") <*>
    optional
        (strOption
             (help "Path to a logfile" <> short 'l' <> long "log-file" <>
              metavar "FILENAME")) <*>
    optional
        (strOption
             (help "Output file for a command/timing profile" <>
              long "profile-file" <>
              metavar "FILENAME")) <*>
    optional
        (strOption
             (help "Root directory for build directories" <> short 'b' <>
              long "build-root-dir" <>
              metavar "DIRECTORY")) <*>
    switch
        (help "Keep build directories after exit" <> short 'k' <>
         long "keep-build-dir") <*>
    switch
        (help "Predictable build directory names" <> short 'u' <>
         long "predictable-build-dir") <*>
    optional
        (strOption
             (help
                  "Cache directory for shared images, default: '~/.b9/repo-cache'" <>
              long "repo-cache" <>
              metavar "DIRECTORY")) <*>
    optional
        (strOption
             (help "Remote repository to share image to" <> short 'r' <>
              long "repo" <>
              metavar "REPOSITORY_ID"))
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
        -> GlobalOpts
    toGlobalOpts cfg verbose quiet logF profF buildRoot keep notUnique mRepoCache repo =
        let minLogLevel =
                if verbose
                    then Just LogTrace
                    else if quiet
                             then Just LogError
                             else Nothing
            b9cfg' =
                let b9cfg =
                        mempty
                        { verbosity = minLogLevel
                        , logFile = logF
                        , profileFile = profF
                        , buildDirRoot = buildRoot
                        , keepTempDirs = keep
                        , uniqueBuildDirs = not notUnique
                        , repository = repo
                        }
                in b9cfg
                   { repositoryCache = Path <$> mRepoCache
                   }
        in GlobalOpts
           { configFile = (Path <$> cfg) <|> pure defaultB9ConfigFile
           , cliB9Config = b9cfg'
           }

cmds :: Parser BuildAction
cmds =
    subparser
        (command
             "version"
             (info
                  (pure runShowVersion)
                  (progDesc "Show program version and exit.")) <>
         command
             "build"
             (info
                  (runBuildArtifacts <$> buildFileParser)
                  (progDesc "Merge all build file and generate all artifacts.")) <>
         command
             "run"
             (info
                  (runRun <$> sharedImageNameParser <*> many (strArgument idm))
                  (progDesc
                       "Run a command on the lastest version of the specified shared image. All modifications are lost on exit.")) <>
         command
             "push"
             (info
                  (runPush <$> sharedImageNameParser)
                  (progDesc
                       "Push the lastest shared image from cache to the selected  remote repository.")) <>
         command
             "pull"
             (info
                  (runPull <$> optional sharedImageNameParser)
                  (progDesc
                       "Either pull shared image meta data from all repositories, or only from just a selected one. If additionally the name of a shared images was specified, pull the newest version from either the selected repo, or from the repo with the most recent version.")) <>
         command
             "clean-local"
             (info
                  (pure runGcLocalRepoCache)
                  (progDesc
                       "Remove old versions of shared images from the local cache.")) <>
         command
             "clean-remote"
             (info
                  (pure runGcRemoteRepoCache)
                  (progDesc
                       "Remove cached meta-data of a remote repository. If no '-r' is given, clean the meta data of ALL remote repositories.")) <>
         command
             "list"
             (info (pure runListSharedImages) (progDesc "List shared images.")) <>
         command
             "add-repo"
             (info
                  (runAddRepo <$> remoteRepoParser)
                  (progDesc "Add a remote repo.")) <>
         command
             "reformat"
             (info
                  (runFormatBuildFiles <$> buildFileParser)
                  (progDesc "Re-Format all build files.")))

buildFileParser :: Parser [FilePath]
buildFileParser =
    helper <*>
    some
        (strOption
             (help
                  "Build file to load, specify multiple build files (each witch '-f') to build them all in a single run." <>
              short 'f' <>
              long "project-file" <>
              metavar "FILENAME" <>
              noArgError (ErrorMsg "No build file specified!")))

buildVars :: Parser BuildVariables
buildVars =
    zip (("arg_" ++) . show <$> ([1 ..] :: [Int])) <$> many (strArgument idm)

remoteRepoParser :: Parser RemoteRepo
remoteRepoParser =
    helper <*>
    (RemoteRepo <$>
     strArgument (help "The name of the remmote repository." <> metavar "NAME") <*>
     strArgument
         (help "The (remote) repository root path." <>
          metavar "REMOTE_DIRECTORY") <*>
     (SshPrivKey <$>
      strArgument
          (help
               "Path to the SSH private key file used for  authorization." <>
           metavar "SSH_PRIV_KEY_FILE")) <*>
     (SshRemoteHost <$>
      ((,) <$> strArgument (help "Repo hostname or IP" <> metavar "HOST") <*>
       argument
           auto
           (help "SSH-Port number" <> value 22 <> showDefault <> metavar "PORT"))) <*>
     (SshRemoteUser <$>
      strArgument (help "SSH-User to login" <> metavar "USER")))

sharedImageNameParser :: Parser SharedImageName
sharedImageNameParser =
    helper <*>
    (SharedImageName <$>
     strArgument (help "Shared image name" <> metavar "NAME"))

b9Version :: String
b9Version = showVersion version
