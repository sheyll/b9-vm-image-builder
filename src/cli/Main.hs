module Main where

import Control.Exception
import Data.Function (on)
import B9.B9Monad
import Data.Maybe
import Data.Version
import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty hiding ((</>))
import Paths_b9
import System.Directory
import System.IO.Error hiding (isDoesNotExistErrorType)
import B9

main :: IO ()
main = do
  b9Opts <- parseCommandLine
  result <- runB9Command b9Opts
  exit result
  where
    exit success = when (not success) (exitWith (ExitFailure 128))

parseCommandLine :: IO B9Options
parseCommandLine =
    execParser
        (info
             (helper <*> (B9Options <$> parseGlobalOpts <*> cmds <*> parseBuildVars))
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

type BuildAction = Maybe SystemPath -> ConfigParser -> B9Config -> IO Bool

runB9Command :: B9Options -> IO Bool
runB9Command (B9Options globalOpts action vars) = do
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

runPush :: SharedImageName -> BuildAction
runPush name _cfgFile cp conf = impl
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    impl =
        runB9Monad
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
    runB9Monad cp conf' (pullRemoteRepos >> maybePullImage)
  where
    conf' =
        conf
        { keepTempDirs = False
        }
    maybePullImage = maybe (return True) pullLatestImage mName

runRun :: SharedImageName -> [String] -> BuildAction
runRun (SharedImageName name) cmdAndArgs _cfgFile cp conf =
    runProgram prog (Environment []) cp conf'
  where
    conf' =
        conf
        { keepTempDirs = False
        , interactive = True
        }
    prog = do
        e <- lxc ("running-" ++ name)
        img <- fromShared name
        void $ mount e img "/"
        runCommand e (Run c as)
        return True
    (c:as) =
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
        runB9Monad cp conf' $
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
        runB9Monad cp conf' $
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
        runB9Monad cp conf' $
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

cmds :: Parser BuildAction
cmds =
    subparser
        (command
             "version"
             (info
                  (pure runShowVersion)
                  (progDesc "Show program version and exit.")) <>
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
                  (progDesc "Add a remote repo.")))

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
