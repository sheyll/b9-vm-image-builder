module Main where

import Options.Applicative hiding (action)
import Options.Applicative.Help.Pretty
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
  execParser (info (helper <*> (B9Options <$> globals <*> cmds <*> buildVars))
               (fullDesc
                <> progDesc "Build and run VM-Images inside LXC containers.\
                            \ Custom arguments follow after '--' and are\
                            \ accessable in many strings in project configs \
                            \ trough shell like variable references, i.e. \
                            \'${arg_N}' referes to positional argument $N.\n\
                            \\n\
                            \Repository names passed to the command line are\
                            \ looked up in the B9 configuration file, which is\
                            \ on Un*x like system per default located in: \
                            \ '~/.b9/b9.config'"
                <> headerDoc (Just helpHeader)))
  where
    helpHeader = linebreak <> text "B9 - a benign VM-Image build tool"

data B9Options = B9Options GlobalOpts
                           BuildAction
                           BuildVariables

data GlobalOpts = GlobalOpts { configFile :: Maybe SystemPath
                             , cliB9Config :: B9Config  }

type BuildAction = ConfigParser -> B9Config -> IO Bool

runB9 :: B9Options -> IO Bool
runB9 (B9Options globalOpts action vars) = do
  let cfgWithArgs = cfgCli { envVars = envVars cfgCli ++ vars }
      cfgCli = cliB9Config globalOpts
  cp <- configure (configFile globalOpts) cfgCli
  action cp cfgWithArgs

runBuild :: [FilePath] -> BuildAction
runBuild projectFiles cp conf = do
  prjs <- mapM consult projectFiles
  buildProject (mconcat prjs) cp conf

runShow :: [FilePath] -> BuildAction
runShow projectFiles cp conf = do
  prjs <- mapM consult projectFiles
  showProject (mconcat prjs) cp conf

runListSharedImages :: BuildAction
runListSharedImages cp conf = impl
  where
    conf' = conf { keepTempDirs = True }
    impl = do
      imgs <- run "list-share-images" cp conf' getSharedImages
      if null imgs
        then putStrLn "\n\nNO SHAREABLE IMAGES\n"
        else putStrLn "SHAREABLE IMAGES:"
      mapM_ (putStrLn . ppShow) imgs
      return True

globals :: Parser GlobalOpts
globals = toGlobalOpts
               <$> optional (strOption
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
               <*> optional (strOption
                             (help "Cache directory for shared images, default: '~/.b9/repo-cache'"
                             <> long "repo-cache"
                             <> metavar "DIRECTORY"))
               <*> optional (strOption
                             (help "Remote repository to share image to"
                              <> short 'r'
                              <> long "repo"
                              <> metavar "REPOSITORY_ID"))

  where
    toGlobalOpts ::  Maybe FilePath
              -> Bool
              -> Bool
              -> Maybe FilePath
              -> Maybe FilePath
              -> Maybe FilePath
              -> Bool
              -> Bool
              -> Maybe String
              -> Maybe FilePath
              -> GlobalOpts
    toGlobalOpts cfg verbose quiet logF profF buildRoot keep notUnique
                 repo mRepoCache =
      let minLogLevel = if verbose then Just LogTrace else
                          if quiet then Just LogError else Nothing
          b9cfg' = let b9cfg = mempty { verbosity = minLogLevel
                                      , logFile = logF
                                      , profileFile = profF
                                      , buildDirRoot = buildRoot
                                      , keepTempDirs = keep
                                      , uniqueBuildDirs = not notUnique
                                      , repository = repo
                                      }
                   in case mRepoCache of
                        Nothing -> b9cfg
                        Just repoCache ->
                          let rc = Path repoCache
                          in b9cfg { repositoryCache = rc }
      in GlobalOpts { configFile = (Path <$> cfg) <|> pure defaultB9ConfigFile
                    , cliB9Config = b9cfg' }

cmds :: Parser BuildAction
cmds = subparser (  command "build"
                             (info (runBuild <$> projects)
                                   (progDesc "Merge all project files and\
                                             \ build."))
                  <> command "print"
                             (info (runShow <$> projects)
                                   (progDesc "Show the final project that\
                                             \ would be used by the 'build' \
                                             \ command."))
                  <> command "list-shared-images"
                             (info (pure runListSharedImages)
                                   (progDesc "List shared images.")))

projects :: Parser [FilePath]
projects = helper <*>
           some (strOption
                  (help "Project file to load, specify multiple project\
                        \ files to merge them into a single project."
                  <> short 'f'
                  <> long "project-file"
                  <> metavar "FILENAME"
                  <> noArgError (ErrorMsg "No project file specified!")))

buildVars :: Parser BuildVariables
buildVars = zip (("arg_"++) . show <$> ([1..] :: [Int])) <$> many (strArgument idm)
