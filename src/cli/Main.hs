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
  code <- build (cliAction opts) (mconcat prjs) conf (cliB9Config opts)
  exit code

exit success = when (not success) $ exitWith (ExitFailure 128)

data CliOpts = CliOpts { configFile :: Maybe SystemPath
                       , projectFiles :: [FilePath]
                       , cliB9Config :: B9Config
                       , cliAction :: BuildAction
                       }

parseCommandLine :: IO CliOpts
parseCommandLine =
  execParser (info (helper <*> cliArgParser)
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
               <*> switch (help "Show the processed project and config without building"
                             <> short 'd'
                             <> long "dry-run")
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
                             (help "Cache downloaded base images downloaded in a \
                                   \custom repository, default: 'cache-repo' \
                                   \defined in the B9 config file"
                             <> long "repo-cache"
                             <> metavar "REPOSITORY_ID"))
               <*> optional (strOption
                             (help "Publish base images to a repository defined\
                                   \ in the B9 config file"
                              <> short 'P'
                              <> long "publish-to"
                              <> metavar "REPOSITORY_ID"))
              <*> many (strArgument idm)

  where
    toCliOpts :: [FilePath]
              -> Maybe FilePath
              -> Bool
              -> Bool
              -> Bool
              -> Maybe FilePath
              -> Maybe FilePath
              -> Maybe FilePath
              -> Bool
              -> Bool
              -> Maybe String
              -> Maybe String
              -> [String]
              -> CliOpts
    toCliOpts ps cfg dryRun verbose quiet logF profF buildRoot keep notUnique
              repo repoCache rest =
      let minLogLevel = if verbose then Just LogTrace else
                          if quiet then Just LogError else Nothing
          extraArgs = zip (("arg_"++) . show <$> [1..]) rest
      in CliOpts { configFile = (Path <$> cfg) <|> pure defaultB9ConfigFile
                 , projectFiles = ps
                 , cliAction = if dryRun
                                  then DryRun
                                  else RunBuild
                 , cliB9Config = mempty { verbosity = minLogLevel
                                        , logFile = logF
                                        , profileFile = profF
                                        , buildDirRoot = buildRoot
                                        , keepTempDirs = keep
                                        , uniqueBuildDirs = not notUnique
                                        , envVars = extraArgs
                                        , repository = repo
                                        , repositoryCache = repoCache
                                        }
                 }
