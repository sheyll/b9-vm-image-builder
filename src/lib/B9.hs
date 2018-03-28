{-| B9 is a library and build tool with primitive operations to rmrun a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.ArtifactGenerator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 (configure, b9Version, b9VersionString
          , B9RunParameters(..),runB9,defaultB9RunParameters

          , module X) where
import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Monoid as X
import Data.List as X
import Data.Maybe as X
import Text.Show.Pretty as X (ppShow)
import System.Exit as X (exitWith, ExitCode(..))
import System.FilePath as X
        (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import Text.Printf as X (printf)
import Data.Version as X
import B9.Builder as X
import B9.Invokation as X
import Paths_b9 (version)
import qualified B9.LibVirtLXC as LibVirtLXC
import Data.Function (on)

-- | Return the cabal package version of the B9 library.
b9Version :: Version
b9Version = version

-- | Return the cabal package version of the B9 library,
-- formatted using `showVersion`.
b9VersionString :: String
b9VersionString = showVersion version

-- | A data structure that contains the `B9Invokation Bool`
-- as well as build parameters.
data B9RunParameters =
  B9RunParameters
            B9CustomConfig
            (B9Invokation Bool)
            BuildVariables

-- | Run a b9 build.
-- Return `True` if the build was successful.
runB9 :: B9RunParameters -> IO Bool
runB9 (B9RunParameters globalOpts action vars) = do
  let cfgWithArgs = cfgCli { envVars = envVars cfgCli ++ vars }
      cfgCli      = customB9Config globalOpts
      cfgFile     = customB9ConfigPath globalOpts
  cp <- configure cfgFile cfgCli
  action cfgFile cp cfgWithArgs

defaultB9RunParameters :: B9Invokation Bool -> B9RunParameters
defaultB9RunParameters ba =
  B9RunParameters (B9CustomConfig mempty mempty) ba mempty

runShowVersion :: MonadIO m => m Bool
runShowVersion = do
  liftIO $ putStrLn b9Version
  return True

runBuildArtifacts :: [FilePath] -> B9Invokation Bool
runBuildArtifacts buildFiles = do
  generators <- mapM consult buildFiles
  buildArtifacts (mconcat generators)

runFormatBuildFiles :: MonadIO m => [FilePath] -> m Bool
runFormatBuildFiles buildFiles = do
  generators <- mapM consult buildFiles
  let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
  putStrLn `mapM` (generatorsFormatted)
  (uncurry writeFile) `mapM` (buildFiles `zip` generatorsFormatted)
  return True

runPush :: SharedImageName -> B9Invokation Bool
runPush name = do
    modifyInvokationConfig (\conf -> conf {keepTempDirs = False})
    conf <- getInvokationConfig
    run
      ( if not (isJust (repository conf))
        then do
          errorL
            "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
          return False
        else do
          pushSharedImageLatestVersion name
          return True
      )

runPull :: Maybe SharedImageName -> B9Invokation Bool
runPull mName = do
  modifyInvokationConfig (\conf -> conf {keepTempDirs = False})
  run (pullRemoteRepos >> maybePullImage)
 where
  maybePullImage = maybe (return True) pullLatestImage mName

runRun :: SharedImageName -> [String] -> B9Invokation Bool
runRun (SharedImageName name) cmdAndArgs = do
  modifyInvokationConfig (\conf -> conf { keepTempDirs = False
                                        , interactive = True })
  buildArtifacts runCmdAndArgs
 where
    runCmdAndArgs = Artifact
      (IID ("run-" ++ name))
      ( VmImages
        [ImageTarget Transient (From name KeepSize) (MountPoint "/")]
        ( VmScript X86_64
                   [SharedDirectory "." (MountPoint "/mnt/CWD")]
                   (Run (head cmdAndArgs') (tail cmdAndArgs'))
        )
      )
      where
        cmdAndArgs' = if null cmdAndArgs then ["/usr/bin/zsh"] else cmdAndArgs


runGcLocalRepoCache :: B9Invokation Bool
runGcLocalRepoCache _cfgFile cp conf = do
  modifyInvokationConfig (\conf -> conf {keepTempDirs = False})
  impl
 where
  impl  = run $ do
    toDelete <- (obsoleteSharedmages . map snd)
      <$> lookupSharedImages (== Cache) (const True)
    imgDir <- getSharedImagesCacheDir
    let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
        infoFiles     = sharedImageFileName <$> toDelete
        imgFiles      = (imageFileName . sharedImageImage) <$> toDelete
    if null filesToDelete
      then liftIO $ do
        putStrLn "\n\nNO IMAGES TO DELETE\n"
        return True
      else liftIO $ do
        putStrLn "DELETING FILES:"
        putStrLn (unlines filesToDelete)
        mapM_ removeIfExists filesToDelete
        return True
  obsoleteSharedmages :: [SharedImage] -> [SharedImage]
  obsoleteSharedmages =
    concatMap (tail . reverse) . filter ((> 1) . length) . groupBy
      ((==) `on` siName)
  removeIfExists :: FilePath -> IO ()
  removeIfExists fileName = removeFile fileName `catch` handleExists
   where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise             = throwIO e

runGcRemoteRepoCache :: B9Invokation Bool
runGcRemoteRepoCache = do
  modifyInvokationConfig (\conf -> conf {keepTempDirs = False})
  run $ do
    repos <- getSelectedRepos
    cache <- getRepoCache
    mapM_ (cleanRemoteRepo cache) repos
    return True

runListSharedImages :: B9Invokation Bool
runListSharedImages _cfgFile = do
  modifyInvokationConfig (\conf -> conf {keepTempDirs = False})
  run $ do
    remoteRepo <- getSelectedRemoteRepo
    let repoPred = maybe (== Cache) ((==) . toRemoteRepository) remoteRepo
    allRepos <- getRemoteRepos
    if isNothing remoteRepo
      then liftIO $ do
        putStrLn "Showing local shared images only."
        putStrLn
          $ "\nTo view the contents of a remote repo add \n\
                        \the '-r' switch with one of the remote \n\
                        \repository ids."
      else liftIO $ putStrLn
        ("Showing shared images on: " ++ remoteRepoRepoId (fromJust remoteRepo))
    when (not (null allRepos)) $ liftIO $ do
      putStrLn "\nAvailable remote repositories:"
      mapM_ (putStrLn . (" * " ++) . remoteRepoRepoId) allRepos
    imgs <- lookupSharedImages repoPred (const True)
    if null imgs
      then liftIO $ putStrLn "\n\nNO SHARED IMAGES\n"
      else liftIO $ do
        putStrLn ""
        putStrLn $ prettyPrintSharedImages $ map snd imgs
    return True


runAddRepo :: RemoteRepo -> B9Invokation Bool
runAddRepo repo = do
  repo' <- remoteRepoCheckSshPrivKey repo
  cp <- askInvokationConfigParser
  case writeRemoteRepoConfig repo' cp of
    Left er -> error
      ( printf
        "Failed to add remote repo '%s' to b9 configuration. The error was: \"%s\"."
        (show repo)
        (show er)
      )
    Right cpWithRepo -> writeB9Config cfgFile cpWithRepo
  return True

runLookupLocalSharedImage
  :: SharedImageName -> B9Invokation (Maybe SharedImageBuildId)
runLookupLocalSharedImage n = run go
 where
  go =
    extractNewestImageFromResults
      <$> lookupSharedImages isAvailableOnLocalHost hasTheDesiredName
   where
    extractNewestImageFromResults =
      listToMaybe . map getBuildId . take 1 . reverse . map snd
      where
        getBuildId (SharedImage _ _ i _ _) = i
    isAvailableOnLocalHost = (Cache ==)
    hasTheDesiredName (SharedImage n' _ _ _ _) = n == n'
