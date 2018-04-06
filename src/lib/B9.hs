{-| B9 is a library and build tool with primitive operations to rmrun a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.ArtifactGenerator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 ( b9Version, b9VersionString
          , B9RunParameters(..)
          , runB9
          , defaultB9RunParameters
          , runShowVersion
          , runBuildArtifacts
          , runFormatBuildFiles
          , runPush
          , runPull
          , runRun
          , runGcLocalRepoCache
          , runGcRemoteRepoCache
          , runListSharedImages
          , runAddRepo
          , runLookupLocalSharedImage
          , module X) where
import Control.Applicative as X
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Exception(throwIO, catch)
import System.IO.Error (isDoesNotExistError)
import System.Directory (removeFile)
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
import Data.Function (on)
import Control.Lens as Lens ((^.), over, (.~), (%~), set)

-- | Return the cabal package version of the B9 library.
b9Version :: Version
b9Version = version

-- | Return the cabal package version of the B9 library,
-- formatted using `showVersion`.
b9VersionString :: String
b9VersionString = showVersion version

-- | A data structure that contains the `B9Invokation`
-- as well as build parameters.
data B9RunParameters a =
  B9RunParameters
            B9ConfigOverride
            (B9Invokation a ())
            BuildVariables

-- | Run a b9 build.
-- Return `True` if the build was successful.
runB9 :: B9RunParameters a -> IO (Maybe a)
runB9 (B9RunParameters overrides action vars) = do
  invokeB9 $ do
    modifyInvokationConfig
      (over envVars (++ vars) . const (overrides ^. customB9Config))
    mapM_ overrideB9ConfigPath (overrides ^. customB9ConfigPath)
    action

defaultB9RunParameters :: B9Invokation a () -> B9RunParameters a
defaultB9RunParameters ba =
  B9RunParameters (B9ConfigOverride Nothing mempty) ba mempty

runShowVersion :: B9Invokation Bool ()
runShowVersion = doAfterConfiguration $ const $ do
  liftIO $ putStrLn b9VersionString
  return True

runBuildArtifacts :: [FilePath] -> B9Invokation String ()
runBuildArtifacts buildFiles = do
  generators <- mapM consult buildFiles
  buildArtifacts (mconcat generators)

runFormatBuildFiles :: [FilePath] -> B9Invokation Bool ()
runFormatBuildFiles buildFiles = doAfterConfiguration $ const $ liftIO $ do
  generators <- mapM consult buildFiles
  let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
  putStrLn `mapM` (generatorsFormatted)
  (uncurry writeFile) `mapM` (buildFiles `zip` generatorsFormatted)
  return True

runPush :: SharedImageName -> B9Invokation Bool ()
runPush name = do
  modifyInvokationConfig (keepTempDirs .~ False)
  run $ const $ do
    conf <- getConfig
    if not (isJust (conf ^. repository))
      then do
        errorL
          "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
        return False
      else do
        pushSharedImageLatestVersion name
        return True


runPull :: Maybe SharedImageName -> B9Invokation Bool ()
runPull mName = do
  modifyInvokationConfig (keepTempDirs .~ False)
  run (const (pullRemoteRepos >> maybePullImage))
  where maybePullImage = maybe (return True) pullLatestImage mName

runRun :: SharedImageName -> [String] -> B9Invokation String ()
runRun (SharedImageName name) cmdAndArgs = do
  modifyInvokationConfig
    (Lens.set keepTempDirs False . Lens.set interactive True)
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


runGcLocalRepoCache :: B9Invokation Bool ()
runGcLocalRepoCache = do
  modifyInvokationConfig (keepTempDirs .~ False)
  impl
 where
  impl = run $ const $ do
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

runGcRemoteRepoCache :: B9Invokation Bool ()
runGcRemoteRepoCache = do
  modifyInvokationConfig (keepTempDirs .~ False)
  run $ const $ do
    repos <- getSelectedRepos
    cache <- getRepoCache
    mapM_ (cleanRemoteRepo cache) repos
    return True

runListSharedImages :: B9Invokation [SharedImage] ()
runListSharedImages = do
  modifyInvokationConfig (keepTempDirs .~ False)
  run $ const $ do
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
    return $ map snd imgs


runAddRepo :: RemoteRepo -> B9Invokation Bool ()
runAddRepo repo = do
  repo' <- remoteRepoCheckSshPrivKey repo
  modifyPermanentConfig
    (  remoteRepos
    %~ ( mappend [repo']
       . filter ((== remoteRepoRepoId repo') . remoteRepoRepoId)
       )
    )

runLookupLocalSharedImage
  :: SharedImageName -> B9Invokation (Maybe SharedImageBuildId) ()
runLookupLocalSharedImage n = run $ const $ do
  traceL (printf "Searching for cached image: %s" (show n))
  imgs <- lookupSharedImages isAvailableOnLocalHost hasTheDesiredName
  traceL "Candidate images: "
  traceL (printf "%s\n" (prettyPrintSharedImages (map snd imgs)))
  let res = extractNewestImageFromResults imgs
  traceL (printf "Returning result: %s" (show res))
  return res
 where
  extractNewestImageFromResults =
    listToMaybe . map toBuildId . take 1 . reverse . map snd
    where toBuildId (SharedImage _ _ i _ _) = i
  isAvailableOnLocalHost = (Cache ==)
  hasTheDesiredName (SharedImage n' _ _ _ _) = n == n'