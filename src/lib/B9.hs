{-| B9 is a library and build tool with primitive operations to rmrun a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.Artifact.Generator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 ( b9Version, b9VersionString
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
import Control.Exception(throwIO, catch)
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X (ReaderT, local, ask)
import Control.Lens as X (Lens, (^.), (.~), (%~), (&))
import System.IO.Error (isDoesNotExistError)
import System.Directory (removeFile)
import Data.Function (on)
import Data.List as X
import Data.Maybe as X
import Data.Monoid as X
import Text.Show.Pretty as X (ppShow)
import System.Exit as X (exitWith, ExitCode(..))
import System.FilePath as X
        (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import Text.Printf as X (printf)
import Data.Version as X
import System.IO.B9Extras as X
import B9.Artifact as X
import B9.Artifact.Readable as X
import B9.Artifact.Content as X
import B9.Artifact.Content.AST as X
import B9.Artifact.Content.CloudConfigYaml as X
import B9.Artifact.Content.ErlTerms as X
import B9.Artifact.Content.ErlangPropList as X
import B9.Artifact.Content.Readable as X
import B9.Artifact.Content.StringTemplate as X
import B9.Artifact.Content.YamlObject as X
import B9.Artifact.Readable.Source as X
import B9.Artifact.Readable.Interpreter as X
import B9.B9Monad as X
import B9.B9Config as X
import B9.ExecEnv as X
import B9.DiskImages as X
import B9.DiskImageBuilder as X
import B9.ShellScript as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.Vm as X
import B9.VmBuilder as X
import B9.QCUtil as X
import B9.Environment as X
import Paths_b9 (version)

-- | Return the cabal package version of the B9 library.
b9Version :: Version
b9Version = version

-- | Return the cabal package version of the B9 library,
-- formatted using `showVersion`.
b9VersionString :: String
b9VersionString = showVersion version

-- | Just print the 'b9VersionString'
runShowVersion :: MonadIO m => m ()
runShowVersion = liftIO $ putStrLn b9VersionString

-- | Execute the artifact generators defined in a list of text files.
-- Read the text files in the list and parse them as 'ArtifactGenerator's
-- then 'mappend' them and apply 'buildArtifacts' to them.
runBuildArtifacts :: MonadIO m => [FilePath] -> B9ConfigAction m String
runBuildArtifacts buildFiles = do
  generators <- mapM consult buildFiles
  run (buildArtifacts (mconcat generators))

-- | Read all text files and parse them as 'ArtifactGenerator's.
-- Then overwrite the files with their contents but _pretty printed_
-- (i.e. formatted).
runFormatBuildFiles :: MonadIO m => [FilePath] -> m ()
runFormatBuildFiles buildFiles = liftIO $ do
  generators <- mapM consult buildFiles
  let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
  putStrLn `mapM` generatorsFormatted
  zipWithM_ writeFile buildFiles generatorsFormatted

-- | Upload a 'SharedImageName' to the default remote repository.
-- Note: The remote repository is specified in the 'B9Config'.
runPush :: MonadIO m => SharedImageName -> B9ConfigAction m ()
runPush name = localRuntimeConfig (keepTempDirs .~ False) $ run $ do
  conf <- getConfig
  if isNothing (conf ^. repository)
    then errorExitL
      "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
    else pushSharedImageLatestVersion name

-- | Either pull a list of available 'SharedImageName's from the remote
-- repository if 'Nothing' is passed as parameter, or pull the latest version
-- of the image from the remote repository. Note: The remote repository is
-- specified in the 'B9Config'.
runPull :: MonadIO m => Maybe SharedImageName -> B9ConfigAction m ()
runPull mName = localRuntimeConfig (keepTempDirs .~ False)
                                   (run (pullRemoteRepos >> maybePullImage))
 where
  maybePullImage = mapM_
    ( \name ->
      pullLatestImage name >>= maybe (failPull name) (const (return ()))
    )
    mName
  failPull name = errorExitL (printf "failed to pull: %s" (show name))

-- | Execute an interactive root shell in a running container from a
-- 'SharedImageName'.
runRun :: MonadIO m => SharedImageName -> [String] -> B9ConfigAction m String
runRun (SharedImageName name) cmdAndArgs = localRuntimeConfig
  ((keepTempDirs .~ False) . (interactive .~ True))
  (run (buildArtifacts runCmdAndArgs))
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

-- | Delete all obsolete versions of all 'SharedImageName's.
runGcLocalRepoCache :: MonadIO m => B9ConfigAction m ()
runGcLocalRepoCache = localRuntimeConfig (keepTempDirs .~ False) (run impl)
 where
  impl = do
    toDelete <- obsoleteSharedmages . map snd <$> lookupSharedImages
      (== Cache)
      (const True)
    imgDir <- getSharedImagesCacheDir
    let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
        infoFiles     = sharedImageFileName <$> toDelete
        imgFiles      = imageFileName . sharedImageImage <$> toDelete
    if null filesToDelete
      then liftIO $ putStrLn "\n\nNO IMAGES TO DELETE\n"
      else liftIO $ do
        putStrLn "DELETING FILES:"
        putStrLn (unlines filesToDelete)
        mapM_ removeIfExists filesToDelete
   where
    obsoleteSharedmages :: [SharedImage] -> [SharedImage]
    obsoleteSharedmages =
      concatMap (tail . reverse) . filter ((> 1) . length) . groupBy
        ((==) `on` sharedImageName)
    removeIfExists :: FilePath -> IO ()
    removeIfExists fileName = removeFile fileName `catch` handleExists
     where
      handleExists e | isDoesNotExistError e = return ()
                     | otherwise             = throwIO e

-- | Clear the shared image cache for a remote. Note: The remote repository is
-- specified in the 'B9Config'.
runGcRemoteRepoCache :: MonadIO m => B9ConfigAction m ()
runGcRemoteRepoCache = localRuntimeConfig
  (keepTempDirs .~ False)
  ( run
    ( do
      repos <- getSelectedRepos
      cache <- getRepoCache
      mapM_ (cleanRemoteRepo cache) repos
    )
  )

-- | Print a list of shared images cached locally or remotely, if a remote
-- repository was selected. Note: The remote repository is
-- specified in the 'B9Config'.
runListSharedImages :: MonadIO m => B9ConfigAction m [SharedImage]
runListSharedImages = localRuntimeConfig
  (keepTempDirs .~ False)
  ( run
    ( do
      remoteRepo <- getSelectedRemoteRepo
      let repoPred = maybe (== Cache) ((==) . toRemoteRepository) remoteRepo
      allRepos <- getRemoteRepos
      if isNothing remoteRepo
        then liftIO $ do
          putStrLn "Showing local shared images only.\n"
          putStrLn "To view the contents of a remote repo add"
          putStrLn "the '-r' switch with one of the remote"
          putStrLn "repository ids."
        else liftIO $ putStrLn
          (  "Showing shared images on: "
          ++ remoteRepoRepoId (fromJust remoteRepo)
          )
      unless (null allRepos) $ liftIO $ do
        putStrLn "\nAvailable remote repositories:"
        mapM_ (putStrLn . (" * " ++) . remoteRepoRepoId) allRepos
      imgs <- lookupSharedImages repoPred (const True)
      if null imgs
        then liftIO $ putStrLn "\n\nNO SHARED IMAGES\n"
        else liftIO $ do
          putStrLn ""
          putStrLn $ prettyPrintSharedImages $ map snd imgs
      return (map snd imgs)
    )
  )

-- | Check the SSH settings for a remote repository and add it to the user wide
-- B9 configuration file.
runAddRepo :: MonadIO m => RemoteRepo -> B9ConfigAction m ()
runAddRepo repo = do
  repo' <- remoteRepoCheckSshPrivKey repo
  modifyPermanentConfig
    ( Endo
      (  remoteRepos
      %~ ( mappend [repo']
         . filter ((== remoteRepoRepoId repo') . remoteRepoRepoId)
         )
      )
    )

-- | Find the most recent version of a 'SharedImageName' in the local image cache.
runLookupLocalSharedImage
  :: MonadIO m => SharedImageName -> B9ConfigAction m (Maybe SharedImageBuildId)
runLookupLocalSharedImage n = run $ do
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
