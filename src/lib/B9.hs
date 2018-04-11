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
import B9.B9Monad as X
import Data.ConfigFile.B9Extras as X
import B9.B9Config as X
import B9.ExecEnv as X
import B9.DiskImages as X
import B9.DiskImageBuilder as X
import B9.Invokation as X
import B9.ShellScript as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.ArtifactGenerator as X
import B9.ArtifactGeneratorImpl as X
import B9.Vm as X
import B9.VmBuilder as X
import B9.QCUtil as X
import B9.Content.AST as X
import B9.Content.StringTemplate as X
import B9.Content.ErlTerms as X
import B9.Content.ErlangPropList as X
import B9.Content.YamlObject as X
import B9.Content.Generator as X
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

runBuildArtifacts :: [FilePath] -> ReaderT B9Config IO String
runBuildArtifacts buildFiles = do
  generators <- mapM consult buildFiles
  run (buildArtifacts (mconcat generators))

runFormatBuildFiles :: MonadIO m => [FilePath] -> m ()
runFormatBuildFiles buildFiles = liftIO $ do
  generators <- mapM consult buildFiles
  let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
  putStrLn `mapM` generatorsFormatted
  zipWithM_ writeFile buildFiles generatorsFormatted

runPush :: SharedImageName -> ReaderT B9Config IO Bool
runPush name = local (keepTempDirs .~ False) $ run $ do
  conf <- getConfig
  if isNothing (conf ^. repository)
    then do
      errorL
        "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
      return False
    else do
      pushSharedImageLatestVersion name
      return True

runPull :: Maybe SharedImageName -> ReaderT B9Config IO Bool
runPull mName = local (keepTempDirs .~ False)
                      (run (pullRemoteRepos >> maybePullImage))
  where maybePullImage = maybe (return True) pullLatestImage mName

runRun :: SharedImageName -> [String] -> ReaderT B9Config IO String
runRun (SharedImageName name) cmdAndArgs = local
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


runGcLocalRepoCache :: ReaderT B9Config IO Bool
runGcLocalRepoCache = local (keepTempDirs .~ False) (run impl)
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

runGcRemoteRepoCache :: ReaderT B9Config IO ()
runGcRemoteRepoCache = local
  (keepTempDirs .~ False)
  ( run
    ( do
      repos <- getSelectedRepos
      cache <- getRepoCache
      mapM_ (cleanRemoteRepo cache) repos
    )
  )

runListSharedImages :: ReaderT B9Config IO [SharedImage]
runListSharedImages = local
  (keepTempDirs .~ False)
  ( run
    ( do
      remoteRepo <- getSelectedRemoteRepo
      let repoPred = maybe (== Cache) ((==) . toRemoteRepository) remoteRepo
      allRepos <- getRemoteRepos
      if isNothing remoteRepo
        then liftIO $ do
          putStrLn "Showing local shared images only."
          putStrLn
            "\nTo view the contents of a remote repo add \n\
                        \the '-r' switch with one of the remote \n\
                        \repository ids."
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


runAddRepo :: RemoteRepo -> ReaderT B9Config IO B9Config
runAddRepo repo = do
  repo' <- remoteRepoCheckSshPrivKey repo
  cfg   <- ask
  return
    (  cfg
    &  remoteRepos
    %~ ( mappend [repo']
       . filter ((== remoteRepoRepoId repo') . remoteRepoRepoId)
       )
    )

runLookupLocalSharedImage
  :: SharedImageName -> ReaderT B9Config IO (Maybe SharedImageBuildId)
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



