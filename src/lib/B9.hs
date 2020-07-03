-- | B9 is a library and build tool with which one can create/convert different types
--    of VM images. Additionally installation steps - like installing software -
--    can be done in a LXC container, running on the disk images.
--
--    B9 allows to create and convert virtual machine image files as well as
--    related ISO and VFAT disk images for e.g. cloud-init configuration sources.
--
--    This module re-exports the modules needed to build a tool around the
--    library, e.g. see @src\/cli\/Main.hs@ as an example.
--
--    "B9.Artifact.Generator" is the module containing the basic data structure
--    used to describe a B9 build.
module B9
  ( b9Version,
    b9VersionString,
    runShowVersion,
    runBuildArtifacts,
    runFormatBuildFiles,
    runPush,
    runPull,
    runRun,
    runGcLocalRepoCache,
    runGcRemoteRepoCache,
    runListSharedImages,
    runAddRepo,
    runLookupLocalSharedImage,
    module X,
  )
where

import B9.Artifact.Content as X
import B9.Artifact.Content.AST as X
import B9.Artifact.Content.CloudConfigYaml as X
import B9.Artifact.Content.ErlTerms as X
import B9.Artifact.Content.ErlangPropList as X
import B9.Artifact.Content.Readable as X
import B9.Artifact.Content.StringTemplate as X
import B9.Artifact.Content.YamlObject as X
import B9.Artifact.Readable as X
import B9.Artifact.Readable.Interpreter as X
import B9.B9Config as X
import B9.B9Error as X
import B9.B9Exec as X
import B9.B9Logging as X
import B9.B9Monad as X
import B9.BuildInfo as X
import B9.DiskImageBuilder as X
import B9.DiskImages as X
import B9.Environment as X
import B9.ExecEnv as X
import B9.QCUtil as X
import B9.Repository as X
import B9.RepositoryIO as X
import B9.ShellScript as X
import B9.Text as X
import B9.Vm as X
import B9.VmBuilder as X
import Control.Applicative as X
import Control.Lens as X
  ( (%~),
    (&),
    (.~),
    Lens,
    (^.),
  )
import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X
  ( ReaderT,
    ask,
    local,
  )
import Data.Foldable (fold)
import Data.List as X
import Data.Maybe as X
import Data.Monoid as X
import Data.Set (Set)
import Data.Version as X
import Paths_b9 (version)
import System.Exit as X
  ( ExitCode (..),
    exitWith,
  )
import System.FilePath as X
  ( (<.>),
    (</>),
    replaceExtension,
    takeDirectory,
    takeFileName,
  )
import System.IO.B9Extras as X
import Text.Printf as X
  ( printf,
  )
import Text.Show.Pretty as X
  ( ppShow,
  )

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
runBuildArtifacts :: [FilePath] -> B9ConfigAction String
runBuildArtifacts buildFiles = do
  generators <- mapM consult buildFiles
  runB9 (buildArtifacts (mconcat generators))

-- | Read all text files and parse them as 'ArtifactGenerator's.
-- Then overwrite the files with their contents but _pretty printed_
-- (i.e. formatted).
runFormatBuildFiles :: MonadIO m => [FilePath] -> m ()
runFormatBuildFiles buildFiles = liftIO $ do
  generators <- mapM consult buildFiles
  let generatorsFormatted = map ppShow (generators :: [ArtifactGenerator])
  putStrLn `mapM_` generatorsFormatted
  zipWithM_ writeFile buildFiles generatorsFormatted

-- | Upload a 'SharedImageName' to the default remote repository.
-- Note: The remote repository is specified in the 'B9Config'.
runPush :: SharedImageName -> B9ConfigAction ()
runPush name = localB9Config (keepTempDirs .~ False) $ runB9 $ do
  conf <- getConfig
  if isNothing (conf ^. repository)
    then
      errorExitL
        "No repository specified! Use '-r' to specify a repo BEFORE 'push'."
    else pushSharedImageLatestVersion name

-- | Either pull a list of available 'SharedImageName's from the remote
-- repository if 'Nothing' is passed as parameter, or pull the latest version
-- of the image from the remote repository. Note: The remote repository is
-- specified in the 'B9Config'.
runPull :: Maybe SharedImageName -> B9ConfigAction ()
runPull mName =
  localB9Config
    (keepTempDirs .~ False)
    (runB9 (pullRemoteRepos >> maybePullImage))
  where
    maybePullImage =
      mapM_
        ( \name -> pullLatestImage name >>= maybe (failPull name) (const (return ()))
        )
        mName
    failPull name = errorExitL (printf "failed to pull: %s" (show name))

-- | Execute an interactive root shell in a running container from a
-- 'SharedImageName'.
runRun :: SharedImageName -> [String] -> B9ConfigAction String
runRun (SharedImageName name) cmdAndArgs =
  localB9Config
    ((keepTempDirs .~ False) . (interactive .~ True))
    (runB9 (buildArtifacts runCmdAndArgs))
  where
    runCmdAndArgs =
      Artifact
        (IID ("run-" ++ name))
        ( VmImages
            [ImageTarget Transient (From name KeepSize) (MountPoint "/")]
            ( VmScript
                X86_64
                [SharedDirectory "." (MountPoint "/mnt/CWD")]
                (Run (head cmdAndArgs') (tail cmdAndArgs'))
            )
        )
      where
        cmdAndArgs' = if null cmdAndArgs then ["/usr/bin/zsh"] else cmdAndArgs

-- | Delete all obsolete versions of all 'SharedImageName's.
runGcLocalRepoCache :: B9ConfigAction ()
runGcLocalRepoCache = localB9Config (keepTempDirs .~ False) (runB9 cleanLocalRepoCache)

-- TODO delete-too-many-revisions

-- | Clear the shared image cache for a remote. Note: The remote repository is
-- specified in the 'B9Config'.
runGcRemoteRepoCache :: B9ConfigAction ()
runGcRemoteRepoCache =
  localB9Config
    (keepTempDirs .~ False)
    ( runB9
        ( do
            repos <- getSelectedRepos
            cache <- getRepoCache
            mapM_ (cleanRemoteRepo cache) repos
        )
    )

-- | Print a list of shared images cached locally or remotely, if a remote
-- repository was selected. Note: The remote repository is
-- specified in the 'B9Config'.
runListSharedImages :: B9ConfigAction (Set SharedImage)
runListSharedImages =
  localB9Config
    (keepTempDirs .~ False)
    ( runB9
        ( do
            MkSelectedRemoteRepo remoteRepo <- getSelectedRemoteRepo
            let repoPred = maybe (== Cache) ((==) . toRemoteRepository) remoteRepo
            allRepos <- getRemoteRepos
            if isNothing remoteRepo
              then liftIO $ do
                putStrLn "Showing local shared images only.\n"
                putStrLn "To view the contents of a remote repo add"
                putStrLn "the '-r' switch with one of the remote"
                putStrLn "repository ids."
              else
                liftIO $
                  putStrLn
                    ( "Showing shared images on: "
                        ++ remoteRepoRepoId (fromJust remoteRepo)
                    )
            unless (null allRepos) $ liftIO $ do
              putStrLn "\nAvailable remote repositories:"
              mapM_ (putStrLn . (" * " ++) . remoteRepoRepoId) allRepos
            imgs <-
              fold
                . filterRepoImagesMap repoPred (const True)
                <$> getSharedImages
            if null imgs
              then liftIO $ putStrLn "\n\nNO SHARED IMAGES\n"
              else liftIO $ do
                putStrLn ""
                putStrLn $ prettyPrintSharedImages imgs
            return imgs
        )
    )

-- | Check the SSH settings for a remote repository and add it to the user wide
-- B9 configuration file.
runAddRepo :: RemoteRepo -> B9ConfigAction ()
runAddRepo repo = do
  repo' <- remoteRepoCheckSshPrivKey repo
  modifyPermanentConfig
    ( Endo
        ( remoteRepos
            %~ ( mappend [repo']
                   . filter ((== remoteRepoRepoId repo') . remoteRepoRepoId)
               )
        )
    )

-- | Find the most recent version of a 'SharedImageName' in the local image cache.
runLookupLocalSharedImage ::
  SharedImageName -> B9ConfigAction (Maybe SharedImageBuildId)
runLookupLocalSharedImage n = runB9 $ do
  traceL (printf "Searching for cached image: %s" (show n))
  imgs <- lookupCachedImages n <$> getSharedImages
  traceL "Candidate images: "
  traceL (printf "%s\n" (prettyPrintSharedImages imgs))
  let res = if null imgs then Nothing else Just (sharedImageBuildId (maximum imgs))
  traceL (printf "Returning result: %s" (show res))
  return res
