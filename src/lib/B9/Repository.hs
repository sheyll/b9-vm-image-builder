-- | B9 has a concept of 'B9.DiskImages.SharedImaged'. Shared images can be pulled and
-- pushed to/from remote locations via rsync+ssh. B9 also maintains a local cache;
-- the whole thing is supposed to be build-server-safe, that means no two builds
-- shall interfere with each other. This is accomplished by refraining from
-- automatic cache updates from/to remote repositories.
module B9.Repository
  ( initRepoCache,
    RepoCacheReader,
    getRepoCache,
    withRemoteRepos,
    withSelectedRemoteRepo,
    getSelectedRemoteRepo,
    SelectedRemoteRepoReader,
    SelectedRemoteRepo (..),
    initRemoteRepo,
    cleanRemoteRepo,
    remoteRepoCheckSshPrivKey,
    remoteRepoCacheDir,
    localRepoDir,
    lookupRemoteRepo,
    module X,
  )
where

import B9.B9Config
import B9.B9Config.Repository as X
import B9.B9Error
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.B9Extras
import Text.Printf

-- | Initialize the local repository cache directory and the 'RemoteRepo's.
-- Run the given action with a 'B9Config' that contains the initialized
-- repositories in '_remoteRepos'.
--
-- @since 0.5.65
withRemoteRepos ::
  (Member B9ConfigReader e, Lifted IO e) =>
  Eff (RepoCacheReader ': e) a ->
  Eff e a
withRemoteRepos f = do
  cfg <- getB9Config
  repoCache <-
    lift
      (initRepoCache (fromMaybe defaultRepositoryCache (_repositoryCache cfg)))
  remoteRepos' <- mapM (initRemoteRepo repoCache) (_remoteRepos cfg)
  let setRemoteRepos = remoteRepos .~ remoteRepos'
  localB9Config setRemoteRepos (runReader repoCache f)

-- | Alias for a 'Reader' 'Eff'ect that reads a list of 'RemoteRepo's.
--
-- @since 0.5.65
type RepoCacheReader = Reader RepoCache

-- | Ask for the 'RepoCache' initialized by 'withRemoteRepos'.
--
-- @since 0.5.65
getRepoCache :: Member RepoCacheReader e => Eff e RepoCache
getRepoCache = ask

-- | Run a 'SelectedRemoteRepoReader' with the 'SelectedRemoteRepo' selected
-- in the 'B9Config'.
--
-- If the selected repo does not exist, and exception is thrown.
--
-- @since 0.5.65
withSelectedRemoteRepo ::
  (Member B9ConfigReader e, Member ExcB9 e) =>
  Eff (SelectedRemoteRepoReader ': e) a ->
  Eff e a
withSelectedRemoteRepo e = do
  remoteRepos' <- _remoteRepos <$> getB9Config
  mSelectedRepoName <- _repository <$> getB9Config
  case mSelectedRepoName of
    Nothing -> runReader (MkSelectedRemoteRepo Nothing) e
    Just selectedRepoName ->
      case lookupRemoteRepo remoteRepos' selectedRepoName of
        Nothing ->
          throwB9Error
            ( printf
                "selected remote repo '%s' not configured, valid remote repos are: '%s'"
                (show selectedRepoName)
                (show remoteRepos')
            )
        Just r -> runReader (MkSelectedRemoteRepo (Just r)) e

-- | Contains the 'Just' the 'RemoteRepo' selected by the 'B9Config' value '_repository',
-- or 'Nothing' of no 'RemoteRepo' was selected in the 'B9Config'.
--
-- @since 0.5.65
newtype SelectedRemoteRepo = MkSelectedRemoteRepo {fromSelectedRemoteRepo :: Maybe RemoteRepo}

-- | Alias for a 'Reader' 'Eff'ect that reads the 'RemoteRepo'
-- selected by the 'B9Config' value '_repository'. See 'withSelectedRemoteRepo'.
--
-- @since 0.5.65
type SelectedRemoteRepoReader = Reader SelectedRemoteRepo

-- | Ask for the 'RemoteRepo'
-- selected by the 'B9Config' value '_repository'. See 'withSelectedRemoteRepo'.
--
-- @since 0.5.65
getSelectedRemoteRepo ::
  Member SelectedRemoteRepoReader e => Eff e SelectedRemoteRepo
getSelectedRemoteRepo = ask

-- | Initialize the local repository cache directory.
initRepoCache :: MonadIO m => SystemPath -> m RepoCache
initRepoCache repoDirSystemPath = do
  repoDir <- resolve repoDirSystemPath
  ensureDir (repoDir ++ "/")
  return (RepoCache repoDir)

-- | Check for existance of priv-key and make it an absolute path.
remoteRepoCheckSshPrivKey :: MonadIO m => RemoteRepo -> m RemoteRepo
remoteRepoCheckSshPrivKey (RemoteRepo rId rp (SshPrivKey keyFile) h u) = do
  exists <- liftIO (doesFileExist keyFile)
  keyFile' <- liftIO (canonicalizePath keyFile)
  unless
    exists
    ( error
        (printf "SSH Key file '%s' for repository '%s' is missing." keyFile' rId)
    )
  return (RemoteRepo rId rp (SshPrivKey keyFile') h u)

-- | Initialize the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
initRemoteRepo :: MonadIO m => RepoCache -> RemoteRepo -> m RemoteRepo
initRemoteRepo cache repo = do
  -- TODO logging traceL $ printf "Initializing remote repo: %s" (remoteRepoRepoId repo)
  repo' <- remoteRepoCheckSshPrivKey repo
  let (RemoteRepo repoId _ _ _ _) = repo'
  ensureDir (remoteRepoCacheDir cache repoId ++ "/")
  return repo'

-- | Empty the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
cleanRemoteRepo :: MonadIO m => RepoCache -> RemoteRepo -> m ()
cleanRemoteRepo cache repo = do
  let repoId = remoteRepoRepoId repo
      repoDir = remoteRepoCacheDir cache repoId ++ "/"
  -- TODO logging infoL $ printf "Cleaning remote repo: %s" repoId
  ensureDir repoDir
  -- TODO logging traceL $ printf "Deleting directory: %s" repoDir
  liftIO $ removeDirectoryRecursive repoDir
  ensureDir repoDir

-- | Return the cache directory for a remote repository relative to the root
-- cache dir.
remoteRepoCacheDir ::
  -- | The repository cache directory
  RepoCache ->
  -- | Id of the repository
  String ->
  -- | The existing, absolute path to the
  -- cache directory
  FilePath
remoteRepoCacheDir (RepoCache cacheDir) repoId =
  cacheDir </> "remote-repos" </> repoId

-- | Return the local repository directory.
localRepoDir ::
  -- | The repository cache directory
  RepoCache ->
  -- | The existing, absolute path to the
  --  directory
  FilePath
localRepoDir (RepoCache cacheDir) = cacheDir </> "local-repo"

-- | Select the first 'RemoteRepo' with a given @repoId@.
lookupRemoteRepo :: [RemoteRepo] -> String -> Maybe RemoteRepo
lookupRemoteRepo repos repoId = lookup repoId repoIdRepoPairs
  where
    repoIdRepoPairs = map (\r@(RemoteRepo rid _ _ _ _) -> (rid, r)) repos
