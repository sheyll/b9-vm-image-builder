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
    Repository (..),
    RepoImagesMap,
    toRemoteRepository,
    SelectedRemoteRepo (..),
    initRemoteRepo,
    cleanRemoteRepo,
    remoteRepoCheckSshPrivKey,
    remoteRepoCacheDir,
    localRepoDir,
    lookupRemoteRepo,
    filterRepoImagesMap,
    lookupCachedImages,
    allCachedSharedImages,
    allSharedImagesInRepo,
    sharedRepoImagesMax,
    allSharedImages,
    allRepositories,
    module X,
  )
where

import B9.B9Config
import B9.B9Config.Repository as X
import B9.B9Error
import B9.DiskImages
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import System.Directory
import System.FilePath
import System.IO.B9Extras
import Test.QuickCheck
import Text.Printf

data Repository
  = Cache
  | Remote String -- TODO use a newtype
  deriving (Eq, Ord, Read, Show, Generic)

instance Arbitrary Repository where
  arbitrary =
    oneof [pure Cache, Remote <$> listOf1 (choose ('A', 'z'))]

instance Function Repository

instance CoArbitrary Repository

-- | Convert a `RemoteRepo` down to a mere `Repository`
toRemoteRepository :: RemoteRepo -> Repository
toRemoteRepository = Remote . remoteRepoRepoId

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

-- | A 'Map' that maps 'Repository's to the 'SharedImage's they hold.
--
-- @since 1.1.0
type RepoImagesMap = Map Repository (Set SharedImage)

-- | Filter the 'SharedImage's returned by 'getSharedImages'
-- using a 'Repository'-, and a 'SharedImage' predicate.
--
-- @since 1.1.0
filterRepoImagesMap ::
  (Repository -> Bool) ->
  (SharedImage -> Bool) ->
  RepoImagesMap ->
  RepoImagesMap
filterRepoImagesMap repoPred imgPred =
  Map.map (Set.filter imgPred)
    . Map.filterWithKey (const . repoPred)

-- | Return the versions of a shared image named 'name' from the local cache.
--
-- @since 1.1.0
lookupCachedImages ::
  SharedImageName ->
  RepoImagesMap ->
  Set SharedImage -- TODO: test
lookupCachedImages name =
  allCachedSharedImages
    . filterRepoImagesMap (== Cache) ((== name) . sharedImageName)

-- | Get a 'Set' of all 'SharedImage's in all 'Repository's.
--
-- @since 1.1.0
allSharedImages :: RepoImagesMap -> Set SharedImage
allSharedImages = fold

-- | Return the 'SharedImage's, that are contained in a 'Repository'.
--
-- @since 1.1.0
allSharedImagesInRepo :: Repository -> RepoImagesMap -> Set SharedImage
allSharedImagesInRepo repo = fromMaybe Set.empty . Map.lookup repo -- TODO: test

-- | Keep 'SharedImage's that are in the 'Cache' 'Repository'.
--
-- @since 1.1.0
allCachedSharedImages ::
  RepoImagesMap ->
  Set SharedImage -- TODO: test
allCachedSharedImages = allSharedImagesInRepo Cache

-- | Return the maximum with regard to the 'Ord' instance of 'SharedImage'
-- from an 'RepoImagesMap'
--
-- @since 1.1.0
sharedRepoImagesMax :: RepoImagesMap -> Maybe (Repository, SharedImage)
sharedRepoImagesMax = error "TODO" -- TODO: test

-- | Return a 'Set' of 'Repository' names from a 'RepoImagesMap'
--
-- @since 1.1.0
allRepositories :: RepoImagesMap -> Set Repository
allRepositories = Map.keysSet
