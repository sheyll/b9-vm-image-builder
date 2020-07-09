-- | B9 has a concept of 'B9.DiskImages.SharedImaged'. Shared images can be pulled and
-- pushed to/from remote locations via rsync+ssh. B9 also maintains a local cache;
-- the whole thing is supposed to be build-server-safe, that means no two builds
-- shall interfere with each other. This is accomplished by refraining from
-- automatic cache updates from/to remote repositories.
module B9.Repository
  ( RepoCacheReader,
    getRepoCache,
    withSelectedRemoteRepo,
    getSelectedRemoteRepo,
    SelectedRemoteRepoReader,
    Repository (..),
    RepoImagesMap,
    toRemoteRepository,
    SelectedRemoteRepo (..),
    remoteRepoCacheDir,
    localRepoDir,
    lookupRemoteRepo,
    filterRepoImagesMap,
    lookupCachedImages,
    allCachedSharedImages,
    allSharedImagesWithRepo,
    maxSharedImageOfAllRepos,
    allSharedImagesInRepo,
    allSharedImages,
    allRepositories,
    groupBySharedImageName,
    keepNLatestSharedImages,
    dropAllButNLatestSharedImages,
    module X,
  )
where

import B9.B9Config
import B9.B9Config.Repository as X
import B9.B9Error
import B9.DiskImages
import Control.Eff
import Control.Eff.Reader.Lazy
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import System.FilePath
import Test.QuickCheck
import Text.Printf

data Repository
  = Cache
  | Remote String -- TODO use a newtype
  deriving (Eq, Ord, Read, Show, Generic)

instance Arbitrary Repository where
  arbitrary =
    Test.QuickCheck.oneof
      [ pure Cache,
        Remote . printf "remote-repo-%0X" <$> choose (0, 31 :: Int)
      ]

instance Function Repository

instance CoArbitrary Repository

-- | Convert a `RemoteRepo` down to a mere `Repository`
toRemoteRepository :: RemoteRepo -> Repository
toRemoteRepository = Remote . remoteRepoRepoId

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
  Set SharedImage
lookupCachedImages name =
  allSharedImages
    . filterRepoImagesMap (== Cache) ((== name) . sharedImageName)

-- | Return a 'Set' of 'Repository' names from a 'RepoImagesMap'
--
-- @since 1.1.0
allRepositories :: RepoImagesMap -> Set Repository
allRepositories = Map.keysSet

-- | Get a 'Set' of all 'SharedImage's in all 'Repository's.
--
-- @since 1.1.0
allSharedImages :: RepoImagesMap -> Set SharedImage
allSharedImages = fold

-- | Fetch all 'SharedImage's like 'allSharedImages' but attach
-- the 'Repository' that the image belongs to.
--
-- Usage example: In combination with 'filterRepoImagesMap' to find
-- the latest version of a certain image in all known repositories.
--
-- @since 1.1.0
allSharedImagesWithRepo :: RepoImagesMap -> Set (SharedImage, Repository)
allSharedImagesWithRepo = Map.foldMapWithKey (Set.map . flip (,))

-- | Return the maximum with regard to the 'Ord' instance of 'SharedImage'
-- from an 'RepoImagesMap'
--
-- @since 1.1.0
maxSharedImageOfAllRepos :: RepoImagesMap -> Maybe (SharedImage, Repository)
maxSharedImageOfAllRepos = Set.lookupMax . allSharedImagesWithRepo

-- | Return the 'SharedImage's, that are contained in a 'Repository'.
--
-- @since 1.1.0
allSharedImagesInRepo :: Repository -> RepoImagesMap -> Set SharedImage
allSharedImagesInRepo repo = fromMaybe Set.empty . Map.lookup repo

-- | Keep 'SharedImage's that are in the 'Cache' 'Repository'.
--
-- @since 1.1.0
allCachedSharedImages ::
  RepoImagesMap ->
  Set SharedImage
allCachedSharedImages = allSharedImagesInRepo Cache

-- | Take a subset that contains the @n@
-- latest versions of 'SharedImage's with the same name.
--
-- For example, if the input contains:
--
-- @@@
-- fromList
-- [ SharedImage "foo" "2020-07-07 13:34:31"
-- , SharedImage "foo" "2020-07-07 13:34:32"
-- , SharedImage "foo" "2020-07-07 13:34:33"
-- , SharedImage "bar" "2020-07-07 13:34:34"
-- , SharedImage "bar" "2020-07-07 13:34:35"
-- , SharedImage "bar" "2020-07-07 13:34:36"
-- ]
-- @@@
--
-- The output of @keepNLatestSharedImages 2@ will be:
--
-- @@@
-- fromList
-- [ SharedImage "foo" "2020-07-07 13:34:32"
-- , SharedImage "foo" "2020-07-07 13:34:33"
-- , SharedImage "bar" "2020-07-07 13:34:35"
-- , SharedImage "bar" "2020-07-07 13:34:36"
-- ]
-- @@@
--
-- @since 1.1.0
keepNLatestSharedImages :: Int -> Set SharedImage -> Set SharedImage
keepNLatestSharedImages n =
  fold
    . Map.map
      ( \s ->
          let nOld = max 0 (length s - n)
           in Set.drop nOld s
      )
    . groupBySharedImageName

-- | Take a subset that contains obsolete images.
--
-- Do the opposite of 'keepNLatestSharedImages',
-- and return all **but** the @n@
-- latest versions of 'SharedImage's with the same name.
--
-- For example, if the input contains:
--
-- @@@
-- fromList
-- [ SharedImage "foo" "2020-07-07 13:34:31"
-- , SharedImage "foo" "2020-07-07 13:34:32"
-- , SharedImage "foo" "2020-07-07 13:34:33"
-- , SharedImage "bar" "2020-07-07 13:34:34"
-- , SharedImage "bar" "2020-07-07 13:34:35"
-- , SharedImage "bar" "2020-07-07 13:34:36"
-- ]
-- @@@
--
-- The output of @keepNLatestSharedImages 2@ will be:
--
-- @@@
-- fromList
-- [ SharedImage "foo" "2020-07-07 13:34:31"
-- , SharedImage "bar" "2020-07-07 13:34:34"
-- ]
-- @@@
--
-- @since 1.1.0
dropAllButNLatestSharedImages :: Int -> Set SharedImage -> Set SharedImage
dropAllButNLatestSharedImages n =
  fold
    . Map.map
      ( \s ->
          let nOld = max 0 (length s - n)
           in Set.take nOld s
      )
    . groupBySharedImageName

-- | Group by 'SharedImageName'.
--
-- @since 1.1.0
groupBySharedImageName :: Set SharedImage -> Map SharedImageName (Set SharedImage)
groupBySharedImageName =
  foldr
    ( \img ->
        Map.alter
          ( Just
              . maybe
                (Set.singleton img)
                (Set.insert img)
          )
          (sharedImageName img)
    )
    Map.empty

