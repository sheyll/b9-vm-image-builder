-- | Effectful functions executing shared image respository operations.
--    See "B9.Repository"
module B9.RepositoryIO
  ( repoSearch, -- internal
    pushToRepo, -- internal
    pullFromRepo, -- internal
    withRemoteRepos,
    pullGlob,
    FilePathGlob (..),
    getSharedImages,
    getSharedImagesCacheDir,
    getSelectedRepos,
    pullRemoteRepos,
    pullLatestImage,
    cleanOldSharedImageRevisionsFromCache,
    cleanLocalRepoCache,
    pushToSelectedRepo,
    pushSharedImageLatestVersion,
    getLatestImageByName,
    -- initRemoteRepo,
    cleanRemoteRepo,
    remoteRepoCheckSshPrivKey,
  )
where

import B9.B9Config
import B9.B9Error
import B9.B9Exec
import System.IO.B9Extras
import B9.B9Logging
import B9.DiskImages
import B9.Repository
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Exception
import Control.Lens ((.~), (^.), view)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class
import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO.B9Extras (SystemPath, consult, ensureDir, resolve)
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

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

-- | Find files which are in 'subDir' and match 'glob' in the repository
-- cache. NOTE: This operates on the repository cache, but does not enforce a
-- repository cache update.
repoSearch ::
  forall e.
  (CommandIO e, Member RepoCacheReader e) =>
  FilePath ->
  FilePathGlob ->
  Eff e [(Repository, [FilePath])]
repoSearch subDir glob = (:) <$> localMatches <*> remoteRepoMatches
  where
    remoteRepoMatches =
      getRemoteRepos >>= mapM remoteRepoSearch
    localMatches :: Eff e (Repository, [FilePath])
    localMatches = do
      cache <- getRepoCache
      let dir = localRepoDir cache </> subDir
      files <- findGlob dir
      return (Cache, files)
    remoteRepoSearch :: RemoteRepo -> Eff e (Repository, [FilePath])
    remoteRepoSearch repo = do
      cache <- getRepoCache
      let dir = remoteRepoCacheDir cache repoId </> subDir
          (RemoteRepo repoId _ _ _ _) = repo
      files <- findGlob dir
      return (Remote repoId, files)
    findGlob :: FilePath -> Eff e [FilePath]
    findGlob dir = do
      traceL (printf "reading contents of directory '%s'" dir)
      ensureDir (dir ++ "/")
      files <- liftIO (getDirectoryContents dir)
      return ((dir </>) <$> filter (matchGlob glob) files)

-- | Push a file from the cache to a remote repository
pushToRepo :: (CommandIO e) => RemoteRepo -> FilePath -> FilePath -> Eff e ()
pushToRepo repo@(RemoteRepo repoId _ _ _ _) src dest = do
  dbgL (printf "PUSHING '%s' TO REPO '%s'" (takeFileName src) repoId)
  cmd (repoEnsureDirCmd repo dest)
  cmd (pushCmd repo src dest)

-- | Pull a file from a remote repository to cache
pullFromRepo :: (CommandIO e) => RemoteRepo -> FilePath -> FilePath -> Eff e ()
pullFromRepo repo@(RemoteRepo repoId rootDir _key (SshRemoteHost (host, _port)) (SshRemoteUser user)) src dest =
  do
    dbgL (printf "PULLING '%s' FROM REPO '%s'" (takeFileName src) repoId)
    cmd
      ( printf
          "rsync -rtv -e 'ssh %s' '%s@%s:%s' '%s'"
          (sshOpts repo)
          user
          host
          (rootDir </> src)
          dest
      )

-- | Push a file from the cache to a remote repository
pullGlob ::
  (CommandIO e, Member RepoCacheReader e) =>
  FilePath ->
  FilePathGlob ->
  RemoteRepo ->
  Eff e ()
pullGlob subDir glob repo@(RemoteRepo repoId rootDir _key (SshRemoteHost (host, _port)) (SshRemoteUser user)) =
  do
    cache <- getRepoCache
    infoL (printf "SYNCING REPO METADATA '%s'" repoId)
    let c =
          printf
            "rsync -rtv --include '%s' --exclude '*.*' -e 'ssh %s' '%s@%s:%s/' '%s/'"
            (globToPattern glob)
            (sshOpts repo)
            user
            host
            (rootDir </> subDir)
            destDir
        destDir = repoCacheDir </> subDir
        repoCacheDir = remoteRepoCacheDir cache repoId
    ensureDir destDir
    cmd c

-- | Express a pattern for file paths, used when searching repositories.
newtype FilePathGlob
  = FileExtension String

-- * Internals

globToPattern :: FilePathGlob -> String
globToPattern (FileExtension ext) = "*." ++ ext

-- | A predicate that is satisfied if a file path matches a glob.
matchGlob :: FilePathGlob -> FilePath -> Bool
matchGlob (FileExtension ext) = isSuffixOf ("." ++ ext)

-- | A shell command string for invoking rsync to push a path to a remote host
-- via ssh.
pushCmd :: RemoteRepo -> FilePath -> FilePath -> String
pushCmd repo@(RemoteRepo _repoId rootDir _key (SshRemoteHost (host, _port)) (SshRemoteUser user)) src dest =
  printf
    "rsync -rtv --inplace --ignore-existing -e 'ssh %s' '%s' '%s'"
    (sshOpts repo)
    src
    sshDest
  where
    sshDest = printf "%s@%s:%s/%s" user host rootDir dest :: String

-- | A shell command string for invoking rsync to create the directories for a
-- file push.
repoEnsureDirCmd :: RemoteRepo -> FilePath -> String
repoEnsureDirCmd repo@(RemoteRepo _repoId rootDir _key (SshRemoteHost (host, _port)) (SshRemoteUser user)) dest =
  printf
    "ssh %s %s@%s mkdir -p '%s'"
    (sshOpts repo)
    user
    host
    (rootDir </> takeDirectory dest)

sshOpts :: RemoteRepo -> String
sshOpts (RemoteRepo _repoId _rootDir (SshPrivKey key) (SshRemoteHost (_host, port)) _user) =
  unwords
    [ "-o",
      "StrictHostKeyChecking=no",
      "-o",
      "UserKnownHostsFile=/dev/null",
      "-o",
      printf "Port=%i" port,
      "-o",
      "IdentityFile=" ++ key
    ]

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages ::
  (HasCallStack, CommandIO e, Lifted IO e, Member RepoCacheReader e) =>
  Eff e (Map Repository (Set SharedImage))
getSharedImages = do
  reposAndFiles <-
    repoSearch
      sharedImagesRootDirectory
      (FileExtension sharedImageFileExtension)
  foldrM
    ( \(repo, files) acc -> do
        imgs <- catMaybes <$> mapM consult' files
        let imgSet = Set.fromList imgs
        return (Map.insert repo imgSet acc)
    )
    Map.empty
    reposAndFiles
  where
    consult' f = do
      r <- liftIO (try (consult f))
      case r of
        Left (e :: SomeException) -> do
          dbgL
            ( printf
                "Failed to load shared image meta-data from '%s': '%s'"
                (takeFileName f)
                (show e)
            )
          dbgL (printf "Removing bad meta-data file '%s'" f)
          liftIO (removeFile f)
          return Nothing
        Right c -> return (Just c)

-- | Pull metadata files from all remote repositories.
pullRemoteRepos ::
  (HasCallStack, Lifted IO e, CommandIO e, '[SelectedRemoteRepoReader, RepoCacheReader] <:: e) =>
  Eff e ()
pullRemoteRepos = do
  repos <- getSelectedRepos
  mapM_ dl repos
  where
    dl =
      pullGlob
        sharedImagesRootDirectory
        (FileExtension sharedImageFileExtension)

-- | Pull the latest version of an image, either from the selected remote
-- repo or from the repo that has the latest version.
pullLatestImage ::
  (HasCallStack, Lifted IO e, CommandIO e, '[ExcB9, RepoCacheReader, SelectedRemoteRepoReader] <:: e) =>
  SharedImageName ->
  Eff e (Maybe SharedImageBuildId)
pullLatestImage name@(SharedImageName dbgName) = do
  repos <- getSelectedRepos
  let repoPredicate Cache = False
      repoPredicate (Remote repoId) = repoId `elem` repoIds
      repoIds = map remoteRepoRepoId repos
      hasName sharedImage = name == sharedImageName sharedImage
  candidates <-
    filterRepoImagesMap repoPredicate hasName <$> getSharedImages
  case maxSharedImageOfAllRepos candidates of
    Nothing ->
      do
        errorL
          ( printf
              "No shared image named '%s' on these remote repositories: '%s'"
              dbgName
              (ppShow repoIds)
          )
        return Nothing
    Just (image, Cache) -> do
      errorExitL (printf "Unreachable code reached in `pullLastestImage`: '%s'  %s" dbgName (ppShow image))
    Just (image, Remote repoId) -> do
      dbgL (printf "PULLING SHARED IMAGE: '%s'" (ppShow image))
      cacheDir <- getSharedImagesCacheDir
      let (Image imgFile' _imgType _fs) = sharedImageImage image
          cachedImgFile = cacheDir </> imgFile'
          cachedInfoFile = cacheDir </> sharedImageFileName image
          repoImgFile = sharedImagesRootDirectory </> imgFile'
          repoInfoFile = sharedImagesRootDirectory </> sharedImageFileName image
          repo = fromJust (lookupRemoteRepo repos repoId)
      pullFromRepo repo repoImgFile cachedImgFile
      pullFromRepo repo repoInfoFile cachedInfoFile
      infoL (printf "PULLED '%s' FROM '%s'" dbgName repoId)
      cleanOldSharedImageRevisionsFromCache name
      return (Just (sharedImageBuildId image))

-- | Return the 'Image' of the latest version of a shared image named 'name'
-- from the local cache.
getLatestImageByName ::
  (HasCallStack, Lifted IO e, CommandIO e, Member RepoCacheReader e) =>
  SharedImageName ->
  Eff e (Maybe Image)
getLatestImageByName name = do
  sharedImageRevisions <- lookupCachedImages name <$> getSharedImages
  cacheDir <- getSharedImagesCacheDir
  let image =
        changeImageDirectory cacheDir . sharedImageImage
          <$> Set.lookupMax sharedImageRevisions
  case image of
    Just i -> dbgL (printf "USING SHARED SOURCE IMAGE '%s'" (show i))
    Nothing -> errorL (printf "SOURCE IMAGE '%s' NOT FOUND" (show name))
  return image

-- | Depending on the 'maxLocalSharedImageRevisions' 'B9Config' settings either
-- do nothing or delete all but the configured number of most recent shared
-- images with the given name from the local cache.
cleanOldSharedImageRevisionsFromCache ::
  ('[RepoCacheReader, ExcB9] <:: e, Lifted IO e, CommandIO e) =>
  SharedImageName ->
  Eff e ()
cleanOldSharedImageRevisionsFromCache sn = do
  b9Cfg <- getConfig
  forM_ (b9Cfg ^. maxLocalSharedImageRevisions) $ \maxRevisions -> do
    when
      (maxRevisions < 1)
      ( throwB9Error_
          ( printf
              "Invalid maximum local shared images revision configuration value: %d. Please change the [global] '%s' key in the B9 configuration file to 'Just x' with 'x > 0', or to 'Nothing'."
              maxRevisions
              maxLocalSharedImageRevisionsK
          )
      )
    allRevisions <- lookupCachedImages sn <$> getSharedImages
    let toDelete = dropAllButNLatestSharedImages maxRevisions allRevisions
    removeCachedSharedImages toDelete

-- | Clean all obsolete images in the local image cache.
--
-- @since 1.1.0
cleanLocalRepoCache :: 
    ('[RepoCacheReader, ExcB9] <:: e, Lifted IO e, CommandIO e) =>
    Eff e ()
cleanLocalRepoCache = do
  allCached <- allCachedSharedImages <$> getSharedImages
  maxRevConfig <- view maxLocalSharedImageRevisions <$> getConfig
  let maxRev = maybe 0 (max 0) maxRevConfig 
      byName = groupBySharedImageName allCached 
      toKeep = 
        fold 
          (Map.map 
            (keepNLatestSharedImages maxRev) 
            byName)
      toDelete = 
        fold 
          (Map.map 
            (dropAllButNLatestSharedImages maxRev) 
            byName)
  infoL "ALL CACHED IMAGES:"
  forM_ allCached (infoL . show)
  infoL ("CACHED " ++ maybe "" (("("++) . (++ ") ") . show) maxRevConfig ++ "IMAGES TO KEEP:")
  forM_ toKeep (infoL . show)
  infoL "CACHED IMAGES TO DELETE:"
  forM_ toDelete (infoL . show)
  removeCachedSharedImages toDelete
  return ()

-- | Publish the latest version of a shared image identified by name to the
-- selected repository from the cache.
pushSharedImageLatestVersion ::
  (Lifted IO e, CommandIO e, '[SelectedRemoteRepoReader, RepoCacheReader, ExcB9] <:: e) =>
  SharedImageName ->
  Eff e ()
pushSharedImageLatestVersion name@(SharedImageName imgName) =
  lookupCachedImages name <$> getSharedImages
    >>= maybe
      (errorExitL (printf "Nothing found for %s." (show imgName)))
      ( \sharedImage -> do
          dbgL (printf "PUSHING '%s'" (ppShow sharedImage))
          pushToSelectedRepo sharedImage
          infoL (printf "PUSHED '%s'" imgName)
      )
      . Set.lookupMax

-- | Upload a shared image from the cache to a selected remote repository
pushToSelectedRepo ::
  (Lifted IO e, CommandIO e, '[RepoCacheReader, SelectedRemoteRepoReader] <:: e) =>
  SharedImage ->
  Eff e ()
pushToSelectedRepo i = do
  c <- getSharedImagesCacheDir
  MkSelectedRemoteRepo r <- getSelectedRemoteRepo
  when (isJust r) $ do
    let (Image imgFile' _imgType _imgFS) = sharedImageImage i
        cachedImgFile = c </> imgFile'
        cachedInfoFile = c </> sharedImageFileName i
        repoImgFile = sharedImagesRootDirectory </> imgFile'
        repoInfoFile = sharedImagesRootDirectory </> sharedImageFileName i
    pushToRepo (fromJust r) cachedImgFile repoImgFile
    pushToRepo (fromJust r) cachedInfoFile repoInfoFile

-- | Return either all remote repos or just the single selected repo.
getSelectedRepos :: '[B9ConfigReader, SelectedRemoteRepoReader] <:: e => Eff e [RemoteRepo]
getSelectedRepos = do
  allRepos <- getRemoteRepos
  MkSelectedRemoteRepo selectedRepo <- getSelectedRemoteRepo
  let repos = maybe allRepos return selectedRepo -- 'Maybe' a repo
  return repos

-- | Return the path to the sub directory in the cache that contains files of
-- shared images.
getSharedImagesCacheDir :: '[RepoCacheReader] <:: e => Eff e FilePath
getSharedImagesCacheDir = do
  cacheDir <- localRepoDir <$> getRepoCache
  return (cacheDir </> sharedImagesRootDirectory)

removeCachedSharedImages :: (CommandIO e, Member (Reader RepoCache) e) => Set SharedImage -> Eff e ()
removeCachedSharedImages toDelete = 
  do 
     imgDir <- getSharedImagesCacheDir
     let filesToDelete = Set.map (imgDir </>) (infoFiles <> imgFiles)
         infoFiles = Set.map sharedImageFileName toDelete
         imgFiles = Set.map (imageFileName . sharedImageImage) toDelete
     if Set.null filesToDelete
       then infoL "NO IMAGES TO DELETE"
       else do
         infoL "DELETING FILES:"
         forM_ filesToDelete (infoL . show) 
         liftIO (mapM_ removeIfExists filesToDelete)

