-- | Effectful functions executing shared image respository operations.
--    See "B9.Repository"
module B9.RepositoryIO
  ( repoSearch, -- internal 
    pushToRepo, -- internal
    pullFromRepo, -- internal
    pullGlob,
    Repository (..),
    toRemoteRepository,
    FilePathGlob (..),
    lookupSharedImages,
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
  )
where

import Data.Maybe
import Data.Function (on)
import B9.B9Config (B9ConfigReader, getRemoteRepos, getConfig, maxLocalSharedImageRevisions)
import B9.B9Exec
import B9.B9Error
import B9.B9Logging
import B9.Repository
import B9.DiskImages
import Control.Eff
import Control.Exception
import Control.Lens ((^.))
import Control.Monad.IO.Class
import Control.Monad (when, forM_)
import Data.List
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO.B9Extras (consult, ensureDir)
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

data Repository
  = Cache
  | Remote String
  deriving (Eq, Ord, Read, Show)

-- | Convert a `RemoteRepo` down to a mere `Repository`
toRemoteRepository :: RemoteRepo -> Repository
toRemoteRepository = Remote . remoteRepoRepoId

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
    remoteRepoMatches = do
      remoteRepos <- getRemoteRepos
      mapM remoteRepoSearch remoteRepos
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


-- | Find shared images and the associated repos from two predicates. The result
-- is the concatenated result of the sorted shared images satisfying 'imgPred'.
lookupSharedImages :: 
  (HasCallStack, Lifted IO e, CommandIO e, Member RepoCacheReader e) =>
  (Repository -> Bool) ->
  (SharedImage -> Bool) ->
  Eff e [(Repository, SharedImage)]
lookupSharedImages repoPred imgPred = do
  xs <- getSharedImages
  let rs = [(r, s) | (r, ss) <- xs, s <- ss]
      matchingRepo = filter (repoPred . fst) rs
      matchingImg = filter (imgPred . snd) matchingRepo
      sorted = sortBy (compare `on` snd) matchingImg
  return (mconcat (pure <$> sorted))

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages ::
  (HasCallStack, CommandIO e, Lifted IO e, Member RepoCacheReader e) =>
  Eff e [(Repository, [SharedImage])]
getSharedImages = do
  reposAndFiles <-
    repoSearch
      sharedImagesRootDirectory
      (FileExtension sharedImageFileExtension)
  mapM
    (\(repo, files) -> (repo,) . catMaybes <$> mapM consult' files)
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
  (HasCallStack, Lifted IO e, CommandIO e, '[RepoCacheReader,SelectedRemoteRepoReader] <:: e) => 
  SharedImageName -> Eff e (Maybe SharedImageBuildId)
pullLatestImage name@(SharedImageName dbgName) = do
  repos <- getSelectedRepos
  let repoPredicate Cache = False
      repoPredicate (Remote repoId) = repoId `elem` repoIds
      repoIds = map remoteRepoRepoId repos
      hasName sharedImage = name == sharedImageName sharedImage
  candidates <- lookupSharedImages repoPredicate hasName
  let (Remote repoId, image) = last candidates
  if null candidates
    then do
      errorL
        ( printf
            "No shared image named '%s' on these remote repositories: '%s'"
            dbgName
            (ppShow repoIds)
        )
      return Nothing
    else do
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
  SharedImageName -> Eff e (Maybe Image)
getLatestImageByName name = do
  sharedImage <- getSharedImageVersionsFromCache name
  cacheDir <- getSharedImagesCacheDir
  let image = changeImageDirectory cacheDir . sharedImageImage <$> listToMaybe sharedImage
  case image of
    Just i -> dbgL (printf "USING SHARED SOURCE IMAGE '%s'" (show i))
    Nothing -> errorL (printf "SOURCE IMAGE '%s' NOT FOUND" (show name))
  return image

-- | Return the versions of a shared image named 'name' from the local cache.
--
-- @since 1.1.0
getSharedImageVersionsFromCache ::
  (Member RepoCacheReader e, Lifted IO e, CommandIO e) =>
  SharedImageName -> Eff e [SharedImage]
getSharedImageVersionsFromCache name = do
  imgs <- lookupSharedImages (== Cache) ((== name) . sharedImageName)
  return (reverse (sort (map snd imgs)))

-- | Depending on the 'maxLocalSharedImageRevisions' 'B9Config' settings either
-- do nothing or delete all but the configured number of most recent shared
-- images with the given name from the local cache.
cleanOldSharedImageRevisionsFromCache :: 
  ( '[B9ConfigReader, RepoCacheReader] <:: e, Lifted IO e, CommandIO e) =>
  SharedImageName -> Eff e ()
cleanOldSharedImageRevisionsFromCache _sn = do
  b9Cfg <- getConfig
  forM_ (b9Cfg ^. maxLocalSharedImageRevisions) $ \_maxRevisions -> do
    error "TODO"    

-- | Clean all obsolete images in the local image cache.
-- 
-- @since 1.1.0
cleanLocalRepoCache :: Eff e ()
cleanLocalRepoCache = do
  error "IMPLEMENT ME"
--      toDelete <-
--        obsoleteSharedmages . map snd
--          <$> lookupSharedImages
--            (== Cache)
--            (const True)
--      imgDir <- getSharedImagesCacheDir
--      let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
--          infoFiles = sharedImageFileName <$> toDelete
--          imgFiles = imageFileName . sharedImageImage <$> toDelete
--      if null filesToDelete
--        then infoL "NO IMAGES TO DELETE"
--        else liftIO $ do
--          putStrLn "DELETING FILES:"
--          putStrLn (unlines filesToDelete)
--          mapM_ removeIfExists filesToDelete
--      where
--        removeIfExists :: FilePath -> IO ()
--        removeIfExists fileName = removeFile fileName `catch` handleExists
--          where
--            handleExists e
--              | isDoesNotExistError e = return ()
--              | otherwise = throwIO e
--        -- TODO delete-too-many-revisions
--        obsoleteSharedmages :: [SharedImage] -> [SharedImage]
--        obsoleteSharedmages =
--          concatMap (tail . reverse) . filter ((> 1) . length)
--            . groupBy
--              ((==) `on` sharedImageName)

-- | Publish the latest version of a shared image identified by name to the
-- selected repository from the cache.
pushSharedImageLatestVersion ::
 (Lifted IO e, CommandIO e, '[SelectedRemoteRepoReader, RepoCacheReader, ExcB9] <:: e) =>
 SharedImageName -> Eff e ()
pushSharedImageLatestVersion name@(SharedImageName imgName) =
  getSharedImageVersionsFromCache name
    >>= maybe
      (errorExitL (printf "Nothing found for %s." (show imgName)))
      ( \sharedImage -> do
          dbgL (printf "PUSHING '%s'" (ppShow sharedImage))
          pushToSelectedRepo sharedImage
          infoL (printf "PUSHED '%s'" imgName)
      ) . listToMaybe

-- | Upload a shared image from the cache to a selected remote repository
pushToSelectedRepo :: 
  (Lifted IO e, CommandIO e, '[B9ConfigReader, LoggerReader, RepoCacheReader, SelectedRemoteRepoReader] <:: e) =>
  SharedImage -> Eff e ()
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

