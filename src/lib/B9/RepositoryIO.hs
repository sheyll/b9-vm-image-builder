{-# LANGUAGE ScopedTypeVariables #-}
{-| Effectful functions executing shared image respository operations.
    See "B9.Repository" -}
module B9.RepositoryIO
       (repoSearch, pushToRepo, pullFromRepo, shareImage,
        pushSharedImageLatestVersion, lookupSharedImages, getSharedImages,
        getSharedImagesCacheDir, getSelectedRepos, pullRemoteRepos,
        getLatestImageByName, getLatestSharedImageByNameFromCache,
        getSharedImageCachedFilePath, pullLatestImage, pullGlob,
        Repository(..), toRemoteRepository, FilePathGlob(..))
       where

import B9.B9Monad
import B9.QemuImg
import B9.ConfigUtils
import B9.DiskImages
import B9.Repository
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)


-- | Publish an sharedImage made from an image and image meta data to the
-- configured repository
shareImage :: Image -> SharedImageName -> B9 SharedImage
shareImage buildImg sname@(SharedImageName name) = do
    sharedImage <- createSharedImageInCache buildImg sname
    infoL (printf "SHARED '%s'" name)
    pushToSelectedRepo sharedImage
    return sharedImage

-- | Return a 'SharedImage' with the current build data and build id from the
-- name and disk image.
getSharedImageFromImageInfo :: SharedImageName -> Image -> B9 SharedImage
getSharedImageFromImageInfo name (Image _ imgType imgFS) = do
    buildId <- getBuildId
    date <- getBuildDate
    return
        (SharedImage
             name
             (SharedImageDate date)
             (SharedImageBuildId buildId)
             imgType
             imgFS)

-- | Convert the disk image and serialize the base image data structure.
createSharedImageInCache :: Image -> SharedImageName -> B9 SharedImage
createSharedImageInCache img sname@(SharedImageName name) = do
    dbgL (printf "CREATING SHARED IMAGE: '%s' '%s'" (ppShow img) name)
    sharedImg <- getSharedImageFromImageInfo sname img
    dir <- getSharedImagesCacheDir
    convertImage img (changeImageDirectory dir (sharedImageImage sharedImg))
    tell (dir </> sharedImageFileName sharedImg) sharedImg
    dbgL (printf "CREATED SHARED IMAGE IN CAHCE '%s'" (ppShow sharedImg))
    return sharedImg


-- | Publish the latest version of a shared image identified by name to the
-- selected repository from the cache.
pushSharedImageLatestVersion :: SharedImageName -> B9 ()
pushSharedImageLatestVersion name@(SharedImageName imgName) = do
    sharedImage <- getLatestSharedImageByNameFromCache name
    dbgL (printf "PUSHING '%s'" (ppShow sharedImage))
    pushToSelectedRepo sharedImage
    infoL (printf "PUSHED '%s'" imgName)

-- | Upload a shared image from the cache to a selected remote repository
pushToSelectedRepo :: SharedImage -> B9 ()
pushToSelectedRepo i = do
    c <- getSharedImagesCacheDir
    r <- getSelectedRemoteRepo
    when (isJust r) $
        do let (Image imgFile' _imgType _imgFS) = sharedImageImage i
               cachedImgFile = c </> imgFile'
               cachedInfoFile = c </> sharedImageFileName i
               repoImgFile = sharedImagesRootDirectory </> imgFile'
               repoInfoFile =
                   sharedImagesRootDirectory </> sharedImageFileName i
           pushToRepo (fromJust r) cachedImgFile repoImgFile
           pushToRepo (fromJust r) cachedInfoFile repoInfoFile

-- | Pull metadata files from all remote repositories.
pullRemoteRepos :: B9 ()
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
pullLatestImage :: SharedImageName -> B9 Bool
pullLatestImage name@(SharedImageName dbgName) = do
    repos <- getSelectedRepos
    let repoPredicate Cache = False
        repoPredicate (Remote repoId) = repoId `elem` repoIds
        repoIds = map remoteRepoRepoId repos
        hasName sharedImage = name == siName sharedImage
    candidates <- lookupSharedImages repoPredicate hasName
    let (Remote repoId,image) = last candidates
    if null candidates
        then do
            errorL
                (printf
                     "No shared image named '%s' on these remote repositories: '%s'"
                     dbgName
                     (ppShow repoIds))
            return False
        else do
            dbgL (printf "PULLING SHARED IMAGE: '%s'" (ppShow image))
            cacheDir <- getSharedImagesCacheDir
            let (Image imgFile' _imgType _fs) = sharedImageImage image
                cachedImgFile = cacheDir </> imgFile'
                cachedInfoFile = cacheDir </> sharedImageFileName image
                repoImgFile = sharedImagesRootDirectory </> imgFile'
                repoInfoFile =
                    sharedImagesRootDirectory </> sharedImageFileName image
                repo = fromJust (lookupRemoteRepo repos repoId)
            pullFromRepo repo repoImgFile cachedImgFile
            pullFromRepo repo repoInfoFile cachedInfoFile
            infoL (printf "PULLED '%s' FROM '%s'" dbgName repoId)
            return True


-- | Return the 'Image' of the latest version of a shared image named 'name'
-- from the local cache.
getLatestImageByName :: SharedImageName -> B9 Image
getLatestImageByName name = do
    sharedImage <- getLatestSharedImageByNameFromCache name
    cacheDir <- getSharedImagesCacheDir
    let image = changeImageDirectory cacheDir (sharedImageImage sharedImage)
    dbgL (printf "USING SHARED SOURCE IMAGE '%s'" (show image))
    return image

-- | Return the path the to shared image in the local cache
getSharedImageCachedFilePath :: SharedImage -> B9 FilePath
getSharedImageCachedFilePath sharedImage = do
    cacheDir <- getSharedImagesCacheDir
    let (Image f _ _) = changeImageDirectory cacheDir (sharedImageImage sharedImage)
    return f

-- | Return the latest version of a shared image named 'name' from the local cache.
getLatestSharedImageByNameFromCache :: SharedImageName -> B9 SharedImage
getLatestSharedImageByNameFromCache name@(SharedImageName dbgName) = do
    imgs <- lookupSharedImages (== Cache) ((== name) . siName)
    case reverse imgs of
        (Cache,sharedImage):_rest -> return sharedImage
        _ -> error (printf "No image(s) named '%s' found." dbgName)

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages :: B9 [(Repository, [SharedImage])]
getSharedImages = do
    reposAndFiles <-
        repoSearch
            sharedImagesRootDirectory
            (FileExtension sharedImageFileExtension)
    mapM
        (\(repo,files) ->
              ((repo, ) . catMaybes) <$> mapM consult' files)
        reposAndFiles
  where
    consult' f = do
        r <- liftIO (try (consult f))
        case r of
            Left (e :: SomeException) -> do
                dbgL
                    (printf
                         "Failed to load shared image meta-data from '%s': '%s'"
                         (takeFileName f)
                         (show e))
                dbgL (printf "Removing bad meta-data file '%s'" f)
                liftIO (removeFile f)
                return Nothing
            Right c -> return (Just c)

-- | Find shared images and the associated repos from two predicates. The result
-- is the concatenated result of the sorted shared images satisfying 'imgPred'.
lookupSharedImages :: (Repository -> Bool)
                   -> (SharedImage -> Bool)
                   -> B9 [(Repository, SharedImage)]
lookupSharedImages repoPred imgPred = do
    xs <- getSharedImages
    let rs =
            [(r, s) | (r,ss) <- xs
                    , s <- ss]
        matchingRepo = filter (repoPred . fst) rs
        matchingImg = filter (imgPred . snd) matchingRepo
        sorted = sortBy (compare `on` snd) matchingImg
    return (mconcat (pure <$> sorted))

-- | Return either all remote repos or just the single selected repo.
getSelectedRepos :: B9 [RemoteRepo]
getSelectedRepos = do
    allRepos <- getRemoteRepos
    selectedRepo <- getSelectedRemoteRepo
    let repos = maybe allRepos return selectedRepo -- 'Maybe' a repo
    return repos

-- | Return the path to the sub directory in the cache that contains files of
-- shared images.
getSharedImagesCacheDir :: B9 FilePath
getSharedImagesCacheDir = do
    cacheDir <- localRepoDir <$> getRepoCache
    return (cacheDir </> sharedImagesRootDirectory)

data Repository = Cache | Remote String
  deriving (Eq, Ord, Read, Show)

-- | Convert a `RemoteRepo` down to a mere `Repository`
toRemoteRepository :: RemoteRepo -> Repository
toRemoteRepository = Remote . remoteRepoRepoId

-- | Find files which are in 'subDir' and match 'glob' in the repository
-- cache. NOTE: This operates on the repository cache, but does not enforce a
-- repository cache update.
repoSearch :: FilePath -> FilePathGlob -> B9 [(Repository, [FilePath])]
repoSearch subDir glob = (:) <$> localMatches <*> remoteRepoMatches
  where remoteRepoMatches = do
          remoteRepos <- getRemoteRepos
          mapM remoteRepoSearch remoteRepos

        localMatches :: B9 (Repository, [FilePath])
        localMatches = do
          cache <- getRepoCache
          let dir = localRepoDir cache </> subDir
          files <- findGlob dir
          return (Cache, files)

        remoteRepoSearch :: RemoteRepo -> B9 (Repository, [FilePath])
        remoteRepoSearch repo = do
          cache <- getRepoCache
          let dir = remoteRepoCacheDir cache repoId </> subDir
              (RemoteRepo repoId _ _ _ _) = repo
          files <- findGlob dir
          return (Remote repoId, files)

        findGlob :: FilePath -> B9 [FilePath]
        findGlob dir = do
          traceL (printf "reading contents of directory '%s'" dir)
          ensureDir (dir ++ "/")
          files <- liftIO (getDirectoryContents dir)
          return ((dir </>) <$> filter (matchGlob glob) files)

-- | Push a file from the cache to a remote repository
pushToRepo :: RemoteRepo -> FilePath -> FilePath -> B9 ()
pushToRepo repo@(RemoteRepo repoId _ _ _ _) src dest = do
  dbgL (printf "PUSHING '%s' TO REPO '%s'" (takeFileName src) repoId)
  cmd (repoEnsureDirCmd repo dest)
  cmd (pushCmd repo src dest)

-- | Pull a file from a remote repository to cache
pullFromRepo :: RemoteRepo -> FilePath -> FilePath -> B9 ()
pullFromRepo repo@(RemoteRepo repoId
                              rootDir
                              _key
                              (SshRemoteHost (host, _port))
                              (SshRemoteUser user)) src dest = do
  dbgL (printf "PULLING '%s' FROM REPO '%s'" (takeFileName src) repoId)
  cmd (printf "rsync -rtv -e 'ssh %s' '%s@%s:%s' '%s'"
              (sshOpts repo)
              user
              host
              (rootDir </> src)
              dest)

-- | Push a file from the cache to a remote repository
pullGlob :: FilePath -> FilePathGlob -> RemoteRepo -> B9 ()
pullGlob subDir glob repo@(RemoteRepo repoId rootDir _key (SshRemoteHost (host,_port)) (SshRemoteUser user)) = do
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
data FilePathGlob = FileExtension String

-- * Internals

globToPattern :: FilePathGlob -> String
globToPattern (FileExtension ext) = "*." ++ ext

-- | A predicate that is satisfied if a file path matches a glob.
matchGlob :: FilePathGlob -> FilePath -> Bool
matchGlob (FileExtension ext) = isSuffixOf ("." ++ ext)

-- | A shell command string for invoking rsync to push a path to a remote host
-- via ssh.
pushCmd :: RemoteRepo -> FilePath -> FilePath -> String
pushCmd repo@(RemoteRepo _repoId
                         rootDir
                         _key
                         (SshRemoteHost (host, _port))
                         (SshRemoteUser user)) src dest =
  printf "rsync -rtv --inplace --ignore-existing -e 'ssh %s' '%s' '%s'"
         (sshOpts repo) src sshDest
  where sshDest = printf "%s@%s:%s/%s" user host rootDir dest :: String

-- | A shell command string for invoking rsync to create the directories for a
-- file push.
repoEnsureDirCmd :: RemoteRepo -> FilePath -> String
repoEnsureDirCmd repo@(RemoteRepo _repoId
                                   rootDir
                                   _key
                                   (SshRemoteHost (host, _port))
                                   (SshRemoteUser user)) dest =
  printf "ssh %s %s@%s mkdir -p '%s'"
         (sshOpts repo)
         user
         host
         (rootDir </> takeDirectory dest)

sshOpts :: RemoteRepo -> String
sshOpts (RemoteRepo _repoId
                    _rootDir
                    (SshPrivKey key)
                    (SshRemoteHost (_host, port))
                    _user) =
  unwords ["-o","StrictHostKeyChecking=no"
          ,"-o","UserKnownHostsFile=/dev/null"
          ,"-o",printf "Port=%i" port
          ,"-o","IdentityFile=" ++ key]
