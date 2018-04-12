{-| B9 has a concept of 'B9.DiskImages.SharedImaged'. Shared images can be pulled and
pushed to/from remote locations via rsync+ssh. B9 also maintains a local cache;
the whole thing is supposed to be build-server-safe, that means no two builds
shall interfere with each other. This is accomplished by refraining from
automatic cache updates from/to remote repositories.-}
module B9.Repository (initRepoCache
                     ,initRemoteRepo
                     ,cleanRemoteRepo
                     ,remoteRepoCheckSshPrivKey
                     ,remoteRepoCacheDir
                     ,localRepoDir
                     ,lookupRemoteRepo
                     ,module X) where

import Control.Monad
import Control.Monad.IO.Class
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Text.Printf
import System.FilePath
import System.Directory
import B9.B9Config.Repository as X
import System.IO.B9Extras

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
  unless exists
         (error (printf "SSH Key file '%s' for repository '%s' is missing."
                        keyFile'
                        rId))
  return (RemoteRepo rId rp (SshPrivKey keyFile') h u)

-- | Initialize the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
initRemoteRepo :: MonadIO m
               => RepoCache
               -> RemoteRepo
               -> m RemoteRepo
initRemoteRepo cache repo = do
  -- TODO logging traceL $ printf "Initializing remote repo: %s" (remoteRepoRepoId repo)
  repo' <- remoteRepoCheckSshPrivKey repo
  let (RemoteRepo repoId _ _ _ _) = repo'
  ensureDir (remoteRepoCacheDir cache repoId ++ "/")
  return repo'

-- | Empty the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
cleanRemoteRepo :: MonadIO m
                  => RepoCache
                  -> RemoteRepo
                  -> m ()
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
remoteRepoCacheDir :: RepoCache  -- ^ The repository cache directory
                   -> String    -- ^ Id of the repository
                   -> FilePath  -- ^ The existing, absolute path to the
                                -- cache directory
remoteRepoCacheDir (RepoCache cacheDir) repoId =
  cacheDir </> "remote-repos" </> repoId

-- | Return the local repository directory.
localRepoDir :: RepoCache  -- ^ The repository cache directory
             -> FilePath  -- ^ The existing, absolute path to the
                          --  directory
localRepoDir (RepoCache cacheDir) =
  cacheDir </> "local-repo"

-- | Select the first 'RemoteRepo' with a given @repoId@.
lookupRemoteRepo :: [RemoteRepo] -> String -> Maybe RemoteRepo
lookupRemoteRepo repos repoId = lookup repoId repoIdRepoPairs
  where repoIdRepoPairs = map (\r@(RemoteRepo rid _ _ _ _) -> (rid,r)) repos

