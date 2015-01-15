module B9.RepositoryIO (publishDirectoryRecursive
                       ,repoSearch
                       ,Repository(..)
                       ,FilePathGlob(..)) where

import B9.Repository
import B9.B9Monad
import B9.ConfigUtils

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Text.Printf (printf)

data Repository = Cache | Remote String
  deriving (Read, Show)

-- | Upload a directory from the cache to remote repository
shareFile :: FilePath -> B9 ()
shareFile dir = do
  cache <- getRepoCache
  let subDir = takeFileName dir -- e.g.: "/.../BuildDir/.../b9-shared-images/"
                                -- -> "b9-shared-images"
      cacheSubDir = localRepoDir cache </> subDir
      newFileName f = cacheSubDir </> takeFileName f
  ensureDir (cacheSubDir ++ "/")
  filesToMove <- liftIO (getDirectoryContents dir)
  liftIO (sequence_ (renameFile <$> filesToMove <*> (newFileName <$> filesToMove)))

-- | Find files which are in 'subDir' and match 'glob' in the repository
-- cache. NOTE: This operates on the repository cache, but does not enforce a
-- repository cache update.
repoSearch :: FilePath -> FilePathGlob -> B9 [(Repository, [FilePath])]
repoSearch subDir glob = (:) <$> localMatches <*> remoteRepoMatches
  where localMatches = repoCacheSearch subDir glob
        remoteRepoMatches = do
          remoteRepos <- getRemoteRepos
          mapM (remoteRepoSearch subDir glob) remoteRepos

repoCacheSearch :: FilePath -> FilePathGlob -> B9 (Repository, [FilePath])
repoCacheSearch subDir glob = do
  cache <- getRepoCache
  let dir = localRepoDir cache </> subDir
  files <- findGlob dir glob
  return (Cache, files)

remoteRepoSearch :: FilePath -> FilePathGlob -> RemoteRepo -> B9 (Repository, [FilePath])
remoteRepoSearch subDir glob repo = do
  cache <- getRepoCache
  let dir = remoteRepoSearch cache repoId </> subDir
      (RemoteRepo repoId _ _ _ _) = repo
  files <- findGlob dir glob
  return (Remote repoId, files)

findGlob :: FilePath -> FilePathGlob -> B9 [FilePath]
findGlob dir glob = do
  traceL (printf "reading contents of directory '%s'" dir)
  ensureDir (dir ++ "/")
  files <- liftIO (getDirectoryContents dir)
  return ((dir </>) <$> (filter (matchGlob glob) files))

-- | Express a pattern for file paths, used when searching repositories.
data FilePathGlob = FileNameEndsWith String

-- * Internals

-- | A predicate that is satisfied if a file path matches a glob.
matchGlob :: FilePathGlob -> FilePath -> Bool
matchGlob (FileNameEndsWith suffix) = isSuffixOf suffix

uploadDirectoryRecursive :: RemoteRepo -> FilePath -> B9 ()
uploadDirectoryRecursive repo@(RemoteRepo repoId _ _ _ _) dir = do
  dbgL (printf "UPLOADING DIRECTORY '%s' TO REPO '%s'"
               dir
               (show repoId))
  let uploadCmd = createUploadCommandTemplate repo
  cmd (printf uploadCmd dir)

createUploadCommandTemplate :: RemoteRepo -> String
createUploadCommandTemplate (RemoteRepo _repoId
                                        rootDir
                                        (SshPrivKey key)
                                        (SshRemoteHost (host, port))
                                        (SshRemoteUser user)) =
  "rsync -av 'ssh " ++ sshOpts ++ "' '%s' '"++ dest ++"'"
  where sshOpts = unwords ["-o","StrictHostKeyChecking=no"
                          ,"-o","UserKnownHostsFile=/dev/null"
                          ,"-o",printf "Port=%i" port
                          ,"-o","IdentityFile=" ++ key]
        dest = printf "%s@%s:%s" user host rootDir
