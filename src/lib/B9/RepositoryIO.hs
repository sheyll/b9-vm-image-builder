module B9.RepositoryIO (repoSearch
                       ,uploadToRepo
                       ,Repository(..)
                       ,FilePathGlob(..)) where

import B9.Repository
import B9.B9Monad
import B9.ConfigUtils

import Control.Applicative
import Data.List
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Text.Printf (printf)

data Repository = Cache | Remote String
  deriving (Read, Show)

-- | Upload a file from the cache to a remote repository
uploadToRepo :: RemoteRepo -> FilePath -> FilePath -> B9 ()
uploadToRepo repo@(RemoteRepo repoId _ _ _ _) src dest = do
  dbgL (printf "UPLOADING '%s' TO REPO '%s'" (takeFileName src) repoId)
  cmd (repoEnsureDirCmd repo dest)
  cmd (uploadCmd repo src dest)

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
          return ((dir </>) <$> (filter (matchGlob glob) files))

-- | Express a pattern for file paths, used when searching repositories.
data FilePathGlob = FileNameEndsWith String

-- * Internals

-- | A predicate that is satisfied if a file path matches a glob.
matchGlob :: FilePathGlob -> FilePath -> Bool
matchGlob (FileNameEndsWith suffix) = isSuffixOf suffix

-- | A shell command string for invoking rsync to upload a path to a remote host
-- via ssh.
uploadCmd :: RemoteRepo -> FilePath -> FilePath -> String
uploadCmd (RemoteRepo _repoId
                       rootDir
                       (SshPrivKey key)
                       (SshRemoteHost (host, port))
                       (SshRemoteUser user)) src dest =
  printf "rsync -rtv --inplace --ignore-existing -e 'ssh %s' '%s' '%s'"
         sshOpts src sshDest
  where sshOpts = unwords ["-o","StrictHostKeyChecking=no"
                          ,"-o","UserKnownHostsFile=/dev/null"
                          ,"-o",printf "Port=%i" port
                          ,"-o","IdentityFile=" ++ key]
        sshDest = printf "%s@%s:%s/%s" user host rootDir dest :: String

-- | A shell command string for invoking rsync to create the directories for a
-- file upload.
repoEnsureDirCmd :: RemoteRepo -> FilePath -> String
repoEnsureDirCmd (RemoteRepo _repoId
                              rootDir
                              (SshPrivKey key)
                              (SshRemoteHost (host, port))
                              (SshRemoteUser user)) dest =
  printf "ssh %s %s@%s mkdir -p '%s'"
         sshOpts
         user
         host
         (rootDir </> takeDirectory dest)
  where
    sshOpts = unwords ["-o","StrictHostKeyChecking=no"
                      ,"-o","UserKnownHostsFile=/dev/null"
                      ,"-o",printf "Port=%i" port
                      ,"-o","IdentityFile=" ++ key]
