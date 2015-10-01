{-| Effectful functions executing shared image respository operations.
    See "B9.Repository" -}
module B9.RepositoryIO (repoSearch
                       ,pushToRepo
                       ,pullFromRepo
                       ,pullGlob
                       ,Repository(..)
                       ,toRemoteRepository
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
