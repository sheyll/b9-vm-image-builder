module B9.RepositoryIO (publishDirectoryRecursive
                       ,repoSearch
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

-- | Recursively upload a directory into the repository cache directory and into
-- a repository if configured
publishDirectoryRecursive :: FilePath -> B9 ()
publishDirectoryRecursive dir = do
   cacheDir <- getRepositoryCache
   let cache = repoSpecFromDirectory (Path cacheDir)
   uploadDirectoryRecursive cache dir
   publishRepo <- getRepository
   when (isJust publishRepo) $ do
     let Repository publishRepoId publishRepoType = fromJust publishRepo
     uploadDirectoryRecursive publishRepoType dir
     infoL (printf "PUBLISHED TO '%s'" publishRepoId)

-- | Find files which are in 'subDir' and match 'glob' in the repository
-- cache. NOTE: This operates on the repository cache, but does not enforce a
-- repository cache update.
repoSearch :: FilePath -> FilePathGlob -> B9 [FilePath]
repoSearch subDir glob = do
  cacheDir <- getRepositoryCache
  let dir = cacheDir </> subDir
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

uploadDirectoryRecursive :: RepositorySpec -> FilePath -> B9 ()
uploadDirectoryRecursive repoType dir = do
  dbgL (printf "UPLOADING DIRECTORY '%s' TO REPO '%s'"
               dir
               (show repoType))
  let uploadCmd = createUploadCommandTemplate repoType
  cmd (printf uploadCmd dir)

createUploadCommandTemplate :: RepositorySpec -> String
createUploadCommandTemplate (LocalRepo (Path repoDir)) =
  "rsync -av '%s' '"++ repoDir ++"'"
createUploadCommandTemplate (RemoteRepo rootDir
                             (SshPrivKey key)
                             (SshRemoteHost (host, port))
                             (SshRemoteUser user)) =
  "rsync -av 'ssh " ++ sshOpts ++ "' '%s' '"++ dest ++"'"
  where sshOpts = unwords ["-o","StrictHostKeyChecking=no"
                          ,"-o","UserKnownHostsFile=/dev/null"
                          ,"-o",printf "Port=%i" port
                          ,"-o","IdentityFile=" ++ key]
        dest = printf "%s@%s:%s" user host rootDir
createUploadCommandTemplate badRepoT =
  error (printf "Cannot create a command for '%s'" (show badRepoT))
