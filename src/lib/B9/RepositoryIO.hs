module B9.RepositoryIO (publishDirectoryRecursive) where

import B9.Repository
import B9.B9Monad
import B9.ConfigUtils

import Control.Monad.IO.Class
import System.Directory
import Text.Printf (printf)

publishDirectoryRecursive :: Repository -> FilePath -> B9 ()
publishDirectoryRecursive repo@(Repository _ repoType) dir = do
  dbgL (printf "UPLOADING DIRECTORY '%s' TO REPO '%s'"
               dir
               (show repo))
  repoTypeResolved <- prepareRepo repoType
  let uploadCmd = createUploadCommandTemplate repoTypeResolved
  cmd (printf uploadCmd dir)

prepareRepo :: RepositoryType -> B9 RepositoryType
prepareRepo (LocalRepo repoDirSystemPath) = do
  repoDir <- resolve repoDirSystemPath
  liftIO (createDirectoryIfMissing True repoDir)
  return (LocalRepo (Path repoDir))
prepareRepo repo = return repo

createUploadCommandTemplate :: RepositoryType -> String
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
