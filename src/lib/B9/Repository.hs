{-| B9 has a concept of 'B9.DiskImages.SharedImaged'. Shared images can be pulled and
pushed to/from remote locations via rsync+ssh. B9 also maintains a local cache;
the whole thing is supposed to be build-server-safe, that means no two builds
shall interfere with each other. This is accomplished by refraining from
automatic cache updates from/to remote repositories.-}
module B9.Repository (RemoteRepo(..)
                     ,remoteRepoRepoId
                     ,RepoCache(..)
                     ,SshPrivKey(..)
                     ,SshRemoteHost(..)
                     ,SshRemoteUser(..)
                     ,initRepoCache
                     ,initRemoteRepo
                     ,remoteRepoCheckSshPrivKey
                     ,remoteRepoCacheDir
                     ,localRepoDir
                     ,writeRemoteRepoConfig
                     ,getConfiguredRemoteRepos
                     ,lookupRemoteRepo) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Data
import Data.List
import Data.ConfigFile
import Text.Printf
import System.FilePath
import System.Directory
import B9.ConfigUtils

newtype RepoCache = RepoCache FilePath
  deriving (Read, Show, Typeable, Data)

data RemoteRepo = RemoteRepo String
                             FilePath
                             SshPrivKey
                             SshRemoteHost
                             SshRemoteUser
  deriving (Read, Show, Typeable, Data)

remoteRepoRepoId :: RemoteRepo -> String
remoteRepoRepoId (RemoteRepo repoId _ _ _ _) = repoId

newtype SshPrivKey = SshPrivKey FilePath
  deriving (Read, Show, Typeable, Data)

newtype SshRemoteHost = SshRemoteHost (String,Int)
  deriving (Read, Show, Typeable, Data)

newtype SshRemoteUser = SshRemoteUser String
  deriving (Read, Show, Typeable, Data)

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
  repo' <- remoteRepoCheckSshPrivKey repo
  let (RemoteRepo repoId _ _ _ _) = repo'
  ensureDir (remoteRepoCacheDir cache repoId ++ "/")
  return repo'

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

-- | Persist a repo to a configuration file.
writeRemoteRepoConfig :: RemoteRepo
                      -> ConfigParser
                      -> Either CPError ConfigParser
writeRemoteRepoConfig repo cpIn = cpWithRepo
  where section = repoId ++ repoSectionSuffix
        (RemoteRepo repoId
                    remoteRootDir
                    (SshPrivKey keyFile)
                    (SshRemoteHost (host,port))
                    (SshRemoteUser user)) = repo
        cpWithRepo = do cp1 <- add_section cpIn section
                        cp2 <- set cp1 section repoRemotePathK remoteRootDir
                        cp3 <- set cp2 section repoRemoteSshKeyK keyFile
                        cp4 <- set cp3 section repoRemoteSshHostK host
                        cp5 <- setshow cp4 section repoRemoteSshPortK port
                        set cp5 section repoRemoteSshUserK user

-- | Load a repository from a configuration file that has been written by
-- 'writeRepositoryToB9Config'.
lookupRemoteRepo :: [RemoteRepo] -> String -> Maybe RemoteRepo
lookupRemoteRepo repos repoId = lookup repoId repoIdRepoPairs
  where repoIdRepoPairs = map (\r@(RemoteRepo rid _ _ _ _) -> (rid,r)) repos

getConfiguredRemoteRepos :: ConfigParser -> [RemoteRepo]
getConfiguredRemoteRepos cp = map parseRepoSection repoSections
  where
    repoSections =
          filter (repoSectionSuffix `isSuffixOf`) (sections cp)
    parseRepoSection section =
      case parseResult of
        Left e -> error ("Error while parsing repo section \""
                         ++ section ++ "\": " ++ show e)
        Right r -> r
      where
        getsec :: Get_C a =>  OptionSpec -> Either CPError a
        getsec = get cp section
        parseResult =
          RemoteRepo repoId
            <$> getsec repoRemotePathK
            <*> (SshPrivKey <$> getsec repoRemoteSshKeyK)
            <*> (SshRemoteHost <$> ((,) <$> getsec repoRemoteSshHostK
                                        <*> getsec repoRemoteSshPortK))
            <*> (SshRemoteUser <$> getsec repoRemoteSshUserK)
          where
            repoId = let prefixLen = length section - suffixLen
                         suffixLen = length repoSectionSuffix
                         in take prefixLen section

repoSectionSuffix :: String
repoSectionSuffix = "-repo"
repoRemotePathK :: String
repoRemotePathK = "remote_path"
repoRemoteSshKeyK :: String
repoRemoteSshKeyK = "ssh_priv_key_file"
repoRemoteSshHostK :: String
repoRemoteSshHostK = "ssh_remote_host"
repoRemoteSshPortK :: String
repoRemoteSshPortK = "ssh_remote_port"
repoRemoteSshUserK :: String
repoRemoteSshUserK = "ssh_remote_user"
