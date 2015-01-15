module B9.Repository (Repository(..)
                     ,RepositorySpec(..)
                     ,SshPrivKey(..)
                     ,SshRemoteHost(..)
                     ,SshRemoteUser(..)
                     ,parseRepositoriesFromB9Config
                     ,writeRepositoryToB9Config
                     ,lookupRepository
                     ,initRepo
                     ,repoSpecFromDirectory) where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Data
import Data.List
import Data.ConfigFile
import Text.Printf
import System.FilePath

import B9.ConfigUtils

data Repository = Repository String RepositorySpec
  deriving (Read, Show, Typeable, Data)

data RepositorySpec = LocalRepo SystemPath
                    | RemoteRepo
                      FilePath
                      SshPrivKey
                      SshRemoteHost
                      SshRemoteUser
  deriving (Read, Show, Typeable, Data)

newtype SshPrivKey = SshPrivKey FilePath
  deriving (Read, Show, Typeable, Data)

newtype SshRemoteHost = SshRemoteHost (String,Int)
  deriving (Read, Show, Typeable, Data)

newtype SshRemoteUser = SshRemoteUser String
  deriving (Read, Show, Typeable, Data)

lookupRepository :: ConfigParser -> String -> Repository
lookupRepository cp repoId =
  let repos = map (\r@(Repository rid _) -> (rid,r))
                  (parseRepositoriesFromB9Config cp)
      in maybe (error (printf "Repository '%s' not defined!" repoId))
               id
               (lookup repoId repos)

writeRepositoryToB9Config :: Repository
                          -> ConfigParser
                          -> Either CPError ConfigParser
writeRepositoryToB9Config repo cp = cpWithRepo
  where section = repoId ++ repoSectionSuffix
        (Repository repoId repoType) = repo
        cpWithRepo = do cp' <- add_section cp section
                        writeRepositorySpecToB9Config section repoType cp'

writeRepositorySpecToB9Config :: SectionSpec
                              -> RepositorySpec
                              -> ConfigParser
                              -> Either CPError ConfigParser
writeRepositorySpecToB9Config section (LocalRepo p) cp = do
  cp' <- set cp section repoTypeK repoTypeLocal
  setshow cp' section repoLocalPathK p
writeRepositorySpecToB9Config section
                              (RemoteRepo remoteRootDir
                                          (SshPrivKey keyFile)
                                          (SshRemoteHost (host,port))
                                          (SshRemoteUser user))
                              cp = do
  cp1 <- set cp section repoTypeK repoTypeRemote
  cp2 <- set cp1 section repoRemotePathK remoteRootDir
  cp3 <- set cp2 section repoRemoteSshKeyK keyFile
  cp4 <- set cp3 section repoRemoteSshHostK host
  cp5 <- setshow cp4 section repoRemoteSshPortK port
  set cp5 section repoRemoteSshUserK user

parseRepositoriesFromB9Config :: ConfigParser -> [Repository]
parseRepositoriesFromB9Config cp = map parseRepoSection repoSections
  where
    repoSections =
          filter (repoSectionSuffix `isSuffixOf`) (sections cp)
    parseRepoSection section =
      case parseResult of
        Left e -> error ("Error while parsing repo section \""
                         ++ section ++ "\": " ++ show e)
        Right r -> Repository repoId r
      where
        repoId = take prefixLen section
          where prefixLen = length section - suffixLen
                suffixLen = length repoSectionSuffix
        getsec :: Get_C a =>  OptionSpec -> Either CPError a
        getsec = get cp section
        parseResult = do
          repoType <- getsec repoTypeK
          case repoType of
           t | t == repoTypeLocal ->
             LocalRepo <$> getsec repoLocalPathK
           t | t == repoTypeRemote ->
             RemoteRepo <$> getsec repoRemotePathK
                        <*> (SshPrivKey <$> getsec repoRemoteSshKeyK)
                        <*> (SshRemoteHost <$>
                             ((,) <$> (getsec repoRemoteSshHostK)
                                  <*> (getsec repoRemoteSshPortK)))
                        <*> (SshRemoteUser <$> getsec repoRemoteSshUserK)

repoSectionSuffix :: String
repoSectionSuffix = "-repo"
repoTypeK :: String
repoTypeK = "repo_type"
repoTypeLocal :: String
repoTypeLocal = "rsync-local"
repoTypeRemote :: String
repoTypeRemote = "rsync-remote"
repoLocalPathK :: String
repoLocalPathK = "path"
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

-- | Initialize the repository, if the repository is local make sure the
-- directory exists.
initRepo :: MonadIO m => Repository -> m Repository
initRepo (Repository repoId (LocalRepo repoDirSystemPath)) = do
  repoDir <- resolve repoDirSystemPath
  ensureDir (repoDir ++ "/")
  return (Repository repoId (LocalRepo (Path repoDir)))
initRepo repo = return repo

-- | Create a 'RepoSpec' for a local directory.
repoSpecFromDirectory :: SystemPath -> RepositorySpec
repoSpecFromDirectory = LocalRepo
