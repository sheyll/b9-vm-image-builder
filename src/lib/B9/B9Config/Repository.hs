module B9.B9Config.Repository ( RemoteRepo(..)
                              , remoteRepoRepoId
                              , RepoCache(..)
                              , SshPrivKey(..)
                              , SshRemoteHost(..)
                              , SshRemoteUser(..)
                              , remoteRepoToConfigParser
                              , parseRemoteRepos
                              ) where

import Data.Data
import Data.List (isSuffixOf)
import Data.ConfigFile

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


-- | Persist a repo to a configuration file.
remoteRepoToConfigParser :: RemoteRepo
                      -> ConfigParser
                      -> Either CPError ConfigParser
remoteRepoToConfigParser repo cpIn = cpWithRepo
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
parseRemoteRepos :: ConfigParser -> Either CPError [RemoteRepo]
parseRemoteRepos cp = traverse parseRepoSection repoSections
  where
    repoSections =
          filter (repoSectionSuffix `isSuffixOf`) (sections cp)
    parseRepoSection section = parseResult
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
