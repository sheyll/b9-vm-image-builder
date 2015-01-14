module B9.Repository (Repository(..)
                     ,RepositoryType(..)
                     ,SshPrivKey(..)
                     ,SshRemoteHost(..)
                     ,SshRemoteUser(..)
                     ,RepositoryCache(..)
                     ,parseRepositoriesFromB9Config
                     ,writeRepositoryToB9Config
                     ,lookupRepository) where

import Control.Applicative
import Data.Data
import Data.Monoid
import Data.List
import Data.ConfigFile
import Text.Printf

import B9.ConfigUtils

data Repository = Repository String RepositoryType
  deriving (Read, Show, Typeable, Data)

data RepositoryType = LocalRepo SystemPath
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

data RepositoryCache = RepositoryCacheDisabled
                     | RepositoryCache SystemPath
  deriving (Read, Show, Typeable, Data)

instance Monoid RepositoryCache where
  mempty = RepositoryCacheDisabled
  mappend x RepositoryCacheDisabled = x
  mappend _ x = x

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
  where section = repoId ++ b9ConfigRepositorySectionSuffix
        (Repository repoId repoType) = repo
        cpWithRepo = do cp' <- add_section cp section
                        writeRepositoryTypeToB9Config section repoType cp'

writeRepositoryTypeToB9Config :: SectionSpec
                              -> RepositoryType
                              -> ConfigParser
                              -> Either CPError ConfigParser
writeRepositoryTypeToB9Config section (LocalRepo p) cp = do
  cp <- set cp section repoTypeK repoTypeLocal
  setshow cp section repoTypeLocalPathK p
writeRepositoryTypeToB9Config section
                              (RemoteRepo remoteRootDir
                                          (SshPrivKey keyFile)
                                          (SshRemoteHost (host,port))
                                          (SshRemoteUser user))
                              cp = do
  cp <- set cp section repoTypeK repoTypeRemote
  cp <- set cp section repoTypeRemotePathK remoteRootDir
  cp <- set cp section repoTypeRemoteSshKeyK keyFile
  cp <- set cp section repoTypeRemoteSshHostK host
  cp <- setshow cp section repoTypeRemoteSshPortK port
  set cp section repoTypeRemoteSshUserK user

parseRepositoriesFromB9Config :: ConfigParser -> [Repository]
parseRepositoriesFromB9Config cp = map parseRepoSection repoSections
  where
    repoSections =
          filter (b9ConfigRepositorySectionSuffix `isSuffixOf`) (sections cp)
    parseRepoSection section =
      case parseResult of
        Left e -> error ("Error while parsing repo section \""
                         ++ section ++ "\": " ++ show e)
        Right r -> Repository repoId r
      where
        repoId = take prefixLen section
          where prefixLen = length section - suffixLen
                suffixLen = length b9ConfigRepositorySectionSuffix
        getsec :: Get_C a =>  OptionSpec -> Either CPError a
        getsec = get cp section
        parseResult = do
          repoType <- getsec repoTypeK
          case repoType of
           t | t == repoTypeLocal ->
             LocalRepo <$> getsec repoTypeLocalPathK
           t | t == repoTypeRemote ->
             RemoteRepo <$> getsec repoTypeRemotePathK
                        <*> (SshPrivKey <$> getsec repoTypeRemoteSshKeyK)
                        <*> (SshRemoteHost <$>
                             ((,) <$> (getsec repoTypeRemoteSshHostK)
                                  <*> (getsec repoTypeRemoteSshPortK)))
                        <*> (SshRemoteUser <$> getsec repoTypeRemoteSshUserK)

b9ConfigRepositorySectionSuffix = "-repo"

repoTypeK = "repo_type"
repoTypeLocal = "rsync-local"
repoTypeRemote = "rsync-remote"

repoTypeLocalPathK = "path"

repoTypeRemotePathK = "remote_path"
repoTypeRemoteSshKeyK = "ssh_priv_key_file"
repoTypeRemoteSshHostK = "ssh_remote_host"
repoTypeRemoteSshPortK = "ssh_remote_port"
repoTypeRemoteSshUserK = "ssh_remote_user"
