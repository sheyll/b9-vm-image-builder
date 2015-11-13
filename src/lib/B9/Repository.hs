{-| B9 has a concept of 'B9.DiskImages.SharedImage'. Shared images can be pulled and
pushed to/from remote locations via rsync+ssh. B9 also maintains a local cache;
the whole thing is supposed to be build-server-safe, that means no two builds
shall interfere with each other. This is accomplished by refraining from
automatic cache updates from/to remote repositories.-}
module B9.Repository where

import           B9.ConfigUtils
import           B9.DiskImages
import           B9.FileSystems
import           B9.QCUtil
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Data.Binary (Binary)
import           Data.ConfigFile
import           Data.Data
import           Data.Hashable
import           Data.List
import           Data.Monoid
import           GHC.Generics (Generic)
import           System.Directory
import           System.FilePath
import           Test.QuickCheck
import qualified Text.PrettyPrint.Boxes as Boxes
import           Text.Printf


-- * Shared Images

-- | 'SharedImage' holds all data necessary to describe an __instance__ of a shared
--    image identified by a 'SharedImageName'. Shared images are stored in
--    'B9.Repository's.
data SharedImage =
    SharedImage SharedImageName
                SharedImageDate
                SharedImageBuildId
                ImageType
                FileSystem -- TODO the file system should not be a parameter!
    deriving (Eq,Read,Show,Typeable,Data,Generic)

instance Hashable SharedImage
instance Binary SharedImage
instance NFData SharedImage
instance CoArbitrary SharedImage

-- | The name of the image is the de-facto identifier for push, pull, 'From' and
--   'Share'.  B9 always selects the newest version the shared image identified
--   by that name when using a shared image as an 'ImageSource'. This is a
--   wrapper around a string that identifies a 'SharedImage'
newtype SharedImageName =
    SharedImageName String
    deriving (Eq,Ord,Read,Show,Typeable,Data,Hashable,Binary,NFData,CoArbitrary)

-- | The exact time that build job __started__.
--   This is a wrapper around a string contains the build date of a
--   'SharedImage'; this is purely additional convenience and typesafety
newtype SharedImageDate =
    SharedImageDate String
    deriving (Eq,Ord,Read,Show,Typeable,Data,Hashable,Binary,NFData,CoArbitrary)

-- | Every B9 build running in a 'B9Monad'
--   contains a random unique id that is generated once per build (no matter how
--   many artifacts are created in that build) This field contains the build id
--   of the build that created the shared image instance.  This is A wrapper
--   around a string contains the build id of a 'SharedImage'; this is purely
--   additional convenience and typesafety
newtype SharedImageBuildId =
    SharedImageBuildId String
    deriving (Eq,Ord,Read,Show,Typeable,Data,Hashable,Binary,NFData,CoArbitrary)

-- | Shared images are orderd by name, build date and build id
instance Ord SharedImage where
    compare (SharedImage n d b _ _) (SharedImage n' d' b' _ _) =
        compare n n' <> compare d d' <> compare b b'

-- * Accessors and Constructors

-- | Return the name of a shared image.
siName :: SharedImage -> SharedImageName
siName (SharedImage n _ _ _ _) = n

-- | Return the date of a shared image.
siDate :: SharedImage -> SharedImageDate
siDate (SharedImage _ n _ _ _) = n

-- | Return the build id of a shared image.
siBuildId :: SharedImage -> SharedImageBuildId
siBuildId (SharedImage _ _ n _ _) = n

-- | Return the 'ImageType' of a shared image.
siImgType :: SharedImage -> ImageType
siImgType (SharedImage _ _ _ t _) = t

-- | Return the 'FileSystem' of a shared image.
siFsType :: SharedImage -> FileSystem
siFsType (SharedImage _ _ _ _ t) = t

-- | Print the contents of the shared image in one line
prettyPrintSharedImages :: [SharedImage] -> String
prettyPrintSharedImages imgs = Boxes.render table
  where
    table = Boxes.hsep 1 Boxes.left cols
      where
        cols = [nameC, dateC, idC]
          where
            nameC =
                col
                    "Name"
                    ((\(SharedImageName n) ->
                           n) .
                     siName)
            dateC =
                col
                    "Date"
                    ((\(SharedImageDate n) ->
                           n) .
                     siDate)
            idC =
                col
                    "ID"
                    ((\(SharedImageBuildId n) ->
                           n) .
                     siBuildId)
            col title accessor =
                (Boxes.text title) Boxes.// (Boxes.vcat Boxes.left cells)
              where
                cells = Boxes.text <$> accessor <$> imgs

-- | Return the disk image of an sharedImage
sharedImageImage :: SharedImage -> Image
sharedImageImage (SharedImage (SharedImageName n) _ (SharedImageBuildId bid) sharedImageType sharedImageFileSystem) =
    Image
        (n ++
         "_" ++
         bid <.> imageFileExtension sharedImageType sharedImageFileSystem)
        sharedImageType
        sharedImageFileSystem

-- | Calculate the path to the text file holding the serialized 'SharedImage'
-- relative to the directory of shared images in a repository.
sharedImageFileName :: SharedImage -> FilePath
sharedImageFileName (SharedImage (SharedImageName n) _ (SharedImageBuildId bid) _ _) =
    n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension = "b9si"

-- | The internal image type to use as best guess when dealing with a 'From'
-- value.
sharedImageDefaultImageType :: ImageType
sharedImageDefaultImageType = QCow2


-- * Shared image repositories

newtype RepoCache =
    RepoCache FilePath
    deriving (Read,Show,Typeable,Data)

data RemoteRepo =
    RemoteRepo String
               FilePath
               SshPrivKey
               SshRemoteHost
               SshRemoteUser
    deriving (Read,Show,Typeable,Data)

remoteRepoRepoId :: RemoteRepo -> String
remoteRepoRepoId (RemoteRepo repoId _ _ _ _) = repoId

newtype SshPrivKey =
    SshPrivKey FilePath
    deriving (Read,Show,Typeable,Data)

newtype SshRemoteHost =
    SshRemoteHost (String, Int)
    deriving (Read,Show,Typeable,Data)

newtype SshRemoteUser =
    SshRemoteUser String
    deriving (Read,Show,Typeable,Data)

-- | Initialize the local repository cache directory.
initRepoCache
    :: MonadIO m
    => SystemPath -> m RepoCache
initRepoCache repoDirSystemPath = do
    repoDir <- resolve repoDirSystemPath
    ensureDir (repoDir ++ "/")
    return (RepoCache repoDir)

-- | Check for existance of priv-key and make it an absolute path.
remoteRepoCheckSshPrivKey
    :: MonadIO m
    => RemoteRepo -> m RemoteRepo
remoteRepoCheckSshPrivKey (RemoteRepo rId rp (SshPrivKey keyFile) h u) = do
    exists <- liftIO (doesFileExist keyFile)
    keyFile' <- liftIO (canonicalizePath keyFile)
    unless
        exists
        (error
             (printf
                  "SSH Key file '%s' for repository '%s' is missing."
                  keyFile'
                  rId))
    return (RemoteRepo rId rp (SshPrivKey keyFile') h u)

-- | Initialize the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
initRemoteRepo
    :: MonadIO m
    => RepoCache -> RemoteRepo -> m RemoteRepo
initRemoteRepo cache repo = do
    repo' <- remoteRepoCheckSshPrivKey repo
    let (RemoteRepo repoId _ _ _ _) = repo'
    ensureDir (remoteRepoCacheDir cache repoId ++ "/")
    return repo'

-- | Empty the repository; load the corresponding settings from the config
-- file, check that the priv key exists and create the correspondig cache
-- directory.
cleanRemoteRepo
    :: MonadIO m
    => RepoCache -> RemoteRepo -> m ()
cleanRemoteRepo cache repo = do
    let repoId = remoteRepoRepoId repo
        repoDir = remoteRepoCacheDir cache repoId ++ "/"
    ensureDir
        repoDir
    liftIO $
        removeDirectoryRecursive repoDir
    ensureDir repoDir

-- | Return the cache directory for a remote repository relative to the root
-- cache dir.
remoteRepoCacheDir
    :: RepoCache  -- ^ The repository cache directory
    -> String    -- ^ Id of the repository
    -> FilePath  -- ^ The existing, absolute path to the
                 -- cache directory
remoteRepoCacheDir (RepoCache cacheDir) repoId =
    cacheDir </> "remote-repos" </> repoId

-- | Return the local repository directory.
localRepoDir
    :: RepoCache  -- ^ The repository cache directory
    -> FilePath  -- ^ The existing, absolute path to the
                 --  directory
localRepoDir (RepoCache cacheDir) = cacheDir </> "local-repo"

-- | Persist a repo to a configuration file.
writeRemoteRepoConfig :: RemoteRepo
                      -> ConfigParser
                      -> Either CPError ConfigParser
writeRemoteRepoConfig repo cpIn = cpWithRepo
  where
    section = repoId ++ repoSectionSuffix
    (RemoteRepo repoId remoteRootDir (SshPrivKey keyFile) (SshRemoteHost (host,port)) (SshRemoteUser user)) =
        repo
    cpWithRepo = do
        cp1 <- add_section cpIn section
        cp2 <- set cp1 section repoRemotePathK remoteRootDir
        cp3 <- set cp2 section repoRemoteSshKeyK keyFile
        cp4 <- set cp3 section repoRemoteSshHostK host
        cp5 <- setshow cp4 section repoRemoteSshPortK port
        set cp5 section repoRemoteSshUserK user

-- | Load a repository from a configuration file that has been written by
-- 'writeRepositoryToB9Config'.
lookupRemoteRepo :: [RemoteRepo] -> String -> Maybe RemoteRepo
lookupRemoteRepo repos repoId = lookup repoId repoIdRepoPairs
  where
    repoIdRepoPairs =
        map
            (\r@(RemoteRepo rid _ _ _ _) ->
                  (rid, r))
            repos

getConfiguredRemoteRepos :: ConfigParser -> [RemoteRepo]
getConfiguredRemoteRepos cp = map parseRepoSection repoSections
  where
    repoSections = filter (repoSectionSuffix `isSuffixOf`) (sections cp)
    parseRepoSection section =
        case parseResult of
            Left e ->
                error
                    ("Error while parsing repo section \"" ++
                     section ++ "\": " ++ show e)
            Right r -> r
      where
        getsec
            :: Get_C a
            => OptionSpec -> Either CPError a
        getsec = get cp section
        parseResult =
            RemoteRepo repoId <$> getsec repoRemotePathK <*>
            (SshPrivKey <$> getsec repoRemoteSshKeyK) <*>
            (SshRemoteHost <$>
             ((,) <$> getsec repoRemoteSshHostK <*> getsec repoRemoteSshPortK)) <*>
            (SshRemoteUser <$> getsec repoRemoteSshUserK)
          where
            repoId =
                let prefixLen = length section - suffixLen
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

-- * Quick Check instances

instance Arbitrary SharedImageName where
    arbitrary = SharedImageName <$> arbitrarySharedImageName

arbitrarySharedImageName :: Gen String
arbitrarySharedImageName =
    elements [printf "arbitrary-shared-img-name-%d" x | x <- [0 :: Int .. 3]]

instance Arbitrary SharedImageBuildId where
    arbitrary = pure $ SharedImageBuildId "000000000"

instance Arbitrary SharedImageDate where
    arbitrary = pure $ SharedImageDate "1970-01-01-00:00"

instance Arbitrary SharedImage where
    arbitrary =
        SharedImage <$> smaller arbitrary <*> smaller arbitrary <*>
        smaller arbitrary <*>
        smaller arbitrary <*>
        smaller arbitrary
