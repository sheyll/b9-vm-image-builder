module B9.Designing where

import GHC.TypeLits
import Data.Proxy
import Data.Monoid

-- | A directory that a service needs
data Directory (dir :: Symbol) (mode :: DirectoryMode)

--
data DirectoryMode =
  -- | A readonly directory
   ReadOnly

  -- | A directory that is writable *and* persistent, e.g. is not
  -- overwritten/deleted during reboots or even software updates.
  | ReadWrite

  -- | A directory that is writable, but any content, written during service
  -- runtime will be deleted by restarts.
  | Volatile



newtype WithT (tag :: k) m a = WithT { runWithT :: m a}

instance (KnownSymbol dir) => HasCloudInitInstaller (Directory dir mode) where
  type CloudInitInstallerT (Directory dir mode) m = WithT (Directory dir mode) m
  createCloudInit Proxy action = runCommand


class HasCloudInitInstaller service where
  -- | The type of the actual installer callback function that will create the
  -- cloud-init for a service in some Monad
  type CloudInitInstallerT service (m :: * -> *) :: * -> *

  createCloudInit :: Proxy service -> CloudInitInstallerT service -> Ci

-- Low-level install

type ScriptOut = (ExitCode, String, String)
data Script a =
  Run String [String] (ScriptOut -> a)
  | WriteFile FilePath String a
  | ReadFile FilePath (String -> a)

-- | High-level install Representation of a @cloud-config@ @user-data@ document.

data Ci = Ci { _ciUserData :: CiUserData
             , _ciMetaData :: CiMetaData
             }
  deriving (Show,Typeable)

instance Monoid Ci where
  mempty = Ci mempty mempty
  mappend (Ci ul ml) (Ci ur mr) = (Ci (ul <> ur) (ml <> mr))

data CiUserData =
  CiUserData {_ciGroups :: [CiGroup]
             ,_ciUser :: [CiUser]
             ,_ciWriteFiles :: [CiWriteFile]
             ,_ciCaCerts :: CiCaCerts
             ,_ciBootCmds :: [CiCmd]
             ,_ciRunCmds :: [CiCmd]
             ,_ciCompletionMessage :: Maybe String
             ,_ciMounts :: [CiMount]
             ,_ciPhoneHome :: Maybe CiPhoneHome
             ,_ciPowerState :: Maybe CiPowerState}
  deriving (Show,Typeable)

instance Monoid CiUserData where
  mempty = CiUserData [] [] [] mempty [] [] Nothing [] Nothing Nothing
  mappend (CiUserData groups users writeFiles caCerts bootCmds runCmds complMsg mounts phoneHome powerState) (CiUserData groups' users' writeFiles' caCerts' bootCmds' runCmds' complMsg' mounts' phoneHome' powerState') =
    CiUserData (groups <> groups')
               (users <> users')
               (writeFiles <> writeFiles')
               (caCerts <> caCerts')
               (bootCmds <> bootCmds')
               (runCmds <> runCmds')
               (getLast (Last complMsg <> Last complMsg'))
               (mounts <> mounts')
               (getLast (Last phoneHome <> Last phoneHome'))
               (getLast (Last powerState <> Last powerState'))

data CiGroup =
  CiGroup {_ciGroupName :: String
          ,_ciGroupMembers :: [String]}
  deriving (Show,Typeable)

data CiUser
  = CiUserDefault
  | CiUser {_ciUserName :: String
           ,_ciUserGecos :: String
           ,_ciUserInactive :: Bool
           ,_ciUserSystemUser :: Bool
           ,_ciUserHomeDir :: Maybe FilePath
           ,_ciUserNoCreateHome :: Bool
           ,_ciUserNoUserGroup :: Bool
           ,_ciUserPasswordLocked :: Bool
           ,_ciUserSudo :: Maybe CiUserSudo
           ,_ciUserPrimaryGroup :: Maybe String
           ,_ciUserOtherGroups :: [String]
           ,_ciUserSELinuxUser :: Maybe String
           ,_ciUserExpireDate :: Maybe UTCTime
           ,_ciUserSshAuthorizedKeys :: [String]
           ,_ciUserSshImportId :: Maybe String}
  deriving (Show,Typeable)

data CiUserSudo
  = AllNoPasswd
  | NoPasswd [String]
  | AllWithPasswd
  | WithPasswd [String]
  deriving (Show,Typeable)

data CiWriteFile =
  CiWriteFile {_ciWriteFileContent :: Handle 'GeneratedContent
              ,_ciWriteFile :: FileSpec}
  deriving (Show,Typeable)

data CiCaCerts =
  CiCaCerts {_ciCaCertRemoveDefaults :: Bool
            ,_ciCaCertTrusted :: [CiCaCert]}
  deriving (Show,Typeable)

instance Monoid CiCaCerts where
  mempty = CiCaCerts False []
  mappend (CiCaCerts removeDefault trusted) (CiCaCerts removeDefault' trusted') =
    CiCaCerts (getAny (Any removeDefault <> Any removeDefault'))
              (trusted <> trusted')

data CiCaCert =
  CiCaCert {_ciCaCert :: Handle 'GeneratedContent}
  deriving (Show,Typeable)

data CiCmd
  = CiCmd {_ciCmd :: String
          ,_ciCmdArgs :: [String]}
  | CiCmdScript Script
  deriving (Show,Typeable)

data CiMount =
  CiMount {_ciMountBlockDevice :: CiMountFsSpec
          ,_ciMountPoint :: Maybe FilePath
          ,_ciMountFileSystemType :: String
          ,_ciMountOpts :: [String]
          ,_ciMountFreq :: Maybe Word8
          ,_ciMountPassno :: Maybe Word8}
  deriving (Show,Typeable)

data CiMountFsSpec
  = CiMountFsSpecDevice String
  | CiMountFsSpecPartLabel String
  | CiMountFsSpecPartUUID String
  | CiMountFsSpecDiskLabel String
  | CiMountFsSpecDiskUUID String
  | CiMountFsSpecNFS String
                     FilePath
  | CiMountFsSpecNoStorage String
  deriving (Show,Typeable)

data CiPhoneHome =
  CiPhoneHome {_ciPhoneHomeURL :: String
              ,_ciPhoneHomePostOnly :: Maybe [String]
              ,_ciPhoneHomeTries :: Word8}
  deriving (Show,Typeable)

data CiPowerState =
  CiPowerState {_ciPowerStateDelay :: Maybe Word8
               ,_ciPowerStateMode :: CiPowerStateMode
               ,_ciPowerStateMessage :: Maybe String
               ,_ciPowerStateTimeout :: Maybe Word8
               ,_ciPowerStateCondition :: Either Bool CiCmd}
  deriving (Show,Typeable)

data CiPowerStateMode
  = CiPowerStateOff
  | CiPowerStateHalt
  | CiPowerStateReboot
  deriving (Show,Typeable)

data CiMetaData =
  CiMetaData {_ciMetaDataInstanceId :: String
             ,_ciMetaDataNetworkInterfaces :: [CiNetworkInterface]
             ,_ciMetaDataHostname :: Maybe String
             ,_ciMetaDataLocalHostname :: Maybe String
             ,_ciMetaDataFQDN :: Maybe String}
  deriving (Show,Typeable)

instance Monoid CiMetaData where
  mempty = CiMetaData "" [] Nothing Nothing Nothing
  mappend (CiMetaData iid networkInterfaces hostname localHostname fqdn) (CiMetaData iid' networkInterfaces' hostname' localHostname' fqdn') =
    CiMetaData (iid <> iid')
               (networkInterfaces <> networkInterfaces')
               (getLast (Last hostname <> Last hostname'))
               (getLast (Last localHostname <> Last localHostname'))
               (getLast (Last fqdn <> Last fqdn'))

data CiNetworkInterface =
  CiNetworkInterface String -- TODO
  deriving (Show,Typeable)
