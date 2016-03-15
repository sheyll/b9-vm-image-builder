module B9.Dsl.CloudInit where

import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Content
import B9.Dsl.Core
import B9.Dsl.ExecutionEnvironment
import B9.Dsl.File
import B9.ShellScript              (toBashOneLiner, Script(..))
import Control.Lens
import Data.Data
import Data.Monoid
import Data.Singletons.TH          hiding ((%~))
import Data.Time.Clock
import Data.Word
import Text.Printf                 (printf)

-- * Cloud-init API
$(singletons
    [d|

  data CloudInitArtifact = CloudInitMetaData
                         | CloudInitUserData
                         deriving Show
  |])

type instance CreateSpec 'CloudInitMetaData = () -- TODO Maybe (Handle
                                                -- FreeFile)??
type instance AddSpec 'CloudInitUserData 'ExecutableScript = Script

type instance AddSpec 'CloudInitUserData 'FreeFile =
     (FileSpec, Handle 'FreeFile)

type instance AddSpec 'CloudInitUserData 'CloudInitUserData = CiUserData

type instance ConvSpec 'CloudInitUserData 'GeneratedContent = ()

-- | Representation of a @cloud-config@ @user-data@ document.
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

type instance IoCompilerArtifactState 'CloudInitUserData =
     CiUserData

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

makeLenses ''CiUserData


-- | Representation of a @cloud-init@ @meta-data@ document
type instance CreateSpec 'CloudInitMetaData = () -- TODO Maybe (Handle
                                                -- FreeFile)??

type instance AddSpec 'CloudInitMetaData 'CloudInitMetaData = CiMetaData

type instance ConvSpec 'CloudInitMetaData 'GeneratedContent = ()

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

type instance IoCompilerArtifactState 'CloudInitMetaData =
     CiMetaData

data CiNetworkInterface =
  CiNetworkInterface String -- TODO
  deriving (Show,Typeable)


instance CanCreate IoCompiler 'CloudInitUserData where
  runCreate _ _ =
    do (hnd,_) <- allocHandle SCloudInitUserData "user-data"
       putArtifactState hnd mempty
       return hnd

instance CanAdd IoCompiler 'CloudInitUserData 'CloudInitUserData where
  runAdd hnd _ userdata = appendArtifactState hnd userdata

instance CanCreate IoCompiler 'CloudInitMetaData where
  runCreate _ _ =
    do (hnd,_) <- allocHandle SCloudInitMetaData "meta-data"
       putArtifactState hnd mempty
       return hnd

instance CanAdd IoCompiler 'CloudInitMetaData 'CloudInitMetaData where
  runAdd hnd _ metadata = appendArtifactState hnd metadata

instance CanAdd IoCompiler 'CloudInitUserData 'ExecutableScript where
  runAdd hnd _ scr =
    interpret $
    add hnd SCloudInitUserData (mempty {_ciRunCmds = [CiCmdScript scr]})

instance CanAdd IoCompiler 'CloudInitUserData 'FreeFile where
  runAdd hnd _ (fspec,fH) =
    do fH --> hnd
       interpret $
         do fileContent <- convert fH SGeneratedContent ()
            add hnd
                SCloudInitUserData
                (mempty {_ciWriteFiles = [CiWriteFile fileContent fspec]})

instance CanConvert IoCompiler 'CloudInitMetaData 'GeneratedContent where
  runConvert (Handle _ h) _ () = return (Handle SGeneratedContent h)

instance CanConvert IoCompiler 'CloudInitUserData 'GeneratedContent where
  runConvert (Handle _ h) _ () = return (Handle SGeneratedContent h)

-- | Create a @cloud-config@ compatibe @write_files@ 'AST' object.
toUserDataWriteFilesAST
  :: FileSpec -> Content -> AST Content YamlObject
toUserDataWriteFilesAST (FileSpec fileName (s,u,g,o) userName groupName) content =
  ASTObj [("write_files"
          ,ASTArr [ASTObj [("path",ASTString fileName)
                          ,("owner",ASTString (userName ++ ":" ++ groupName))
                          ,("permissions",ASTString (printf "%i%i%i%i" s u g o))
                          ,("content",ASTEmbed content)]])]

-- | Create a @cloud-config@ compatibe @runcmd@ 'AST' object.
toUserDataRunCmdAST
  :: Script -> AST Content YamlObject
toUserDataRunCmdAST scr = ASTObj [("runcmd",ASTArr [ASTString cmd])]
  where cmd = toBashOneLiner scr
