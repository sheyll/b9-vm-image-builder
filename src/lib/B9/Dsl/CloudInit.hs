module B9.Dsl.CloudInit where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Content
import B9.Dsl.Core
import B9.Dsl.ExecutionEnvironment
import B9.Dsl.File
import B9.ShellScript              (toBashOneLiner, Script(..))
import Control.Lens
import Data.Data
import Data.Default
import Data.Singletons.TH          hiding ((%~))
import Data.Time.Clock
import Data.Word
import System.FilePath
import Text.Printf                 (printf)

-- * Cloud-init API

$(singletons
    [d|

  data CloudInitArtifact = CloudInit
                         | CloudInitMetaData
                         | CloudInitUserData
                         deriving Show
  |])

type instance CreateSpec 'CloudInit = String
type instance AddSpec 'CloudInit 'CloudInitMetaData =
     IoProgBuilder (AST Content YamlObject)
type instance AddSpec 'CloudInit 'CloudInitUserData =
     IoProgBuilder (AST Content YamlObject)
type instance AddSpec 'CloudInit 'ExecutableScript = Script
type instance AddSpec 'CloudInit 'FreeFile =
     (FileSpec, Handle 'FreeFile)
type instance ConvSpec 'CloudInit 'CloudInitMetaData = ()
type instance ConvSpec 'CloudInit 'CloudInitUserData = ()
type instance ConvSpec 'CloudInitMetaData 'GeneratedContent = ()
type instance ConvSpec 'CloudInitUserData 'GeneratedContent = ()

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx =
  CiCtx {_ciUserData :: CiUserData
        ,_ciMetaData :: CiMetaData}

data CiUserData =
  CiUserData {_ciGroups :: [CiGroup]
             ,_ciUser :: [CiUser]
             ,_ciWriteFiles :: [CiWriteFile]
             ,_ciCaCerts :: CiCaCerts
             ,_ciBootCmd :: [CiCmd]
             ,_ciRunCmd :: [CiCmd]
             ,_ciCompletionMessage :: Maybe String
             ,_ciMounts :: [CiMount]
             ,_ciPhoneHome :: Maybe CiPhoneHome
             ,_ciPowerState :: Maybe CiPowerState}
  deriving (Show,Typeable)

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
              ,_ciWriteFilePath :: FilePath
              ,_ciWriteFilePermissions :: Maybe String
              ,_ciWriteFileOwner :: Maybe String}
  deriving (Show,Typeable)

data CiCaCerts =
  CiCaCerts {_ciCaCertRemoveDefaults :: Bool
            ,_ciCaCertTrusted :: [CiCaCert]}
  deriving (Show,Typeable)

data CiCaCert =
  CiCaCert {_ciCaCert :: Handle 'GeneratedContent}
  deriving (Show,Typeable)

data CiCmd =
  CiCmd {_ciCmd :: String
        ,_ciCmdArgs :: [String]}
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

data CiNetworkInterface =
  CiNetworkInterface String -- TODO
  deriving (Show,Typeable)

instance Default CiCtx where
  def =
    CiCtx (globalHandle SGeneratedContent)
          (globalHandle SGeneratedContent)

makeLenses ''CiCtx

instance CanCreate IoCompiler 'CloudInit where
  runCreate _ iidPrefix =
    do buildId <- liftIoProgram getBuildId
       (hnd@(Handle _ iid),_) <-
         allocHandle SCloudInit
                     ("cloudinit-" ++ iidPrefix ++ "-" ++ buildId)
       mH <-
         runCreate SGeneratedContent
                   (Concat [FromString "#cloud-config\n"
                           ,RenderYaml (ASTObj [("instance-id",ASTString iid)])]
                   ,iidPrefix ++ "-meta-data")
       hnd --> mH
       uH <-
         runCreate SGeneratedContent
                   (Concat [FromString "#cloud-config\n",RenderYaml (ASTObj [])]
                   ,iidPrefix ++ "-user-data")
       hnd --> uH
       putArtifactState hnd $ CiCtx mH uH
       return hnd

instance CanAdd IoCompiler 'CloudInit 'CloudInitMetaData where
  runAdd hnd _ ast =
    do Just (CiCtx mH _) <- useArtifactState hnd
       modifyArtifactState mH $
         fmap $
         \(Concat [hdr,RenderYaml ast']) ->
           Concat [hdr,RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'CloudInitUserData where
  runAdd hnd _ ast =
    do Just (CiCtx _ uH) <- useArtifactState hnd
       modifyArtifactState uH $
         fmap $
         \(Concat [hdr,RenderYaml ast']) ->
           Concat [hdr,RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'ExecutableScript where
  runAdd hnd _ scr =
    interpret $ add hnd SCloudInitUserData (toUserDataRunCmdAST scr)-- runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)

instance CanAdd IoCompiler 'CloudInit 'FreeFile where
  runAdd hnd _ (fspec,fH) =
    do fH --> hnd
       srcCopy <-
         freeFileTempCopy fH
                          (Just "content-reader")
       runAdd hnd
              SCloudInitUserData
              (liftIoProgram
                 ((toUserDataWriteFilesAST fspec . FromBinary) <$>
                  readContentFromFile srcCopy))


instance CanConvert IoCompiler 'CloudInit 'CloudInitMetaData where
  runConvert hnd _ () =
    do Just (CiCtx (Handle SGeneratedContent h) _) <- useArtifactState hnd
       return (Handle SCloudInitMetaData h)

instance CanConvert IoCompiler 'CloudInit 'CloudInitUserData where
  runConvert hnd _ () =
    do Just (CiCtx _ (Handle SGeneratedContent h)) <- useArtifactState hnd
       return (Handle SCloudInitUserData h)

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
