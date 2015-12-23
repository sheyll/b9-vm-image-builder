module B9.Dsl.CloudInit where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Content
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.FileSystem
import B9.Dsl.ShellScript
import B9.ShellScript      (toBashOneLiner, Script(..))
import Control.Lens
import Control.Monad.Trans
import Data.Data
import Data.Default
import Data.Singletons.TH  hiding ((%~))
import System.FilePath
import Text.Printf         (printf)

-- * Cloud-init API

$(singletons
  [d|
    data CloudInitArtifact
     = CloudInit
     | CloudInitMetaData
     | CloudInitUserData
     deriving (Show)
   |])

type instance CreateSpec 'CloudInit = String
type instance AddSpec 'CloudInit 'CloudInitMetaData =
     AST Content YamlObject
type instance AddSpec 'CloudInit 'CloudInitUserData =
     AST Content YamlObject
type instance AddSpec 'CloudInit 'ExecutableScript = Script
type instance AddSpec 'CloudInit 'FreeFile =
     (FileSpec, Handle 'FreeFile)
type instance ConvSpec 'CloudInit 'CloudInitMetaData = ()
type instance ConvSpec 'CloudInit 'CloudInitUserData = ()
type instance ConvSpec 'CloudInitMetaData 'GeneratedContent = ()
type instance ConvSpec 'CloudInitUserData 'GeneratedContent = ()

-- | Create a new cloud init context
newCloudInit :: (CanCreate m 'CloudInit)
                => String -> ProgramT m (Handle 'CloudInit)
newCloudInit = create SCloudInit

-- | Add an abstract syntax tree of a document to the __meta data__ of a the cloud
-- init context
addMetaData :: (CanAdd m 'CloudInit 'CloudInitMetaData)
               => Handle 'CloudInit -> AST Content YamlObject -> ProgramT m ()
addMetaData hnd = add hnd SCloudInitMetaData

-- | Add an abstract syntax tree of a document to the __user data__ of a the cloud
-- init context
addUserData :: (CanAdd m 'CloudInit 'CloudInitUserData)
               => Handle 'CloudInit -> AST Content YamlObject -> ProgramT m ()
addUserData hnd = add hnd SCloudInitUserData

-- | Render the cloud-init contents to a directory.
writeCloudInitDir :: (CanCreate m 'LocalDirectory
                     ,CanExport m 'LocalDirectory
                     ,CanAdd m 'LocalDirectory 'FreeFile
                     ,CanConvert m 'CloudInit 'CloudInitMetaData
                     ,CanConvert m 'CloudInit 'CloudInitUserData
                     ,CanConvert m 'CloudInitMetaData 'GeneratedContent
                     ,CanConvert m 'CloudInitUserData 'GeneratedContent
                     ,CanConvert m 'GeneratedContent 'FreeFile)
                     => Handle 'CloudInit -> FilePath -> ProgramT m ()
writeCloudInitDir h dst = do
    dirH <- newDirectory
    addCloudInitToArtifact h dirH
    export dirH dst

-- | Render the cloud-init contents into an ISO or VFAT image.
writeCloudInit :: (CanCreate m 'FileSystemBuilder
                  ,CanConvert m 'FileSystemBuilder 'FileSystemImage
                  ,CanExport m 'FileSystemImage
                  ,CanAdd m 'LocalDirectory 'FreeFile
                  ,CanAdd m 'FileSystemBuilder 'FreeFile
                  ,CanConvert m 'CloudInit 'CloudInitMetaData
                  ,CanConvert m 'CloudInit 'CloudInitUserData
                  ,CanConvert m 'CloudInitMetaData 'GeneratedContent
                  ,CanConvert m 'CloudInitUserData 'GeneratedContent
                  ,CanConvert m 'GeneratedContent 'FreeFile)
                  => Handle 'CloudInit
                  -> FileSystem
                  -> FilePath
                  -> ProgramT m ()
writeCloudInit h fs dst = do
    fsBuilder <- create SFileSystemBuilder (FileSystemSpec fs "cidata" 2 MB)
    fsImage <- convert fsBuilder SFileSystemImage ()
    export fsImage dst
    addCloudInitToArtifact h fsBuilder

-- | Render the cloud-init contents into an artifact to which 'FreeFile's can be
-- added.
addCloudInitToArtifact
    :: (AddSpec a 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanConvert m 'CloudInit 'CloudInitMetaData
       ,CanConvert m 'CloudInit 'CloudInitUserData
       ,CanConvert m 'CloudInitMetaData 'GeneratedContent
       ,CanConvert m 'CloudInitUserData 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m a 'FreeFile)
    => Handle 'CloudInit -> Handle a -> ProgramT m ()
addCloudInitToArtifact chH destH = do
    metaData <- convert chH SCloudInitMetaData ()
    metaDataContent <- convert metaData SGeneratedContent ()
    metaDataFile <- convert metaDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "meta-data", metaDataFile)
    userData <- convert chH SCloudInitUserData ()
    userDataContent <- convert userData SGeneratedContent ()
    userDataFile <- convert userDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "user-data", userDataFile)

-- * Implementation

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaDataH :: Handle 'GeneratedContent
    , _userDataH :: Handle 'GeneratedContent
    } deriving (Show, Typeable)

instance Default CiCtx where
    def =
        CiCtx
            (globalHandle SGeneratedContent)
            (globalHandle SGeneratedContent)

makeLenses ''CiCtx

instance CanCreate IoCompiler 'CloudInit where
    runCreate _ iidPrefix = do
        buildId <- lift getBuildId
        (hnd@(Handle _ iid),_) <-
            allocHandle
                SCloudInit
                ("cloudinit-" ++ iidPrefix ++ "-" ++ buildId)
        mH <-
            runCreate
                SGeneratedContent
                ( Concat
                      [ FromString "#cloud-config\n"
                      , RenderYaml (ASTObj [("instance-id", ASTString iid)])]
                , iidPrefix ++ "-meta-data")
        hnd --> mH
        uH <-
            runCreate
                SGeneratedContent
                ( Concat [FromString "#cloud-config\n", RenderYaml (ASTObj [])]
                , iidPrefix ++ "-user-data")
        hnd --> uH
        putArtifactState hnd $ CiCtx mH uH
        return hnd

instance CanAdd IoCompiler 'CloudInit 'CloudInitMetaData where
    runAdd hnd _ ast = do
        Just (CiCtx mH _) <- useArtifactState hnd
        modifyArtifactState mH $ fmap $
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'CloudInitUserData where
    runAdd hnd _ ast = do
        Just (CiCtx _ uH) <- useArtifactState hnd
        modifyArtifactState uH $ fmap $
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'ExecutableScript where
    runAdd hnd _ scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)

instance CanAdd IoCompiler 'CloudInit 'FreeFile where
    runAdd hnd _ (fspec,fH) = do
        fH --> hnd
        fName <- freeFileTempCopy fH (takeFileName (fspec ^. fileSpecPath))
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))

instance CanConvert IoCompiler 'CloudInit 'CloudInitMetaData where
    runConvert hnd _ () = do
        Just (CiCtx (Handle SGeneratedContent h) _) <- useArtifactState hnd
        return (Handle SCloudInitMetaData h)

instance CanConvert IoCompiler 'CloudInit 'CloudInitUserData where
    runConvert hnd _ () = do
        Just (CiCtx _ (Handle SGeneratedContent h)) <- useArtifactState hnd
        return (Handle SCloudInitUserData h)

instance CanConvert IoCompiler 'CloudInitMetaData 'GeneratedContent where
    runConvert (Handle _ h) _ () =
        return (Handle SGeneratedContent h)

instance CanConvert IoCompiler 'CloudInitUserData 'GeneratedContent where
    runConvert (Handle _ h) _ () =
        return (Handle SGeneratedContent h)


-- | Create a @cloud-config@ compatibe @write_files@ 'AST' object.
toUserDataWriteFilesAST :: FileSpec -> Content -> AST Content YamlObject
toUserDataWriteFilesAST (FileSpec fileName (s,u,g,o) userName groupName) content =
    ASTObj
        [ ( "write_files"
          , ASTArr
                [ ASTObj
                      [ ("path", ASTString fileName)
                      , ("owner", ASTString (userName ++ ":" ++ groupName))
                      , ("permissions", ASTString (printf "%i%i%i%i" s u g o))
                      , ("content", ASTEmbed content)]])]

-- | Create a @cloud-config@ compatibe @runcmd@ 'AST' object.
toUserDataRunCmdAST :: Script -> AST Content YamlObject
toUserDataRunCmdAST scr = ASTObj [("runcmd", ASTArr [ASTString cmd])]
  where
    cmd = toBashOneLiner scr
