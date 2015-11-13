{-# LANGUAGE FlexibleInstances #-}
module B9.DSL
       (module X, BuildStep(..), Program, create, add, convert, export,
        Artifact(..), SArtifact(..), Handle(..), CreateSpec, AddSpec,
        ConvSpec, ExportSpec, ExportResult, singletonHandle, handle,
        imageRepositoryH, documentation, doc, ( # ), externalFile, use,
        fromFile, outputFile, variableBindings, ($=), addFile, addExe,
        addFileP, addTemplate, addTemplateP, addTemplateExe, addFileFull,
        addFileFromContent, createContent, appendContent, newDirectory,
        exportDir, newCloudInit, addMetaData, addUserData,
        writeCloudInitDir, writeCloudInitDir', writeCloudInit,
        addCloudInitToArtifact, fromShared, sharedAs, boot, lxc, lxc32,
        mountDir, mountDirRW, mount, runCommand, sh, rootImage, dataImage,
        mountAndShareSharedImage, mountAndShareNewImage, interpret,
        Interpreter(..))
       where

import B9.Content as X
import B9.DiskImages as X
import B9.ExecEnv as X
import B9.FileSystems as X
import B9.CommonTypes as X
import B9.Repository as X
import B9.PartitionTable as X
import B9.ShellScript as X (Script(..))
import Control.Lens hiding ((#), from, use, (<.>), uncons)
import Control.Monad.Free (Free(..), liftF, foldFree)
import Data.Binary
import Data.Data
import Data.Function (on)
import Data.Functor (void)
import GHC.Generics (Generic)
import System.FilePath
import Text.Printf (printf)

-- ---------------------------------------------------------

data BuildStep next where
    Create :: -- Inject
        (Show (CreateSpec a)) =>
        SArtifact a -> CreateSpec a -> (Handle a -> next) -> BuildStep next
    Add :: -- Compose w/o result
        (Show (AddSpec env a)) =>
        Handle env ->
          SArtifact a -> AddSpec env a -> next -> BuildStep next
    Convert :: -- Compose
        (Show (ConvSpec a b)) =>
        Handle a ->
          SArtifact b -> ConvSpec a b -> (Handle b -> next) -> BuildStep next
    Export :: -- Project
        (Show (ExportSpec a), Show (ExportResult a)) =>
        Handle a ->
          ExportSpec a -> (ExportResult a -> next) -> BuildStep next

instance Functor BuildStep where
    fmap f (Create sa src k)            = Create sa src (f . k)
    fmap f (Add hndEnv sa addSpec next) = Add hndEnv sa addSpec (f next)
    fmap f (Convert hA sB conv k)       = Convert hA sB conv (f . k)
    fmap f (Export hnd out k)           = Export hnd out (f . k)

type Program a = Free BuildStep a

-- ---------------------------------------------------------

-- | Declare an artifact.
create
    :: (Show (CreateSpec a))
    => SArtifact a -> CreateSpec a -> Program (Handle a)
create sa src = liftF $ Create sa src id

-- | Add an artifact to another artifact.
add
    :: (Show (AddSpec a b))
    => Handle a -> SArtifact b -> AddSpec a b -> Program ()
add hndA sB addSpec = liftF $ Add hndA sB addSpec ()

-- | Convert an artifact referenced by a handle to a different kind
--  of artifact and return the handle of the new artifact.
convert
    :: (Show (ConvSpec a b))
    => Handle a -> SArtifact b -> ConvSpec a b -> Program (Handle b)
convert hndA sB convSpec = liftF $ Convert hndA sB convSpec id

-- | Exports an artifact referenced by a handle a to a /real/ output,
-- i.e. something that is not necessarily referenced to by 'Handle'.
export
    :: (Show (ExportSpec a), Show (ExportResult a))
    => Handle a -> ExportSpec a -> Program (ExportResult a)
export hnd out = liftF $ Export hnd out id


-- ---------------------------------------------------------

data Artifact
    = VmImage
    | UpdateServerRoot
    | PartitionedVmImage
    | CloudInit
    | CloudInitMetaData
    | CloudInitUserData
    | Documentation
    | ExecutionEnvironment
    | TemplateVariable
    | ExecutableScript
    | GeneratedContent
    | VariableBindings
    | LocalDirectory
    | ExternalFile
    | FreeFile
    | FileSystemBuilder
    | FileSystemImage
    | ImageRepository
    deriving (Read,Show,Generic,Eq,Ord,Data,Typeable)

data SArtifact k where
    SVmImage              :: SArtifact 'VmImage
    SUpdateServerRoot     :: SArtifact 'UpdateServerRoot
    SPartitionedVmImage   :: SArtifact 'PartitionedVmImage
    SCloudInit            :: SArtifact 'CloudInit
    SCloudInitMetaData    :: SArtifact 'CloudInitMetaData
    SCloudInitUserData    :: SArtifact 'CloudInitUserData
    SDocumentation        :: SArtifact 'Documentation
    SExecutionEnvironment :: SArtifact 'ExecutionEnvironment
    STemplateVariable     :: SArtifact 'TemplateVariable
    SExecutableScript     :: SArtifact 'ExecutableScript
    SGeneratedContent     :: SArtifact 'GeneratedContent
    SVariableBindings     :: SArtifact 'VariableBindings
    SLocalDirectory       :: SArtifact 'LocalDirectory
    SExternalFile         :: SArtifact 'ExternalFile
    SFreeFile             :: SArtifact 'FreeFile
    SFileSystemBuilder    :: SArtifact 'FileSystemBuilder
    SFileSystemImage      :: SArtifact 'FileSystemImage
    SImageRepository      :: SArtifact 'ImageRepository

instance Show (SArtifact k) where
    show SVmImage              = "VmImage"
    show SUpdateServerRoot     = "UpdateServerRoot"
    show SPartitionedVmImage   = "PartitionedVmImage"
    show SCloudInit            = "CloudInit"
    show SCloudInitUserData    = "CloudInitUserData"
    show SCloudInitMetaData    = "CloudInitMetaData"
    show SDocumentation        = "Documentation"
    show SExecutionEnvironment = "ExecutionEnvironment"
    show STemplateVariable     = "TemplateVariable"
    show SExecutableScript     = "ExecutableScript"
    show SGeneratedContent     = "GeneratedContent"
    show SVariableBindings     = "VariableBindings"
    show SLocalDirectory       = "LocalDirectory"
    show SExternalFile         = "ExternalFile"
    show SFreeFile             = "FreeFile"
    show SFileSystemBuilder    = "FileSystemBuilder"
    show SFileSystemImage      = "FileSystemImage"
    show SImageRepository      = "ImageRepository"

instance Eq (SArtifact k) where
    x == y = show x == show y

instance Ord (SArtifact k) where
    compare = compare `on` show

-- ---------------------------------------------------------

-- | This type identifies everything that can be created or added in a 'Program'
data Handle (a :: Artifact) =
    Handle (SArtifact a)
           String
    deriving (Show,Eq,Ord)

-- | Create a 'Handle' that contains the string representation of the singleton
-- type as tag value.
singletonHandle :: SArtifact a -> Handle a
singletonHandle sa = Handle sa (show sa)

-- | Create a 'Handle' that contains a string.
handle :: SArtifact a -> String -> Handle a
handle = Handle

-- * Creation type families

type family CreateSpec (a :: Artifact) :: * where
    CreateSpec 'CloudInit            = String
    CreateSpec 'ExecutionEnvironment = ExecEnvSpec
    CreateSpec 'ExternalFile         = FilePath
    CreateSpec 'FileSystemBuilder    = FileSystemSpec
    CreateSpec 'FreeFile             = Maybe String
    CreateSpec 'GeneratedContent     = (Content, String)
    CreateSpec 'LocalDirectory       = ()

-- * Add type families

type family AddSpec (env :: Artifact) (a :: Artifact) :: * where
    AddSpec 'CloudInit 'CloudInitMetaData           = AST Content YamlObject
    AddSpec 'CloudInit 'CloudInitUserData           = AST Content YamlObject
    AddSpec 'CloudInit 'ExecutableScript            = Script
    AddSpec 'CloudInit 'FreeFile                    = (FileSpec, Handle 'FreeFile)
    AddSpec 'Documentation 'Documentation           = String
    AddSpec 'ExecutionEnvironment 'ExecutableScript = Script
    AddSpec 'ExecutionEnvironment 'FreeFile         = (FileSpec, Handle 'FreeFile)
    AddSpec 'ExecutionEnvironment 'LocalDirectory   = SharedDirectory
    AddSpec 'FileSystemBuilder 'FreeFile            = (FileSpec, Handle 'FreeFile)
    AddSpec 'GeneratedContent 'GeneratedContent     = Content
    AddSpec 'ImageRepository 'VmImage               = (SharedImageName, Handle 'VmImage)
    AddSpec 'LocalDirectory 'FreeFile               = (FileSpec, Handle 'FreeFile)
    AddSpec 'UpdateServerRoot 'VmImage              = (SharedImageName, Handle 'VmImage)
    AddSpec 'VariableBindings 'TemplateVariable     = (String, String)

-- * Conversion type families

type family ConvSpec (a :: Artifact) (b :: Artifact) :: * where
    ConvSpec 'CloudInit 'CloudInitMetaData        = ()
    ConvSpec 'CloudInit 'CloudInitUserData        = ()
    ConvSpec 'CloudInitMetaData 'GeneratedContent = ()
    ConvSpec 'CloudInitUserData 'GeneratedContent = ()
    ConvSpec 'ExecutionEnvironment 'VmImage       = (Handle 'VmImage, MountPoint)
    ConvSpec 'ExecutionEnvironment 'FreeFile      = FilePath
    ConvSpec 'ExternalFile 'FreeFile              = ()
    ConvSpec 'FileSystemBuilder 'FileSystemImage  = ()
    ConvSpec 'FileSystemBuilder 'FreeFile         = ()
    ConvSpec 'FileSystemBuilder 'VmImage          = ()
    ConvSpec 'FileSystemImage 'FileSystemImage    = FileSystemResize
    ConvSpec 'FileSystemImage 'FreeFile           = ()
    ConvSpec 'FileSystemImage 'VmImage            = ()
    ConvSpec 'FreeFile 'ExternalFile              = FilePath
    ConvSpec 'FreeFile 'FileSystemImage           = FileSystem
    ConvSpec 'FreeFile 'FreeFile                  = String
    ConvSpec 'FreeFile 'PartitionedVmImage        = ()
    ConvSpec 'FreeFile 'VmImage                   = ImageType
    ConvSpec 'GeneratedContent 'FreeFile          = ()
    ConvSpec 'ImageRepository 'VmImage            = SharedImageName
    ConvSpec 'LocalDirectory 'UpdateServerRoot    = ()
    ConvSpec 'PartitionedVmImage 'FreeFile        = PartitionSpec
    ConvSpec 'VmImage 'FileSystemImage            = ()
    ConvSpec 'VmImage 'FreeFile                   = ()
    ConvSpec 'VmImage 'VmImage                    = Either ImageType ImageSize

-- * Export type families

type family ExportSpec (a :: Artifact) :: * where
    ExportSpec 'FileSystemImage = FilePath
    ExportSpec 'FreeFile        = FilePath
    ExportSpec 'LocalDirectory  = FilePath
    ExportSpec 'VmImage         = FilePath

type family ExportResult (a :: Artifact) :: * where
    ExportResult a = ()

-- * Global Handles

-- | A Global handle repesenting the (local) share image repository.
imageRepositoryH :: Handle 'ImageRepository
imageRepositoryH = singletonHandle SImageRepository

-- * Inline documentation/comment support

-- | A handle representing the documentation gathered throughout a 'Program'
documentation :: Handle 'Documentation
documentation = singletonHandle SDocumentation

doc :: String -> Program ()
doc str = add documentation SDocumentation str

(#) :: Program a -> String -> Program a
m # str = do
    doc str
    m

-- * Incorporating external files

-- | Reference an external file. An external file will never be modified.
externalFile :: FilePath -> Program (Handle 'ExternalFile)
externalFile = create SExternalFile

-- | Get a reference to a copy of an external file inside the build root that
-- can be modified.
use :: FilePath -> Program (Handle 'FreeFile)
use f = do
    extH <- externalFile f
    convert extH SFreeFile ()

-- | 'use' an external file to introduce an artifact with the help of a
--  artifact dependent extra arguement and return the artifacts handle.
fromFile
    :: Show (ConvSpec 'FreeFile b)
    => FilePath -> SArtifact b -> ConvSpec 'FreeFile b -> Program (Handle b)
fromFile f a conversionArg = do
    h <- use f
    convert h a conversionArg

-- | Given an artifact that support extraction or conversion to a file
-- create/write a file to a given output path.
outputFile
    :: Show (ConvSpec a 'FreeFile)
    => Handle a -> ConvSpec a 'FreeFile -> FilePath -> Program ()
outputFile e src dst = do
    outF <- convert e SFreeFile src
    export outF dst

-- * Template support

-- | A handle representing the environment holding all template variable
-- bindings.
variableBindings :: Handle 'VariableBindings
variableBindings = singletonHandle SVariableBindings

($=) :: String -> String -> Program ()
var $= val = add variableBindings STemplateVariable (var, val)

-- * File Inclusion, File-Templating and Script Rendering

-- | Add an existing file to an artifact.
-- Strip the directories from the path, e.g. @/etc/blub.conf@ will be
-- @blob.conf@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addFile
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> Program ()
addFile d f = addFileP d f (0, 6, 4, 4)

-- | Same as 'addFile' but set the destination file permissions to @0755@
-- (executable for all).
addExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> Program ()
addExe d f = addFileP d f (0, 7, 5, 5)

-- | Same as 'addFile' but with an extra output file permission parameter.
addFileP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> Program ()
addFileP d f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
        srcFile = Source NoConversion f
    addFileFull d srcFile dstSpec

-- | Generate a file to an artifact from a local file template.
-- All occurences of @${var}@ will be replaced by the contents of @var@, which
-- is the last values assigned to @var@ using @"var" $= "123"@. The directory
-- part is stripped from the output file name, e.g. @template/blah/foo.cfg@ will
-- be @foo.cfg@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addTemplate
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> Program ()
addTemplate d f = do
  addTemplateP d f (0,6,4,4)

-- | Same as 'addTemplate' but set the destination file permissions to @0755@
-- (executable for all).
addTemplateExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> Program ()
addTemplateExe d f = do
    addTemplateP d f (0, 6, 4, 4)

-- | Same as 'addTemplate' but with an extra output file permission parameter.
addTemplateP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> Program ()
addTemplateP d f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
        srcFile = Source ExpandVariables f
    addFileFull d srcFile dstSpec

-- | Add an existing file from the file system, optionally with template
-- variable expansion to an artifact at a 'FileSpec'.
addFileFull
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> SourceFile -> FileSpec -> Program ()
addFileFull dstH srcFile dstSpec =
    case srcFile of
        (Source ExpandVariables _) ->
            addFileFromContent dstH (FromTextFile srcFile) dstSpec
        (Source NoConversion f) -> do
            origH <- create SExternalFile f
            tmpH <- convert origH SFreeFile ()
            add dstH SFreeFile (dstSpec, tmpH)

-- | Generate a file with a content and add that file to an artifact at a
-- 'FileSpec'.
addFileFromContent
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle e -> Content -> FileSpec -> Program ()
addFileFromContent dstH content dstSpec = do
    cH <-
        createContent
            content
            (printf
                 "contents-of-%s"
                 (dstSpec ^. fileSpecPath . to takeFileName))
    tmpFileH <- convert cH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpFileH)

-- * /Low-level/ 'Content' generation functions

-- | Create a handle for accumulating 'Content' with an initial 'Content'.
createContent :: Content -> String -> Program (Handle 'GeneratedContent)
createContent c title = create SGeneratedContent (c, title)

-- | Accumulate/Append more 'Content' to the 'GeneratedContent'
--   handle obtained by e.g. 'createContent'
appendContent :: Handle 'GeneratedContent -> Content -> Program ()
appendContent hnd c = add hnd SGeneratedContent c

-- * directories

-- | Create a temp directory
newDirectory :: Program (Handle 'LocalDirectory)
newDirectory = create SLocalDirectory ()

-- | Render the directory to the actual destination (which must not exist)
exportDir :: (Handle 'LocalDirectory) -> FilePath -> Program ()
exportDir dirH dest = export dirH dest

-- * cloud init

newCloudInit :: String -> Program (Handle 'CloudInit)
newCloudInit iid = create SCloudInit iid

addMetaData :: Handle 'CloudInit -> AST Content YamlObject -> Program ()
addMetaData hnd ast = add hnd SCloudInitMetaData ast

addUserData :: Handle 'CloudInit -> AST Content YamlObject -> Program ()
addUserData hnd ast = add hnd SCloudInitUserData ast

writeCloudInitDir :: Handle 'CloudInit -> FilePath -> Program ()
writeCloudInitDir h dst = void $ writeCloudInitDir' h dst

writeCloudInitDir' :: Handle 'CloudInit -> FilePath -> Program ()
writeCloudInitDir' h dst = do
    dirH <- newDirectory
    addCloudInitToArtifact h dirH
    export dirH dst

writeCloudInit :: Handle 'CloudInit -> FileSystem -> FilePath -> Program ()
writeCloudInit h fs dst = do
    fsBuilder <- create SFileSystemBuilder (FileSystemSpec fs "cidata" 2 MB)
    fsImage <- convert fsBuilder SFileSystemImage ()
    export fsImage dst
    addCloudInitToArtifact h fsBuilder

addCloudInitToArtifact
    :: (AddSpec a 'FreeFile ~ (FileSpec, Handle 'FreeFile))
    => Handle 'CloudInit -> Handle a -> Program ()
addCloudInitToArtifact chH destH = do
    metaData <- convert chH SCloudInitMetaData ()
    metaDataContent <- convert metaData SGeneratedContent ()
    metaDataFile <- convert metaDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "meta-data", metaDataFile)
    userData <- convert chH SCloudInitUserData ()
    userDataContent <- convert userData SGeneratedContent ()
    userDataFile <- convert userDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "user-data", userDataFile)

-- * Image import

fromShared :: String -> Program (Handle 'VmImage)
fromShared sharedImgName = do
    convert imageRepositoryH SVmImage (SharedImageName sharedImgName)

-- * Image export

-- | Store an image in the local cache with a name as key for lookups, e.g. from
-- 'fromShared'
sharedAs :: Handle 'VmImage -> String -> Program ()
sharedAs hnd name = do
    add imageRepositoryH SVmImage (SharedImageName name, hnd)

-- * Execution environment

boot :: ExecEnvSpec -> Program (Handle 'ExecutionEnvironment)
boot = create SExecutionEnvironment

lxc :: String -> Program (Handle 'ExecutionEnvironment)
lxc name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 X86_64)

lxc32 :: String -> Program (Handle 'ExecutionEnvironment)
lxc32 name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 I386)

-- * Mounting

mountDir :: Handle 'ExecutionEnvironment -> FilePath -> FilePath -> Program ()
mountDir e hostDir dest =
    add e SLocalDirectory (SharedDirectoryRO hostDir (MountPoint dest))

mountDirRW :: Handle 'ExecutionEnvironment -> FilePath -> FilePath -> Program ()
mountDirRW e hostDir dest =
    add e SLocalDirectory (SharedDirectory hostDir (MountPoint dest))

mount
    :: Handle 'ExecutionEnvironment
    -> Handle 'VmImage
    -> FilePath
    -> Program (Handle 'VmImage)
mount e imgHnd dest = convert e SVmImage (imgHnd, MountPoint dest)

-- * Script Execution (inside a container)

runCommand
    :: (Show (AddSpec a 'ExecutableScript))
    => Handle a -> AddSpec a 'ExecutableScript -> Program ()
runCommand hnd s = add hnd SExecutableScript s

sh
    :: (AddSpec a 'ExecutableScript ~ Script)
    => Handle a -> String -> Program ()
sh e s = runCommand e (Run s [])

-- * Some utility vm builder lego

rootImage :: String -> String -> Handle 'ExecutionEnvironment -> Program ()
rootImage nameFrom nameExport env =
    void $ mountAndShareSharedImage nameFrom nameExport "/" env

dataImage :: String -> Handle 'ExecutionEnvironment -> Program ()
dataImage nameExport env =
    void $ mountAndShareNewImage "data" 64 nameExport "/data" env

mountAndShareSharedImage :: String
                         -> String
                         -> String
                         -> Handle 'ExecutionEnvironment
                         -> Program ()
mountAndShareSharedImage nameFrom nameTo mountPoint env = do
    i <- fromShared nameFrom
    i' <- mount env i mountPoint
    i' `sharedAs` nameTo

mountAndShareNewImage
    :: String
    -> Int
    -> String
    -> FilePath
    -> Handle 'ExecutionEnvironment
    -> Program ()
mountAndShareNewImage label sizeGB nameTo mountPoint env = do
  fs <- create SFileSystemBuilder (FileSystemSpec Ext4 label sizeGB GB)
  fi <- convert fs SFileSystemImage ()
  i <- convert fi SVmImage ()
  i' <- mount env i mountPoint
  i' `sharedAs` nameTo

-- * DSL Interpreter

-- | Interpret a `Program` using an `Interpreter` monad.
interpret
    :: Interpreter m
    => Program b -> m b
interpret = foldFree runInterpreter
  where
    runInterpreter (Create sa src k) = do
        hnd <- runCreate sa src
        return (k hnd)
    runInterpreter (Add hnde sa addSpec next) = do
        runAdd hnde sa addSpec
        return next
    runInterpreter (Convert hA sB conv k) = do
        res <- runConvert hA sB conv
        return (k res)
    runInterpreter (Export hnd out k) = do
        res <- runExport hnd out
        return (k res)

-- | Monads that interpret build steps
class (Monad f) => Interpreter f  where
    runCreate
        :: (Show (CreateSpec a))
        => SArtifact a -> CreateSpec a -> f (Handle a)
    runAdd
        :: (Show (AddSpec a b))
        => Handle a -> SArtifact b -> AddSpec a b -> f ()
    runConvert
        :: (Show (ConvSpec a b))
        => Handle a -> SArtifact b -> ConvSpec a b -> f (Handle b)
    runExport
        :: (Show (ExportSpec a), Show (ExportResult a))
        => Handle a -> ExportSpec a -> f (ExportResult a)

-- * QuickCheck instances
