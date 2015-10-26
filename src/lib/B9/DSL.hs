{-# LANGUAGE ConstraintKinds #-}
module B9.DSL where

import B9.B9Config (ExecEnvType(..))
import B9.Content (Content(..), FileSpec(..),AST(..),YamlObject(..),FileSpec,fileSpec)
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..), Mounted, MountPoint(..), FSLabel(..))
import B9.ExecEnv (CPUArch(..))
import B9.ShellScript (Script(..))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
import Control.Monad
#endif
import Control.Monad.Free (Free(..), liftF, foldFree)
import Data.Functor (void)
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Function (on)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Test.QuickCheck
import B9.QCUtil
import System.FilePath

-- ---------------------------------------------------------

data BuildStep next where
        Create ::
            (Show (CreateSpec a)) =>
            SArtifact a -> CreateSpec a -> (Handle a -> next) -> BuildStep next
        Update ::
            (Show (UpdateSpec a)) =>
            Handle a -> UpdateSpec a -> next -> BuildStep next
        Add ::
            (CanAdd env a, Show (AddSpec a)) =>
            Handle env -> SArtifact a -> AddSpec a -> next -> BuildStep next
        Export ::
            (Show (ExportSpec a), Show (ExportResult a)) =>
            Handle a -> ExportSpec a -> (ExportResult a -> next) -> BuildStep next

instance Functor BuildStep where
    fmap f (Create sa src k) = Create sa src (f . k)
    fmap f (Update hnd upd next) = Update hnd upd (f next)
    fmap f (Add hndEnv sa importSpec next) = Add hndEnv sa importSpec (f next)
    fmap f (Export hnd out k) = Export hnd out (f . k)

type Program a = Free BuildStep a

-- ---------------------------------------------------------

-- | A build step that creates something from a source that can be referenced to
-- by a handle.
create
    :: (Show (CreateSpec a))
    => SArtifact a -> CreateSpec a -> Program (Handle a)
create sa src = liftF $ Create sa src id

-- | A build step the updates an object referenced by a handle from according to
-- an update specification.
update
    :: (Show (UpdateSpec a))
    => Handle a -> UpdateSpec a -> Program ()
update hnd upd = liftF $ Update hnd upd ()

-- | A build step that adds an artifact to an environment using an 'ImportSpec'.
add
    :: (CanAdd env b, Show (AddSpec b))
    => Handle env
    -> SArtifact b
    -> AddSpec b
    -> Program ()
add hndEnv sa importSpec = liftF $ Add hndEnv sa importSpec ()

-- | A build step the exports an object referenced by a handle a to an output.
export
    :: (Show (ExportSpec a), Show (ExportResult a))
    => Handle a -> ExportSpec a -> Program (ExportResult a)
export hnd out = liftF $ Export hnd out id

-- ---------------------------------------------------------

data Artifact
    = VmImage
    | CloudInit
    | CloudInitMetaData
    | CloudInitUserData
    | Documentation
    | LinuxVm
    | TemplateVariable
    | MountedHostDir
    | MountedVmImage
    | ExecutableScript
    | FileContent
    | VariableBindings
    | LocalDirectory
    | FileSystemImage
    --    TODO refactor more and more code from e.g. 'DiskImageBuilder' into the
    --    DSL or B9IO. E.g. when exporting cloud-init compile to an /intermediate/
    --    'Program' where file content is added to an ISOImage Artifact, and later
    --    exported.
    deriving (Read,Show,Generic,Eq,Ord,Data,Typeable)


data SArtifact k where
        SVmImage :: SArtifact 'VmImage
        SCloudInit :: SArtifact 'CloudInit
        SCloudInitMetaData :: SArtifact 'CloudInitMetaData
        SCloudInitUserData :: SArtifact 'CloudInitUserData
        SDocumentation :: SArtifact 'Documentation
        SLinuxVm :: SArtifact 'LinuxVm
        STemplateVariable :: SArtifact 'TemplateVariable
        SMountedHostDir :: SArtifact 'MountedHostDir
        SMountedVmImage :: SArtifact 'MountedVmImage
        SExecutableScript :: SArtifact 'ExecutableScript
        SFileContent :: SArtifact 'FileContent
        SVariableBindings :: SArtifact 'VariableBindings
        SLocalDirectory :: SArtifact 'LocalDirectory
        SFileSystemImage :: SArtifact 'FileSystemImage

instance Show (SArtifact k) where
    show SVmImage = "SVmImage"
    show SCloudInit = "SCloudInit"
    show SCloudInitUserData = "SCloudInitUserData"
    show SCloudInitMetaData = "SCloudInitMetaData"
    show SDocumentation = "SDocumentation"
    show SLinuxVm = "SLinuxVm"
    show STemplateVariable = "STemplateVariable"
    show SMountedHostDir = "SMountedHostDir"
    show SMountedVmImage = "SMountedVmImage"
    show SExecutableScript = "SExecutableScript"
    show SFileContent = "SFileContent"
    show SVariableBindings = "SVariableBindings"
    show SLocalDirectory = "SLocalDirectory"
    show SFileSystemImage = "SFileSystemImage"

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


type family CreateSpec (a :: Artifact) :: * where
        CreateSpec 'VmImage = ImageSource
        CreateSpec 'CloudInit = String
        CreateSpec 'LinuxVm = LinuxVmArgs
        CreateSpec 'FileContent = Content
        CreateSpec 'LocalDirectory = ()
        CreateSpec 'FileSystemImage = FileSystemCreation

type family UpdateSpec (a :: Artifact) :: * where
        UpdateSpec 'VmImage = ImageResize
        UpdateSpec 'FileContent = Content

type family AddSpec (a :: Artifact) :: * where
        AddSpec 'Documentation = String
        AddSpec 'FileContent = (FileSpec, Handle 'FileContent)
        AddSpec 'ExecutableScript = Script
        AddSpec 'MountedHostDir = Mounted HostDirMnt
        AddSpec 'MountedVmImage = Mounted (Handle 'VmImage)
        AddSpec 'TemplateVariable = (String, String)
        AddSpec 'CloudInitMetaData = AST Content YamlObject
        AddSpec 'CloudInitUserData = AST Content YamlObject

type CanAdd env a = CanAddP env a ~ 'True

type family CanAddP (env :: Artifact) (a :: Artifact) :: Bool
     where
        CanAddP 'LinuxVm 'FileContent = 'True
        CanAddP 'LinuxVm 'MountedHostDir = 'True
        CanAddP 'LinuxVm 'MountedVmImage = 'True
        CanAddP 'LinuxVm 'ExecutableScript = 'True
        CanAddP 'FileContent 'FileContent = 'True
        CanAddP 'CloudInit 'FileContent = 'True
        CanAddP 'CloudInit 'ExecutableScript = 'True
        CanAddP 'CloudInit 'CloudInitMetaData = 'True
        CanAddP 'CloudInit 'CloudInitUserData = 'True
        CanAddP 'LocalDirectory 'FileContent = 'True
        CanAddP 'FileSystemImage 'FileContent = 'True
        CanAddP 'VariableBindings 'TemplateVariable = 'True
        CanAddP 'Documentation 'Documentation = 'True
        CanAddP env a = 'False

type family ExportSpec (a :: Artifact) :: * where
        ExportSpec 'VmImage = ImageDestination
        ExportSpec 'CloudInit = ()
        ExportSpec 'LocalDirectory = Maybe FilePath
        ExportSpec 'FileSystemImage = FilePath

type family ExportResult (a :: Artifact) :: * where
        ExportResult 'CloudInit =
                                (Handle 'FileContent, Handle 'FileContent)
        ExportResult 'LocalDirectory = FilePath
        ExportResult 'FileSystemImage = Image
        ExportResult a = ()

-- | Instruct an environment to mount a host directory
data HostDirMnt
    = AddMountHostDirRW FilePath
    | AddMountHostDirRO FilePath
    deriving (Read,Show,Eq,Generic,Data,Typeable)

-- | Decribe how a linux container is supposed to be started.
data LinuxVmArgs =
    LinuxVmArgs String
                ExecEnvType
                CPUArch
    deriving (Read,Show,Generic,Eq,Data,Typeable)

-- | Descibe how a 'FileSystem' should be created.
data FileSystemCreation =
    FileSystemCreation FileSystem
                       FSLabel
                       Int
                       SizeUnit
    deriving (Read,Show,Generic,Eq,Data,Typeable)

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

-- | A handle representing the environment holding all template variable
-- bindings.
variableBindings :: Handle 'VariableBindings
variableBindings = singletonHandle SVariableBindings

($=) :: String -> String -> Program ()
var $= val = add variableBindings STemplateVariable (var, val)

-- * Content generation and static file inclusion (for both cloud-init and
-- vm-images)

addFile ::(CanAdd e 'FileContent)
    => Handle e -> FileSpec -> Content -> Program ()
addFile hnd f c = createContent c >>= addContent hnd f

createContent :: Content -> Program (Handle 'FileContent)
createContent = create SFileContent

appendContent
    :: Handle 'FileContent -> Content -> Program ()
appendContent hnd c = update hnd c

addContent
    :: (CanAdd e 'FileContent)
    => Handle e -> FileSpec -> Handle 'FileContent -> Program ()
addContent hnd f c = add hnd SFileContent (f, c)

-- * cloud init

newCloudInit :: String -> Program (Handle 'CloudInit)
newCloudInit iid = create SCloudInit iid

addMetaData
    :: (CanAdd e 'CloudInitMetaData)
    => Handle e -> AST Content YamlObject -> Program ()
addMetaData hnd ast = add hnd SCloudInitMetaData ast

addUserData
    :: (CanAdd e 'CloudInitUserData)
    => Handle e -> AST Content YamlObject -> Program ()
addUserData hnd ast = add hnd SCloudInitUserData ast

writeCloudInitDir :: (Handle 'CloudInit) -> FilePath -> Program ()
writeCloudInitDir h dst = do
    (userDataH,metaDataH) <- export h ()
    dirH <- create SLocalDirectory ()
    addContent dirH (fileSpec "meta-data") metaDataH
    addContent dirH (fileSpec "user-data") userDataH
    void $ export dirH (Just dst)


writeCloudInit :: (Handle 'CloudInit) -> FileSystem -> FilePath -> Program Image
writeCloudInit h fs dst = do
    (userDataH, metaDataH) <- export h ()
    fsH <- create SFileSystemImage (FileSystemCreation fs "cidata" 2 MB)
    addContent fsH (fileSpec "meta-data") metaDataH
    addContent fsH (fileSpec "user-data") userDataH
    export fsH dst


-- * Image import

imageSource :: ImageSource -> Program (Handle 'VmImage)
imageSource src = create SVmImage src

createImage :: String
           -> FileSystem
           -> ImageType
           -> ImageSize
           -> Program (Handle 'VmImage)
createImage s fs it is = imageSource $ EmptyImage s fs it is

importImage :: FilePath
             -> ImageType
             -> FileSystem
             -> Partition
             -> ImageResize
             -> Program (Handle 'VmImage)
importImage f it fs pt is = imageSource $ SourceImage (Image f it fs) pt is

from :: String -> Program (Handle 'VmImage)
from = fromResized KeepSize

fromResized :: ImageResize -> String -> Program (Handle 'VmImage)
fromResized r s = imageSource $ From s r

resize :: Handle 'VmImage -> Int -> SizeUnit -> Program ()
resize hnd s u = update hnd (Resize (ImageSize s u))

resizeToMinimum :: Handle 'VmImage -> Program ()
resizeToMinimum hnd = update hnd ShrinkToMinimum

-- * Image export

exportImageDestination :: Handle 'VmImage
                       -> ImageDestination
                       -> Program ()
exportImageDestination hnd dst = export hnd dst

share :: Handle 'VmImage -> String -> Program ()
share hnd name = exportImageDestination hnd $ Share name QCow2 KeepSize

toLiveImg :: Handle 'VmImage
             -> String
             -> FilePath
             -> ImageResize
             -> Program ()
toLiveImg hnd imgName outDir rs =
    exportImageDestination hnd $ LiveInstallerImage imgName outDir rs

writeImg :: Handle 'VmImage
            -> FilePath
            -> ImageType
            -> FileSystem
            -> ImageResize
            -> Program ()
writeImg hnd name it fs rs =
    exportImageDestination hnd $ LocalFile (Image name it fs) rs

-- * Execution environment

boot :: String -> ExecEnvType -> CPUArch -> Program (Handle 'LinuxVm)
boot name et arch = create SLinuxVm (LinuxVmArgs name et arch)

lxc :: String -> Program (Handle 'LinuxVm)
lxc name = boot name LibVirtLXC X86_64

lxc32 :: String -> Program (Handle 'LinuxVm)
lxc32 name = boot name LibVirtLXC I386

-- * Mounting

mountDir :: Handle 'LinuxVm
         -> FilePath
         -> FilePath
         -> Program ()
mountDir e hostDir dest =
    add e SMountedHostDir (AddMountHostDirRO hostDir, MountPoint dest)

mountDirRW :: Handle 'LinuxVm
           -> FilePath
           -> FilePath
           -> Program ()
mountDirRW e hostDir dest =
    add e SMountedHostDir (AddMountHostDirRW hostDir, MountPoint dest)

mount :: Handle 'LinuxVm
      -> Handle 'VmImage
      -> FilePath
      -> Program ()
mount e imgHnd dest = add e SMountedVmImage (imgHnd, MountPoint dest)

-- * Script Execution (inside a container)

runCommand :: (CanAdd a 'ExecutableScript) => Handle a -> Script -> Program ()
runCommand hnd s = add hnd SExecutableScript s

sh :: (CanAdd a 'ExecutableScript) => Handle a -> String -> Program ()
sh e s = runCommand e (Run s [])

-- * Some utility vm builder lego

rootImage :: String -> String -> Handle 'LinuxVm -> Program ()
rootImage nameFrom nameExport env =
    void $ mountAndShareSharedImage nameFrom nameExport "/" env

dataImage :: String -> Handle 'LinuxVm -> Program ()
dataImage nameExport env =
    void $ mountAndShareNewImage "data" 64 nameExport "/data" env

mountAndShareSharedImage :: String
                         -> String
                         -> String
                         -> Handle 'LinuxVm
                         -> Program (Handle 'VmImage)
mountAndShareSharedImage nameFrom nameExport mountPoint env = do
    img <- from nameFrom
    share img nameExport
    mount env img mountPoint
    return img

mountAndShareNewImage :: String
                      -> Int
                      -> String
                      -> FilePath
                      -> Handle 'LinuxVm
                      -> Program (Handle 'VmImage)
mountAndShareNewImage fsLabel sizeGB nameExport mountPoint env = do
    img <- createImage fsLabel Ext4 QCow2 (ImageSize sizeGB GB)
    share img nameExport
    mount env img mountPoint
    return img

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
    runInterpreter (Update hnd src next) = do
        runUpdate hnd src
        return next
    runInterpreter (Add hnde sa addSpec next) = do
        runAdd  hnde sa addSpec
        return next
    runInterpreter (Export hnd out k) = do
        res <- runExport hnd out
        return (k res)

-- | Monads that interpret build steps
#if MIN_VERSION_base(4,8,0)
class (Monad f) => Interpreter f where
#else
class (Monad f, Functor f) => Interpreter f where
#endif
  runCreate
      :: (Show (CreateSpec a))
      => SArtifact a -> CreateSpec a -> f (Handle a)
  runUpdate
      :: (Show (UpdateSpec a))
      => Handle a -> UpdateSpec a -> f ()
  runAdd
      :: (Show (AddSpec a))
      => Handle env -> SArtifact a -> AddSpec a -> f ()
  runExport
      :: (Show (ExportSpec a), Show (ExportResult a))
      => Handle a -> ExportSpec a -> f (ExportResult a)

-- | An interpreter that just prints out the Program
instance Interpreter IO where
    runCreate sa src = do
        let hnd = singletonHandle sa
        printf "create %s %s from %s\n" (show sa) (show hnd) (show src)
        return hnd
    runUpdate hnd src = do
        printf "update %s using %s\n" (show hnd) (show src)
    runAdd hnde sa src = do
        let hnd = singletonHandle sa
        printf
            "add %s %s %s to %s\n"
            (show sa)
            (show hnd)
            (show src)
            (show hnde)
        return ()
    runExport hnd@(Handle SCloudInit _) dest = do
        printf "export %s to %s\n" (show hnd) (show dest)
        interpret $ do
          m <- createContent $ FromString "meta-data"
          u <- createContent $ FromString "user-data"
          return (m,u)
    runExport hnd@(Handle SLocalDirectory _) dest = do
        printf "export %s to %s\n" (show hnd) (show dest)
        return "local-directory-path"
    runExport hnd@(Handle SFileSystemImage _) dest = do
        printf "export %s to %s\n" (show hnd) (show dest)
        return (Image "local-fs-img" QCow2 Ext4)
    runExport hnd dest = do
        printf "export %s to %s\n" (show hnd) (show dest)
        return undefined


-- * QuickCheck instances
