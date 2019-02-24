-- | Experimental new, hopefully typesafe. domain specific language for
--   description of VM-builds.
{-# LANGUAGE FlexibleInstances #-}
module B9.DSL
       (B9DSL, doc, doc', (#), Documentation(..), ($=), include, includeTemplate, writeContent,
        exportCloudInit, imageSource, createImage, importImage, from,
        fromResized, imageDestination, share, exportLiveInstallerImage,
        exportImage, mount, lxc, lxc32, boot, exec, sh, rootImage,
        dataImage, mountAndShareSharedImage, mountAndShareNewImage, runDSL,
        printDSL, printBuildStep, dslExample)
       where

import B9.ArtifactGenerator (ArtifactSource(..), CloudInitType(..))
import B9.B9Config (ExecEnvType(..))
import B9.Content.Builtin(Content)
import B9.Content.StringTemplate
       (SourceFile(..), SourceFileConversion(..))
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..))
import B9.ExecEnv (CPUArch(..))
import B9.ShellScript (Script(..))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
import Control.Monad
#endif
import Control.Monad.Free (Free(..), liftF, foldFree)
import Data.Functor (void)
import Text.Printf (printf)

data BuildStep next :: * where
        Let :: String -> String -> next -> BuildStep next
        Import ::
          SArtifact a -> Source a -> (Imported a -> next) -> BuildStep next
        Export :: SArtifact a -> Target a -> next -> BuildStep next
        DefineExecEnv ::
          String ->
            ExecEnvType -> CPUArch -> (ExecEnv -> next) -> BuildStep next
        Exec :: ExecEnv -> Script -> next -> BuildStep next

instance Functor BuildStep where
    fmap f (Let k v next) = Let k v (f next)
    fmap f (Import sa src k) = Import sa src (f . k)
    fmap f (Export sa dst next) = Export sa dst (f next)
    fmap f (DefineExecEnv n et a k) = DefineExecEnv n et a (f . k)
    fmap f (Exec et s next) = Exec et s (f next)

type B9DSL a = Free BuildStep a

newtype Tagged a b = Tagged b

instance Show b => Show (Tagged a b) where
    show (Tagged s) = show s

data Documentation
    = Doc String
    | DocIncluded Content

data ExecEnv =
    ExecEnv String
            ExecEnvType
            CPUArch
    deriving (Show)

type family Source (a :: Artifact) :: * where
        Source 'StaticContent = ArtifactSource
        Source 'VmImage = ImageSource
        Source 'MountedImage = (ExecEnv, Tagged ImageSource String, FilePath)
        Source 'MountedHostDirectory = (ExecEnv, FilePath, FilePath, MountOpts String)
        Source 'SelfDocumentation = Documentation

type family Imported (a :: Artifact) :: * where
        Imported 'VmImage = Tagged ImageSource String
        Imported a = ()

type family Target (a :: Artifact) :: * where
        Target 'VmImage = (Tagged ImageSource String, ImageDestination)
        Target 'CloudInit = ([CloudInitType], FilePath)

data Artifact
    = StaticContent
    | VmImage
    | MountedImage
    | CloudInit
    | MountedHostDirectory
    | SelfDocumentation

data SArtifact (k :: Artifact) where
        SStaticContent :: SArtifact 'StaticContent
        SVmImage :: SArtifact 'VmImage
        SMountedImage :: SArtifact 'MountedImage
        SCloudInit :: SArtifact 'CloudInit
        SMountedHostDirectory :: SArtifact 'MountedHostDirectory
        SSelfDocumentation :: SArtifact 'SelfDocumentation

-- * For documentation of the actual build/deployment itself either embed a
--   string or a file, template parameters e.g. ${xxx} can be also used.

doc :: String -> B9DSL ()
doc str = liftF $ Import SSelfDocumentation (Doc str) id

doc' :: Content -> B9DSL ()
doc' c = liftF $ Import SSelfDocumentation (DocIncluded c) id

(#) :: B9DSL a -> String -> B9DSL a
m # str = do
  doc str
  m

-- * Content generation and static file inclusion

($=) :: String -> String -> B9DSL ()
var $= val = liftF $ Let var val ()

-- TODO split file inclusion from file content generation. i.e. add newFile
-- ... and then add an 'appendFile' function. new file should be typed according
-- to its contents so that only compatible content can be appended

include :: FilePath -> FilePath -> B9DSL ()
include dest src = liftF $ Import SStaticContent (FromFile dest (Source NoConversion src)) id

includeTemplate :: FilePath -> FilePath -> B9DSL ()
includeTemplate dest src = liftF $ Import SStaticContent (FromFile dest (Source ExpandVariables src)) id

writeContent :: FilePath -> Content -> B9DSL ()
writeContent dst src = liftF $ Import SStaticContent (FromContent dst src) id

-- * cloud init

exportCloudInit :: FilePath -> B9DSL ()
exportCloudInit dst = liftF $ Export SCloudInit ([CI_ISO, CI_DIR], dst) ()

-- * Image import

imageSource :: ImageSource -> B9DSL (Imported 'VmImage)
imageSource src = liftF $ Import SVmImage src id

createImage :: String
           -> FileSystem
           -> ImageType
           -> ImageSize
           -> B9DSL (Imported 'VmImage)
createImage s fs it is = imageSource $ EmptyImage s fs it is

importImage :: FilePath
             -> ImageType
             -> FileSystem
             -> Partition
             -> ImageResize
             -> B9DSL (Imported 'VmImage)
importImage f it fs pt is = imageSource $ SourceImage (Image f it fs) pt is

from :: String -> B9DSL (Imported 'VmImage)
from = fromResized KeepSize

fromResized :: ImageResize -> String -> B9DSL (Imported 'VmImage)
fromResized r s = imageSource $ From s r

-- * Image export

imageDestination :: Imported 'VmImage
                 -> ImageDestination
                 -> B9DSL ()
imageDestination img dst = liftF $ Export SVmImage (img, dst) ()

share :: Imported 'VmImage -> String -> B9DSL ()
share img name = imageDestination img $ Share name QCow2 KeepSize

exportLiveInstallerImage :: Imported 'VmImage
                         -> String
                         -> FilePath
                         -> ImageResize
                         -> B9DSL ()
exportLiveInstallerImage img imgName outDir resize =
    imageDestination img $ LiveInstallerImage imgName outDir resize

exportImage :: Imported 'VmImage
            -> FilePath
            -> ImageType
            -> FileSystem
            -> ImageResize
            -> B9DSL ()
exportImage img name it fs resize =
    imageDestination img $ LocalFile (Image name it fs) resize

-- * Mounting

class DSLCanMount a  where
    type MountArtifact a :: Artifact
    data MountOpts a
    defaultMountOpts :: a -> MountOpts a
    mountArtifactS :: a -> SArtifact (MountArtifact a)
    mountArtifact :: MountOpts a
                  -> ExecEnv
                  -> a
                  -> FilePath
                  -> Source (MountArtifact a)

-- * Host directory
instance DSLCanMount String where
  type MountArtifact String = 'MountedHostDirectory
  data MountOpts String = ReadOnly | ReadWrite deriving Show
  defaultMountOpts _ = ReadOnly
  mountArtifactS _ = SMountedHostDirectory
  mountArtifact opts e src dest = (e, src, dest, opts)

instance DSLCanMount (Tagged ImageSource String) where
  type MountArtifact (Tagged ImageSource String) = 'MountedImage
  data MountOpts (Tagged ImageSource String) = MountImgNoOptions deriving Show
  defaultMountOpts _ = MountImgNoOptions
  mountArtifactS _ = SMountedImage
  mountArtifact _opts e src dest = (e, src, dest)

mount
    :: DSLCanMount src
    => ExecEnv -> src -> FilePath -> B9DSL (Imported (MountArtifact src))
mount = mount' (defaultMountOpts undefined)

mount'
    :: DSLCanMount src
    => MountOpts src
    -> ExecEnv
    -> src
    -> FilePath
    -> B9DSL (Imported (MountArtifact src))
mount' mopts e src dest =
    liftF $
    Import
        (mountArtifactS src)
        (mountArtifact mopts e src dest)
        id

-- * Execution environment

lxc :: String -> B9DSL ExecEnv
lxc name = boot name LibVirtLXC X86_64

lxc32 :: String -> B9DSL ExecEnv
lxc32 name = boot name LibVirtLXC I386

boot :: String -> ExecEnvType -> CPUArch -> B9DSL ExecEnv
boot name et arch = liftF $ DefineExecEnv name et arch id

-- * Script Execution (inside a container)

exec :: Script -> ExecEnv -> B9DSL ()
exec script e = liftF $ Exec e script ()

sh :: String -> ExecEnv -> B9DSL ()
sh s = exec (Run s [])

-- * Some utility vm builder lego

rootImage :: String -> String -> ExecEnv -> B9DSL ()
rootImage nameFrom nameExport env =
    void $ mountAndShareSharedImage nameFrom nameExport "/" env

dataImage :: String -> ExecEnv -> B9DSL ()
dataImage nameExport env =
    void $ mountAndShareNewImage "data" 64 nameExport "/data" env

mountAndShareSharedImage :: String -> String -> String -> ExecEnv -> B9DSL (Imported 'VmImage)
mountAndShareSharedImage nameFrom nameExport mountPoint env = do
    img <- from nameFrom
    mount env img mountPoint
    share img nameExport
    return img

mountAndShareNewImage :: String -> Int -> String -> FilePath -> ExecEnv -> B9DSL (Imported 'VmImage)
mountAndShareNewImage fsLabel sizeGB nameExport mountPoint env = do
    img <- createImage fsLabel Ext4 QCow2 (ImageSize sizeGB GB)
    share img nameExport
    mount env img mountPoint
    return img

-- * DSL Interpreter

#if MIN_VERSION_base(4,8,0)
runDSL
    :: Monad m
    => (forall a. BuildStep a -> m a) -> B9DSL b -> m b
#else
runDSL
    :: (Monad m, Functor m)
    => (forall a. BuildStep a -> m a) -> B9DSL b -> m b
#endif
runDSL = foldFree

-- | Print the DSL to IO
printDSL :: B9DSL a -> IO ()
printDSL = void . runDSL printBuildStep

printBuildStep :: BuildStep a -> IO a
printBuildStep (Let k v next) = do
    printf "%s := %s\n" k v
    return next
printBuildStep (Import SStaticContent src k) = do
    printf "import static %s\n" (show src)
    return $ k ()
printBuildStep (Import SVmImage src k) = do
    printf "import image %s\n" (show src)
    return (k (Tagged (show src)))
printBuildStep (Import SMountedImage src k) = do
    printf "mount image %s\n" (show src)
    return (k ())
printBuildStep (Import SMountedHostDirectory src k) = do
    printf "mount host directory %s\n" (show src)
    return (k ())
printBuildStep (Import SSelfDocumentation (Doc str) k) = do
    printf "-- %s\n" str
    return (k ())
printBuildStep (Import SSelfDocumentation (DocIncluded c) k) = do
    printf "-- %s\n" (show c)
    return (k ())
printBuildStep (Export SVmImage dst next) = do
    printf "export image %s\n" (show dst)
    return next
printBuildStep (Export SCloudInit dst next) = do
    printf "export cloud-init %s\n" (show dst)
    return next
printBuildStep (DefineExecEnv n et a k) = do
    printf "define env: %s %s %s\n" n (show et) (show a)
    return (k (ExecEnv n et a))
printBuildStep (Exec (ExecEnv n _ _) s next) = do
    printf "exec in %s: %s\n" n (show s)
    return next
printBuildStep _other = do
    printf "???\n"
    return undefined

-- * Tests and experiments

dslExample :: B9DSL ()
dslExample = do
    "x" $= "3"
    includeTemplate "httpd.conf" "httpd.conf.in" # "overwrite all of httpd!"
    exportCloudInit "blah-ci"                    # "export the cloud-init stuff"
    e <- lxc "container-id"
    doc "From here there be dragons:"
    mount e "/tmp" "/mnt/HOST_TMP"
    rootImage "fedora" "testv1-root" e
    dataImage "testv1-data" e
    sh "ls -la" e
