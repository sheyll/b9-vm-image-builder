module B9.DSL(B9DSL) where

import B9.ArtifactGenerator (ArtifactSource(..), CloudInitType(..))
import B9.B9Config (ExecEnvType(..))
import B9.Content.StringTemplate
       (SourceFile(..), SourceFileConversion(..))
import B9.DiskImages
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..))
import B9.ExecEnv (CPUArch(..), SharedDirectory(..))
import B9.ShellScript (Script(..))
import Control.Monad (when)
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

data Tagged a =
    Tagged String

instance Show (Tagged a) where
    show (Tagged s) = s

data ExecEnv =
    ExecEnv String
            ExecEnvType
            CPUArch
    deriving (Show)

type family Source (a :: Artifact) :: * where
        Source 'StaticContent = ArtifactSource
        Source 'VmImage = ImageSource
        Source 'MountedImage = (ExecEnv, Tagged ImageSource, FilePath)
        Source 'MountedHostDirectory = (ExecEnv, FilePath, FilePath)

type family Imported (a :: Artifact) :: * where
        Imported 'VmImage = (Tagged ImageSource)
        Imported a = ()

type family Target (a :: Artifact) :: * where
        Target 'VmImage = (Tagged ImageSource, ImageDestination)
        Target 'CloudInit = ([CloudInitType], FilePath)

data Artifact
    = StaticContent
    | VmImage
    | MountedImage
    | CloudInit
    | MountedHostDirectory

data SArtifact (k :: Artifact) where
        SStaticContent :: SArtifact 'StaticContent
        SVmImage :: SArtifact 'VmImage
        SMountedImage :: SArtifact 'MountedImage
        SCloudInit :: SArtifact 'CloudInit
        SMountedHostDirectory :: SArtifact 'MountedHostDirectory

-- * Content generation and static file inclusion

bind :: String -> String -> B9DSL ()
bind var val = liftF $ Let var val ()

includeFile :: FilePath -> SourceFile -> B9DSL ()
includeFile dest src = liftF $ Import SStaticContent (FromFile dest src) id

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

mount :: Imported 'VmImage -> FilePath -> ExecEnv -> B9DSL ()
mount src mp e = liftF $ Import SMountedImage (e, src, mp) id

-- * Execution environment

lxc :: String -> B9DSL ExecEnv
lxc name = boot name LibVirtLXC X86_64

lxc32 :: String -> B9DSL ExecEnv
lxc32 name = boot name LibVirtLXC I386

boot :: String -> ExecEnvType -> CPUArch -> B9DSL ExecEnv
boot name et arch = liftF $ DefineExecEnv name et arch id

-- * Script Execution

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
    share img nameExport
    mount img mountPoint env
    return img

mountAndShareNewImage :: String -> Int -> String -> FilePath -> ExecEnv -> B9DSL (Imported 'VmImage)
mountAndShareNewImage fsLabel sizeGB nameExport mountPoint env = do
    img <- createImage fsLabel Ext4 QCow2 (ImageSize sizeGB GB)
    share img nameExport
    mount img mountPoint env
    return img

runDSL
    :: Monad m
    => (forall a. BuildStep a -> m a) -> B9DSL b -> m b
runDSL = foldFree

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

installHost :: Bool -> (ExecEnv -> B9DSL ()) -> B9DSL ()
installHost withData service = do
    e <- lxc "h1"
    -- prod image
    rootImage "fedora" "app" e
    when withData $ dataImage "app-data" e
    -- run installer(s)
    service e

teleconfAndDbHost :: B9DSL ()
teleconfAndDbHost =
    installHost
        True
        (\e ->
              installTeleconf e >> installDb e)

installTeleconf :: ExecEnv -> B9DSL ()
installTeleconf e = do
    sh "tc" e
    sh "tc" e
    sh "tc" e
    sh "tc" e
    sh "tc" e

installDb :: ExecEnv -> B9DSL ()
installDb e = do
    exec (Run "dbc" []) e
    exec (Run "dbc" []) e
    exec (Run "dbc" []) e
    exec (Run "dbc" []) e

testv1 :: B9DSL ()
testv1 = do
    bind "x" "3"
    includeFile "httpd.conf" $ Source NoConversion "httpd.conf.in"
    exportCloudInit "blah-ci"
    e <- lxc "container-id"
    rootImage "fedora" "testv1-root" e
    dataImage "testv1-data" e
    sh "ls -la" e
