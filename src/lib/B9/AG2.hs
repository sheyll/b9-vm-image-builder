-- | __EXPERIMENTAL__ dont look!
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module B9.AG2 where


-- import B9.ArtifactGenerator hiding (Let)

import Control.Monad
import B9.Content.StringTemplate
import B9.DiskImages
import B9.B9Config
import B9.ExecEnv                (CPUArch(..), SharedDirectory(..))

import B9.ShellScript
-- import B9.Vm

import Control.Monad.Free

import Text.Printf

data Let =
    Let String
        String
        deriving (Show)

data IncludeFile =
    IncludeFile FilePath
                SourceFile
    deriving (Show)

data ExportCloudInit =
    ExportCloudInit FilePath
    deriving (Show)

data Mount =
    Mount ExecEnv
          ImportedImage
          MountPoint
    deriving (Show)

data ImportImg =
    ImportImg ImageSource
    deriving (Show)

data ExportImg =
    ExportImg ImportedImage
              ImageDestination
    deriving (Show)

data ImportedImage =
    ImportedImage ImageSource
                  Image
    deriving (Show)

data ExecEnv =
    ExecEnv String
            ExecEnvType
            CPUArch
    deriving (Show)

data Exec =
    Exec ExecEnv
         Script
    deriving (Show)

data BuildStep
    = LetBS
    | IncludeFileBS
    | ExportCloudInitBS
    | ImportImgBS
    | ExportImgBS
    | MountBS
    | ExecEnvBS
    | ExecBS

data SBuildStep (bs :: BuildStep) where
        SLetBS :: SBuildStep 'LetBS
        SIncludeFileBS :: SBuildStep 'IncludeFileBS
        SExportCloudInitBS :: SBuildStep 'ExportCloudInitBS
        SImportImgBS :: SBuildStep 'ImportImgBS
        SExportImgBS :: SBuildStep 'ExportImgBS
        SMountBS :: SBuildStep 'MountBS
        SExecEnvBS :: SBuildStep 'ExecEnvBS
        SExecBS :: SBuildStep 'ExecBS


type family BuildStepResult (a :: BuildStep) :: *

type instance BuildStepResult 'LetBS = ()
type instance BuildStepResult 'IncludeFileBS = ()
type instance BuildStepResult 'ExportCloudInitBS = ()
type instance BuildStepResult 'ImportImgBS = ImportedImage
type instance BuildStepResult 'ExportImgBS = ()
type instance BuildStepResult 'MountBS = ()
type instance BuildStepResult 'ExecEnvBS = ExecEnv
type instance BuildStepResult 'ExecBS = ()

type family BuildStepSyntax (bs :: BuildStep) :: *

type instance BuildStepSyntax 'LetBS = Let
type instance BuildStepSyntax 'IncludeFileBS = IncludeFile
type instance BuildStepSyntax 'ExportCloudInitBS = ExportCloudInit
type instance BuildStepSyntax 'ImportImgBS = ImportImg
type instance BuildStepSyntax 'ExportImgBS = ExportImg
type instance BuildStepSyntax 'MountBS = Mount
type instance BuildStepSyntax 'ExecEnvBS = ExecEnv
type instance BuildStepSyntax 'ExecBS = Exec

data VmBuild next :: * where
        VmBuild :: (Show (BuildStepSyntax bs)) =>
          SBuildStep bs ->
            BuildStepSyntax bs ->
              (BuildStepResult bs -> next) -> VmBuild next

instance Functor VmBuild where
    fmap f (VmBuild sbs bs g) = VmBuild sbs bs (f . g)

type VmBuildAlgebra a = Free VmBuild a


vmDo
    :: (Show (BuildStepSyntax bs)) => SBuildStep bs
    -> BuildStepSyntax bs
    -> VmBuildAlgebra (BuildStepResult bs)
vmDo sbs bs = liftF $ VmBuild sbs bs id

bind :: String -> String -> VmBuildAlgebra ()
bind var val = vmDo SLetBS (Let var val)

includeFile :: FilePath -> SourceFile -> VmBuildAlgebra ()
includeFile f src = vmDo SIncludeFileBS (IncludeFile f src)

exportCloudInit :: FilePath -> VmBuildAlgebra ()
exportCloudInit dst = vmDo SExportCloudInitBS (ExportCloudInit dst)

imageSource :: ImageSource -> VmBuildAlgebra ImportedImage
imageSource = vmDo SImportImgBS . ImportImg

createImage :: String
           -> FileSystem
           -> ImageType
           -> ImageSize
           -> VmBuildAlgebra ImportedImage
createImage s fs it is = imageSource $ EmptyImage s fs it is

importImage :: FilePath
             -> ImageType
             -> FileSystem
             -> Partition
             -> ImageResize
             -> VmBuildAlgebra ImportedImage
importImage f it fs pt is = imageSource $ SourceImage (Image f it fs) pt is

from :: String -> VmBuildAlgebra ImportedImage
from = fromResized KeepSize

fromResized :: ImageResize -> String -> VmBuildAlgebra ImportedImage
fromResized r s = imageSource $ From s r

imageDestination :: ImportedImage -> ImageDestination -> VmBuildAlgebra ()
imageDestination img dst = vmDo SExportImgBS (ExportImg img dst)

share :: ImportedImage -> String -> VmBuildAlgebra ()
share img name = imageDestination img $ Share name QCow2 KeepSize

exportLiveInstallerImage :: ImportedImage
                         -> String
                         -> FilePath
                         -> ImageResize
                         -> VmBuildAlgebra ()
exportLiveInstallerImage img imgName outDir resize =
    imageDestination img $ LiveInstallerImage imgName outDir resize

exportImage :: ImportedImage
            -> FilePath
            -> ImageType
            -> FileSystem
            -> ImageResize
            -> VmBuildAlgebra ()
exportImage img name it fs resize =
    imageDestination img $ LocalFile (Image name it fs) resize

mount :: ExecEnv -> ImportedImage -> FilePath -> VmBuildAlgebra ()
mount e src mp = vmDo SMountBS (Mount e src (MountPoint mp))

lxc :: String -> VmBuildAlgebra ExecEnv
lxc name = boot name LibVirtLXC X86_64

boot :: String -> ExecEnvType -> CPUArch -> VmBuildAlgebra ExecEnv
boot name execEnvType arch = vmDo SExecEnvBS (ExecEnv name execEnvType arch)

exec :: ExecEnv -> Script -> VmBuildAlgebra ()
exec e script = vmDo SExecBS (Exec e script)

-- run stuff

testv1 :: VmBuildAlgebra ()
testv1 = do
    bind "x" "3"
    includeFile "httpd.conf" $ Source NoConversion "httpd.conf.in"
    exportCloudInit "blah-ci"
    env <- lxc "container-id"
    rootImage env "fedora" "testv1-root"
    dataImage env "testv1-data"
    exec env (Run "ls" [])

-- * Some utility vm builder lego

rootImage env nameFrom nameExport =
    mountAndShareSharedImage env nameFrom  nameExport "/"

dataImage env nameExport =
    mountAndShareNewImage env "data" 64 nameExport "/data"

mountAndShareSharedImage env nameFrom nameExport mountPoint = do
    img <- from nameFrom
    share img nameExport
    mount env img mountPoint
    return img

mountAndShareNewImage env fsLabel sizeGB nameExport mountPoint = do
    img <- createImage fsLabel Ext4 QCow2 (ImageSize sizeGB GB)
    share img nameExport
    mount env img mountPoint
    return img

teleconfAndDbHost = installHost True (\env -> installTeleconf env >> installDb env)

installHost :: Bool -> (ExecEnv -> VmBuildAlgebra ()) -> VmBuildAlgebra ()
installHost withData service = do
  env <- lxc "h1"
  -- prod image
  rootImg <- from "fedora"
  mount env rootImg "/"
  share rootImg "app-root"

  when withData $ do
    dataImg <- createImage "data" Ext4 QCow2 (ImageSize 32 GB)
    mount env dataImg "/data"
    share dataImg "app-data"

  -- run installer(s)
  service env

class ServiceComponent a  where
    installServer :: a -> Maybe (ExecEnv -> VmBuildAlgebra ())
    createCloudInit :: a -> Maybe (VmBuildAlgebra ())

installTeleconf :: ExecEnv -> VmBuildAlgebra ()
installTeleconf env = do
    exec env (Run "tc" [])
    exec env (Run "tc" [])
    exec env (Run "tc" [])
    exec env (Run "tc" [])

installDb env = do
    exec env (Run "dbc" [])
    exec env (Run "dbc" [])
    exec env (Run "dbc" [])
    exec env (Run "dbc" [])

evalVmBuildAlgebra
    :: (Monad m)
    => (forall (bs :: BuildStep). Show (BuildStepSyntax bs)=> (SBuildStep bs) -> (BuildStepSyntax bs) -> m (BuildStepResult bs))
    -> VmBuildAlgebra a
    -> m a
evalVmBuildAlgebra f =
    foldFree
        (\(VmBuild sbs bs fnext) ->
              do res <- f sbs bs
                 return $ fnext res)


printAlg :: Show (BuildStepSyntax bs) => (SBuildStep bs) -> (BuildStepSyntax bs) -> IO (BuildStepResult bs)
printAlg sbs bs = do
  printf "%s\n" (show bs)
  return undefined
