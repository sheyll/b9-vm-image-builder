module B9.DiskImages where

import Data.Data
import Data.List ( nub )
import System.Directory ( createDirectoryIfMissing
                        , canonicalizePath
                        )
import System.FilePath ( takeDirectory
                       , takeFileName
                       , replaceExtension
                       , (</>) )

data DiskSize = DiskSize Int SizeUnit
              deriving (Eq, Show, Read, Typeable, Data)

data DiskResize = KeepSize
                | ResizeImage DiskSize
                | ResizeFS FileSystem DiskSize
                  deriving (Eq, Show, Read, Typeable, Data)

data Partition = NoPT | Partition Int
               deriving (Eq, Show, Read, Typeable, Data)

data SizeUnit = B | KB | MB | GB
              deriving (Eq, Show, Read, Ord, Typeable, Data)

isPartitioned :: Partition -> Bool
isPartitioned p | p == NoPT = False
                | otherwise = True

getPartition :: Partition -> Int
getPartition (Partition p) = p
getPartition NoPT = error "No partitions!"

newtype MountPoint = MountPoint FilePath
                   deriving (Show, Read, Typeable, Data)

type Mounted a = (a, MountPoint)

data ImageSource = FileSystem FileSystem DiskSize
                 | SourceImage Image Partition DiskResize
                 | CopyOnWrite Image
                 deriving (Show,Read,Typeable,Data)

data FileSystem = NoFileSystem | Ext4
                deriving (Eq,Show,Read,Typeable,Data)

data ImageType = Raw | QCow2 | Vmdk
               deriving (Eq,Read,Typeable,Data)

instance Show ImageType where
  show Raw = "raw"
  show QCow2 = "qcow2"
  show Vmdk = "vmdk"

data Image = Image FilePath ImageType
           deriving (Eq, Show, Read, Typeable, Data)

compatibleImageTypes :: ImageSource -> [ImageType]
compatibleImageTypes src =
  case src of
   (CopyOnWrite _) -> [QCow2]
   (FileSystem _ _) -> [Raw]
   (SourceImage _ (Partition _) _) -> [Raw]
   (SourceImage _ NoPT (ResizeFS _ _)) -> [Raw]
   (SourceImage (Image _ fmt) _ _) -> nub [fmt, Raw, QCow2, Vmdk]

ensureAbsoluteImageSourceDirExists :: ImageSource -> IO ImageSource
ensureAbsoluteImageSourceDirExists src =
  case src of
   (CopyOnWrite img) -> do
     img' <- ensureAbsoluteImageDirExists img
     return $ CopyOnWrite img'

   (SourceImage img part size) -> do
     img' <- ensureAbsoluteImageDirExists img
     return $ SourceImage img' part size

   _ -> return src

ensureAbsoluteImageDirExists :: Image -> IO Image
ensureAbsoluteImageDirExists img@(Image path _) = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  dirAbs <- canonicalizePath dir
  return $ changeImageDirectory dirAbs img

changeImageFormat :: ImageType -> Image -> Image
changeImageFormat fmt' (Image img _) = Image img' fmt'
  where img' = replaceExtension img (show fmt')

changeImageDirectory :: FilePath -> Image -> Image
changeImageDirectory dir (Image img fmt) = Image img' fmt
  where img' = dir </> takeFileName img
