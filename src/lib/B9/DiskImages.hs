module B9.DiskImages where

import Data.Semigroup
import Data.Data
import Data.List
import System.Directory
import System.FilePath

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
                 | CopyOnWrite Image
                 | SourceImage Image Partition DiskResize
                 | From String DiskResize
                 deriving (Show,Read,Typeable,Data)

data FileSystem = NoFileSystem | Ext4
                deriving (Eq,Show,Read,Typeable,Data)

data ImageType = Raw | QCow2 | Vmdk
               deriving (Eq,Read,Typeable,Data,Show)

-- | Return the file name extension of an image file with a specific image
-- format.
imageFileExtension :: ImageType -> String
imageFileExtension Raw = "raw"
imageFileExtension QCow2 = "qcow2"
imageFileExtension Vmdk = "vmdk"

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
   (From _ _) -> [Raw, QCow2, Vmdk]

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
  where img' = replaceExtension img (imageFileExtension fmt')

changeImageDirectory :: FilePath -> Image -> Image
changeImageDirectory dir (Image img fmt) = Image img' fmt
  where img' = dir </> takeFileName img

-- | 'SharedImage' holds all data necessary to identify an image shared.
data SharedImage = SharedImage SharedImageName
                               SharedImageDate
                               SharedImageBuildId
                               ImageType
  deriving (Eq,Read,Show)

-- | Return the name of a shared image.
siName :: SharedImage -> String
siName (SharedImage (SharedImageName n) _ _ _) = n

-- | Shared images are orderd by name, build date and build id
instance Ord SharedImage where
  compare (SharedImage n d b _) (SharedImage n' d' b' _) =
    (compare n n') <> (compare d d') <> (compare b b')

newtype SharedImageName = SharedImageName String deriving (Eq,Ord,Read,Show)
newtype SharedImageDate = SharedImageDate String deriving (Eq,Ord,Read,Show)
newtype SharedImageBuildId = SharedImageBuildId String deriving (Eq,Ord,Read,Show)

-- | Return the disk image of an sharedImage
sharedImageImage :: SharedImage -> Image
sharedImageImage (SharedImage (SharedImageName n)
                              _
                              (SharedImageBuildId bid)
                              sharedImageType) =
  Image (n ++ "_" ++ bid <.> imageFileExtension sharedImageType) sharedImageType

-- | Calculate the path to the text file holding the serialized 'SharedImage'
-- relative to the directory of shared images in a repository.
sharedImageFileName :: SharedImage -> FilePath
sharedImageFileName (SharedImage (SharedImageName n)
                                 _
                                 (SharedImageBuildId bid) _) =
  n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension  = "b9si"

--sharedImageType :: ImageType
--sharedImageType = Raw
