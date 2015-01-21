module B9.DiskImages where

import Data.Semigroup
import Data.Data
import System.FilePath


-- | Build target for disk images.
data ImageTarget = ImageTarget
                     ImageDestination
                     -- ^ where to put the created image after
                     -- the build script ran.
                     ImageSource
                     -- ^ where to get or how to create the image.
                     MountPoint
                     -- ^ Where to mount the image during the
                     -- build inside the execution
                     -- environment.
                     deriving (Read, Show, Typeable, Data)

itImageDestination :: ImageTarget -> ImageDestination
itImageDestination (ImageTarget d _ _) = d

itImageMountPoint :: ImageTarget -> MountPoint
itImageMountPoint (ImageTarget _ _ m) = m

data MountPoint = MountPoint FilePath | NotMounted
                     deriving (Show, Read, Typeable, Data, Eq)

type Mounted a = (a, MountPoint)


data ImageDestination = Share String ImageType ImageResize
                      | LocalFile Image ImageResize
                      | Transient
                      deriving (Read, Show, Typeable, Data)

data ImageSource = EmptyImage String FileSystem ImageType ImageSize
                 | CopyOnWrite Image
                 | SourceImage Image Partition ImageResize
                 | From String ImageResize
                 deriving (Show,Read,Typeable,Data)

data Partition = NoPT | Partition Int
               deriving (Eq, Show, Read, Typeable, Data)


data Image = Image FilePath ImageType FileSystem
           deriving (Eq, Show, Read, Typeable, Data)

data ImageType = Raw | QCow2 | Vmdk
               deriving (Eq,Read,Typeable,Data,Show)

data FileSystem = NoFileSystem | Ext4 | ISO9660 | VFAT
                deriving (Eq,Show,Read,Typeable,Data)


data ImageSize = ImageSize Int SizeUnit
                 deriving (Eq, Show, Read, Typeable, Data)

data SizeUnit = B | KB | MB | GB
              deriving (Eq, Show, Read, Ord, Typeable, Data)

data ImageResize = ResizeImage ImageSize
                 | Resize ImageSize
                 | ShrinkToMinimum
                 | KeepSize
                   deriving (Eq, Show, Read, Typeable, Data)

isPartitioned :: Partition -> Bool
isPartitioned p | p == NoPT = False
                | otherwise = True

getPartition :: Partition -> Int
getPartition (Partition p) = p
getPartition NoPT = error "No partitions!"


-- | Return the file name extension of an image file with a specific image
-- format.
imageFileExtension :: ImageType -> String
imageFileExtension Raw = "raw"
imageFileExtension QCow2 = "qcow2"
imageFileExtension Vmdk = "vmdk"

changeImageFormat :: ImageType -> Image -> Image
changeImageFormat fmt' (Image img _ fs) = Image img' fmt' fs
  where img' = replaceExtension img (imageFileExtension fmt')

changeImageDirectory :: FilePath -> Image -> Image
changeImageDirectory dir (Image img fmt fs) = Image img' fmt fs
  where img' = dir </> takeFileName img

-- | 'SharedImage' holds all data necessary to identify an image shared.
data SharedImage = SharedImage SharedImageName
                               SharedImageDate
                               SharedImageBuildId
                               ImageType
                               FileSystem
  deriving (Eq,Read,Show)

-- | Return the name of a shared image.
siName :: SharedImage -> String
siName (SharedImage (SharedImageName n) _ _ _ _) = n

-- | Shared images are orderd by name, build date and build id
instance Ord SharedImage where
  compare (SharedImage n d b _ _) (SharedImage n' d' b' _ _) =
    (compare n n') <> (compare d d') <> (compare b b')

newtype SharedImageName = SharedImageName String deriving (Eq,Ord,Read,Show)
newtype SharedImageDate = SharedImageDate String deriving (Eq,Ord,Read,Show)
newtype SharedImageBuildId = SharedImageBuildId String deriving (Eq,Ord,Read,Show)

-- | Return the disk image of an sharedImage
sharedImageImage :: SharedImage -> Image
sharedImageImage (SharedImage (SharedImageName n)
                              _
                              (SharedImageBuildId bid)
                              sharedImageType
                              sharedImageFileSystem) =
  Image (n ++ "_" ++ bid <.> imageFileExtension sharedImageType)
        sharedImageType
        sharedImageFileSystem

-- | Calculate the path to the text file holding the serialized 'SharedImage'
-- relative to the directory of shared images in a repository.
sharedImageFileName :: SharedImage -> FilePath
sharedImageFileName (SharedImage (SharedImageName n)
                                 _
                                 (SharedImageBuildId bid)
                                 _
                                 _) =
  n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension  = "b9si"

--sharedImageType :: ImageType
--sharedImageType = Raw
