{-| Data types that describe all B9 relevant elements of virtual machine disk
images.-}
module B9.DiskImages where

import Data.Semigroup
import Data.Data
import System.FilePath

-- | Build target for disk images; the destination, format and size of the image
-- to generate, as well as how to create or obtain the image before a
-- 'B9.Vm.VmScript' is executed with the image mounted at a 'MountPoint'.
data ImageTarget = ImageTarget
                     ImageDestination
                     ImageSource
                     MountPoint
                     deriving (Read, Show, Typeable, Data, Eq)

-- | A mount point or `NotMounted`
data MountPoint = MountPoint FilePath | NotMounted
                     deriving (Show, Read, Typeable, Data, Eq)

-- | The destination of an image.
data ImageDestination = Share String ImageType ImageResize
                      -- ^ Create the image and some meta data so that other
                      -- builds can use them as 'ImageSource's via 'From'.
                      | LiveInstallerImage String FilePath ImageResize
                      -- ^ __DEPRECATED__ Export a raw image that can directly
                      -- be booted.
                      | LocalFile Image ImageResize
                      -- ^ Write an image file to the path in the first
                      -- argument., possible resizing it,
                      | Transient
                      -- ^ Do not export the image. Usefule if the main
                      -- objective of the b9 build is not an image file, but
                      -- rather some artifact produced by executing by a
                      -- containerize build.
                      deriving (Read, Show, Typeable, Data,Eq)

-- | Specification of how the image to build is obtained.
data ImageSource = EmptyImage String FileSystem ImageType ImageSize
                  -- ^ Create an empty image file having a file system label
                  -- (first parameter), a file system type (e.g. 'Ext4') and an
                  -- 'ImageSize'
                 | CopyOnWrite Image
                  -- ^ __DEPRECATED__
                 | SourceImage Image Partition ImageResize
                  -- ^ Clone an existing image file; if the image file contains
                  -- partitions, select the partition to use, b9 will extract
                  -- that partition by reading the offset of the partition from
                  -- the partition table and extract it using @dd@.
                 | From String ImageResize
                  -- ^ Use an image previously shared by via 'Share'.
                 deriving (Show,Read,Typeable,Data,Eq)

-- | The partition to extract.
data Partition = NoPT -- ^ There is no partition table on the image
               | Partition Int -- ^ Extract partition @n@ @n@ must be in @0..3@
  deriving (Eq, Show, Read, Typeable, Data)

-- | A vm disk image file consisting of a path to the image file, and the type
-- and file system.
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

-- | How to resize an image file.
data ImageResize = ResizeImage ImageSize
                   -- ^ Resize the image __but not the file system__. Note that
                   -- a file system contained in the image file might be
                   -- corrupted by this operation. To not only resize the image
                   -- file but also the fil system contained in it, use
                   -- 'Resize'.
                 | Resize ImageSize
                   -- ^ Resize an image and the contained file system.
                 | ShrinkToMinimum
                   -- ^ Resize an image and the contained file system to the
                   -- smallest size to fit the contents of the file system.
                 | KeepSize
                   -- ^ Do not change the image size.
                   deriving (Eq, Show, Read, Typeable, Data)

type Mounted a = (a, MountPoint)

-- | Return the files generated for a local or a live image; shared and transient images
-- are treated like they have no ouput files because the output files are manged
-- by B9.
getImageDestinationOutputFiles :: ImageTarget -> [FilePath]
getImageDestinationOutputFiles (ImageTarget d _ _) =
  case d of
   LiveInstallerImage liName liPath _ ->
     let path = liPath </> "machines" </> liName </> "disks" </> "raw"
     in [path </> "0.raw", path </> "0.size", path </> "VERSION"]
   LocalFile (Image lfPath _ _) _ -> [lfPath]
   _ -> []

itImageDestination :: ImageTarget -> ImageDestination
itImageDestination (ImageTarget d _ _) = d

itImageMountPoint :: ImageTarget -> MountPoint
itImageMountPoint (ImageTarget _ _ m) = m
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

-- | 'SharedImage' holds all data necessary to identify an image shared. Shared
-- images are stored in 'B9.Repository's.
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
    compare n n' <> compare d d' <> compare b b'

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
