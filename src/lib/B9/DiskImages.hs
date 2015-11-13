{-| Data types that describe all B9 relevant elements of virtual machine disk
images.-}
module B9.DiskImages where

import B9.FileSystems
import B9.CommonTypes
import B9.QCUtil
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
import System.FilePath
import Test.QuickCheck

-- * Data types for disk image description, e.g. 'ImageTarget',
-- 'ImageDestination', 'Image', 'MountPoint', 'SharedImage'

-- | A vm disk image file consisting of a path to the image file, and the type
-- and file system.
data Image =
    Image FilePath
          ImageType
          FileSystem
    deriving (Eq,Show,Read,Typeable,Data,Generic)

instance Hashable Image
instance Binary Image
instance NFData Image

-- | An image type defines the actual /file format/ of a file containing file
-- systems. These are like /virtual harddrives/
data ImageType
    = Raw
    | QCow2
    | Vmdk
    deriving (Eq,Read,Typeable,Data,Show,Generic)

instance Hashable ImageType
instance Binary ImageType
instance NFData ImageType
instance CoArbitrary ImageType

-- | A data type for image file or file system size; instead of passing 'Int's
-- around this also captures a size unit so that the 'Int' can be kept small
data ImageSize =
    ImageSize Int
              SizeUnit
    deriving (Eq,Show,Read,Typeable,Data,Generic)

instance Hashable ImageSize
instance Binary ImageSize
instance NFData ImageSize

-- * Constroctor and accessors for 'Image' 'ImageTarget' 'ImageSource'
-- 'ImageDestination' and 'SharedImage'

-- | Return the name of the file corresponding to an 'Image'
imageFileName :: Image -> FilePath
imageFileName (Image f _ _) = f

-- | Return the 'ImageType' of an 'Image'
imageImageType :: Image -> ImageType
imageImageType (Image _ t _) = t

-- | Return the 'FileSystem' of an 'Image'
imageFileSystem :: Image -> FileSystem
imageFileSystem (Image _ _ s) = s

-- | Return the 'FileSystem' and 'ImageType' of an 'Image'
imageFormat :: Image -> (ImageType, FileSystem)
imageFormat i = (imageImageType i, imageFileSystem i)

-- | Return the file name extension of an image file with a specific image
-- format.
imageFileExtension :: ImageType -> FileSystem -> String
imageFileExtension Raw ISO9660 = "iso"
imageFileExtension Raw VFAT = "vfat"
imageFileExtension Raw _ = "raw"
imageFileExtension QCow2 _ = "qcow2"
imageFileExtension Vmdk _ = "vmdk"

-- | Change the image file format and also rename the image file name to
-- have the appropriate file name extension. See 'imageFileExtension' and
-- 'replaceExtension'
changeImageFormat :: ImageType -> Image -> Image
changeImageFormat fmt' (Image img _ fs) = Image img' fmt' fs
  where
    img' = replaceExtension img (imageFileExtension fmt' fs)

-- | Change the file system type and rename the image file name to
-- have the appropriate file name extension. See 'imageFileExtension' and
-- 'replaceExtension'
changeImageFileSystem :: FileSystem -> Image -> Image
changeImageFileSystem fs' (Image img fmt _) = Image img' fmt fs'
  where
    img' = replaceExtension img (imageFileExtension fmt fs')

changeImageDirectory :: FilePath -> Image -> Image
changeImageDirectory dir (Image img fmt fs) = Image img' fmt fs
  where img' = dir </> takeFileName img

-- * QuickCheck instances

instance Arbitrary Image where
    arbitrary =
        Image "img-file-name" <$> smaller arbitrary <*> smaller arbitrary

instance Arbitrary ImageType where
    arbitrary = elements [Raw, QCow2, Vmdk]

instance Arbitrary ImageSize where
    arbitrary = ImageSize <$> smaller arbitrary <*> smaller arbitrary
