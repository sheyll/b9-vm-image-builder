{-| Data types that describe all B9 relevant elements of virtual machine disk
images.-}
module B9.DiskImages where

import           B9.QCUtil
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import           Data.Data
import           Data.Maybe
import           Data.Semigroup
import           System.FilePath
import           Test.QuickCheck
import qualified Text.PrettyPrint.Boxes as Boxes
import           Text.Printf

-- * Data types for disk image description, e.g. 'ImageTarget',
-- 'ImageDestination', 'Image', 'MountPoint', 'SharedImage'

-- | Build target for disk images; the destination, format and size of the image
-- to generate, as well as how to create or obtain the image before a
-- 'B9.Vm.VmScript' is executed with the image mounted at a 'MountPoint'.
data ImageTarget = ImageTarget
                     ImageDestination
                     ImageSource
                     MountPoint
                     deriving (Read, Show, Typeable, Data, Eq)

-- | A mount point or 'NotMounted'
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

-- | An image type defines the actual /file format/ of a file containing file
-- systems. These are like /virtual harddrives/
data ImageType = Raw | QCow2 | Vmdk
               deriving (Eq,Read,Typeable,Data,Show)

-- | The file systems that b9 can use and convert.
data FileSystem = NoFileSystem | Ext4 | ISO9660 | VFAT
                deriving (Eq,Show,Read,Typeable,Data)

-- | A data type for image file or file system size; instead of passing 'Int's
-- around this also captures a size unit so that the 'Int' can be kept small
data ImageSize = ImageSize Int SizeUnit
                 deriving (Eq, Show, Read, Typeable, Data)

-- | Enumeration of size multipliers. The exact semantics may vary depending on
-- what external tools look at these. E.g. the size unit is convert to a size
-- parameter of the @qemu-img@ command line tool.
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

-- | A type alias that indicates that something of type @a@ is mount at a
-- 'MountPoint'
type Mounted a = (a, MountPoint)

-- * Shared Images

-- | 'SharedImage' holds all data necessary to describe an __instance__ of a shared
--    image identified by a 'SharedImageName'. Shared images are stored in
--    'B9.Repository's.
data SharedImage =
    SharedImage SharedImageName
                SharedImageDate
                SharedImageBuildId
                ImageType
                FileSystem
    deriving (Eq,Read,Show)

-- | The name of the image is the de-facto identifier for push, pull, 'From' and
--   'Share'.  B9 always selects the newest version the shared image identified
--   by that name when using a shared image as an 'ImageSource'. This is a
--   wrapper around a string that identifies a 'SharedImage'
newtype SharedImageName = SharedImageName String deriving (Eq,Ord,Read,Show)

-- | The exact time that build job __started__.
--   This is a wrapper around a string contains the build date of a
--   'SharedImage'; this is purely additional convenience and typesafety
newtype SharedImageDate = SharedImageDate String deriving (Eq,Ord,Read,Show)

-- | Every B9 build running in a 'B9Monad'
--   contains a random unique id that is generated once per build (no matter how
--   many artifacts are created in that build) This field contains the build id
--   of the build that created the shared image instance.  This is A wrapper
--   around a string contains the build id of a 'SharedImage'; this is purely
--   additional convenience and typesafety
newtype SharedImageBuildId = SharedImageBuildId String deriving (Eq,Ord,Read,Show)

-- | Shared images are orderd by name, build date and build id
instance Ord SharedImage where
  compare (SharedImage n d b _ _) (SharedImage n' d' b' _ _) =
    compare n n' <> compare d d' <> compare b b'

-- * Constroctor and accessors for 'Image' 'ImageTarget' 'ImageSource'
-- 'ImageDestination' and 'SharedImage'

-- | Return the name of the file corresponding to an 'Image'
imageFileName :: Image -> FilePath
imageFileName (Image f _ _) = f

-- | Return the 'ImageType' of an 'Image'
imageImageType :: Image -> ImageType
imageImageType (Image _ t _) = t

-- | Return the files generated for a 'LocalFile' or a 'LiveInstallerImage'; 'SharedImage' and 'Transient'
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

-- | Return the name of a shared image, if the 'ImageDestination' is a 'Share'
--   destination
imageDestinationSharedImageName :: ImageDestination -> Maybe SharedImageName
imageDestinationSharedImageName (Share n _ _) = Just (SharedImageName n)
imageDestinationSharedImageName _ = Nothing

-- | Return the name of a shared source image, if the 'ImageSource' is a 'From'
--   source
imageSourceSharedImageName :: ImageSource -> Maybe SharedImageName
imageSourceSharedImageName (From n _) = Just (SharedImageName n)
imageSourceSharedImageName _ = Nothing

-- | Get the 'ImageDestination' of an 'ImageTarget'
itImageDestination :: ImageTarget -> ImageDestination
itImageDestination (ImageTarget d _ _) = d

-- | Get the 'ImageSource' of an 'ImageTarget'
itImageSource :: ImageTarget -> ImageSource
itImageSource (ImageTarget _ s _) = s

-- | Get the 'MountPoint' of an 'ImageTarget'
itImageMountPoint :: ImageTarget -> MountPoint
itImageMountPoint (ImageTarget _ _ m) = m


-- | Return true if a 'Partition' parameter is actually refering to a partition,
-- false if it is 'NoPT'
isPartitioned :: Partition -> Bool
isPartitioned p
  | p == NoPT = False
  | otherwise = True

-- | Return the 'Partition' index or throw a runtime error if aplied to 'NoPT'
getPartition :: Partition -> Int
getPartition (Partition p) = p
getPartition NoPT = error "No partitions!"

-- | Return the file name extension of an image file with a specific image
-- format.
imageFileExtension :: ImageType -> String
imageFileExtension Raw = "raw"
imageFileExtension QCow2 = "qcow2"
imageFileExtension Vmdk = "vmdk"

-- | Change the image file format and also rename the image file name to
-- have the appropriate file name extension. See 'imageFileExtension' and
-- 'replaceExtension'
changeImageFormat :: ImageType -> Image -> Image
changeImageFormat fmt' (Image img _ fs) = Image img' fmt' fs
  where img' = replaceExtension img (imageFileExtension fmt')

changeImageDirectory :: FilePath -> Image -> Image
changeImageDirectory dir (Image img fmt fs) = Image img' fmt fs
  where img' = dir </> takeFileName img

-- * Constructors and accessors for 'ImageSource's
getImageSourceImageType :: ImageSource -> Maybe ImageType
getImageSourceImageType (EmptyImage _ _ t _) = Just t
getImageSourceImageType (CopyOnWrite i) = Just $ imageImageType i
getImageSourceImageType (SourceImage i _ _) = Just $ imageImageType i
getImageSourceImageType (From _ _) = Nothing

-- * Constructors and accessors for 'SharedImage's

-- | Return the name of a shared image.
siName :: SharedImage -> SharedImageName
siName (SharedImage n _ _ _ _) = n

-- | Return the date of a shared image.
siDate :: SharedImage -> SharedImageDate
siDate (SharedImage _ n _ _ _) = n

-- | Return the build id of a shared image.
siBuildId :: SharedImage -> SharedImageBuildId
siBuildId (SharedImage _ _ n _ _) = n

-- | Print the contents of the shared image in one line
prettyPrintSharedImages :: [SharedImage] -> String
prettyPrintSharedImages imgs = Boxes.render table
  where
    table = Boxes.hsep 1 Boxes.left cols
      where
        cols = [nameC, dateC, idC]
          where
            nameC = col "Name" ((\(SharedImageName n) -> n) . siName)
            dateC = col "Date" ((\(SharedImageDate n) -> n) . siDate)
            idC = col "ID" ((\(SharedImageBuildId n) -> n) . siBuildId)
            col title accessor =
              (Boxes.text title) Boxes.// (Boxes.vcat Boxes.left cells)
              where
                cells = Boxes.text <$> accessor <$> imgs

-- | Return the disk image of an sharedImage
sharedImageImage :: SharedImage -> Image
sharedImageImage (SharedImage (SharedImageName n) _ (SharedImageBuildId bid) sharedImageType sharedImageFileSystem) =
    Image
        (n ++ "_" ++ bid <.> imageFileExtension sharedImageType)
        sharedImageType
        sharedImageFileSystem

-- | Calculate the path to the text file holding the serialized 'SharedImage'
-- relative to the directory of shared images in a repository.
sharedImageFileName :: SharedImage -> FilePath
sharedImageFileName (SharedImage (SharedImageName n) _ (SharedImageBuildId bid) _ _) =
    n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension  = "b9si"

-- | The internal image type to use as best guess when dealing with a 'From'
-- value.
sharedImageDefaultImageType :: ImageType
sharedImageDefaultImageType = QCow2

-- * Constructors for 'ImageTarget's

-- | Use a 'QCow2' image with an 'Ext4' file system
transientCOWImage :: FilePath -> FilePath -> ImageTarget
transientCOWImage fileName mountPoint =
    ImageTarget
        Transient
        (CopyOnWrite (Image fileName QCow2 Ext4))
        (MountPoint mountPoint)

-- | Use a shared image
transientSharedImage :: SharedImageName -> FilePath -> ImageTarget
transientSharedImage (SharedImageName name) mountPoint =
    ImageTarget Transient (From name KeepSize) (MountPoint mountPoint)

-- | Use a shared image
transientLocalImage :: FilePath -> FilePath -> ImageTarget
transientLocalImage name mountPoint =
    ImageTarget Transient (From name KeepSize) (MountPoint mountPoint)

-- | Share a 'QCow2' image with 'Ext4' fs
shareCOWImage :: FilePath -> SharedImageName -> FilePath -> ImageTarget
shareCOWImage srcFilename (SharedImageName destName) mountPoint =
    ImageTarget
        (Share destName QCow2 KeepSize)
        (CopyOnWrite (Image srcFilename QCow2 Ext4))
        (MountPoint mountPoint)

-- | Share an image based on a shared image
shareSharedImage :: SharedImageName
                 -> SharedImageName
                 -> FilePath
                 -> ImageTarget
shareSharedImage (SharedImageName srcName) (SharedImageName destName) mountPoint =
    ImageTarget
        (Share destName QCow2 KeepSize)
        (From srcName KeepSize)
        (MountPoint mountPoint)

-- | Share a 'QCow2' image with 'Ext4' fs
shareLocalImage :: FilePath -> SharedImageName -> FilePath -> ImageTarget
shareLocalImage srcName (SharedImageName destName) mountPoint =
    ImageTarget
        (Share destName QCow2 KeepSize)
        (SourceImage (Image srcName QCow2 Ext4) NoPT KeepSize)
        (MountPoint mountPoint)

-- | Export a 'QCow2' image with 'Ext4' fs
cowToliveInstallerImage :: String
                        -> FilePath
                        -> FilePath
                        -> FilePath
                        -> ImageTarget
cowToliveInstallerImage srcName destName outDir mountPoint =
    ImageTarget
        (LiveInstallerImage destName outDir KeepSize)
        (CopyOnWrite (Image srcName QCow2 Ext4))
        (MountPoint mountPoint)

-- | Export a 'QCow2' image file with 'Ext4' fs as
--   a local file
cowToLocalImage :: FilePath -> FilePath -> FilePath -> ImageTarget
cowToLocalImage srcName destName mountPoint =
    ImageTarget
        (LocalFile (Image destName QCow2 Ext4) KeepSize)
        (CopyOnWrite (Image srcName QCow2 Ext4))
        (MountPoint mountPoint)

-- | Export a 'QCow2' image file with 'Ext4' fs as
--   a local file
localToLocalImage :: FilePath -> FilePath -> FilePath -> ImageTarget
localToLocalImage srcName destName mountPoint =
    ImageTarget
        (LocalFile (Image destName QCow2 Ext4) KeepSize)
        (SourceImage (Image srcName QCow2 Ext4) NoPT KeepSize)
        (MountPoint mountPoint)

-- | Create a local image file from the contents of the first partition
--   of a local 'QCow2' image.
partition1ToLocalImage :: FilePath -> FilePath -> FilePath -> ImageTarget
partition1ToLocalImage srcName destName mountPoint =
    ImageTarget
        (LocalFile (Image destName QCow2 Ext4) KeepSize)
        (SourceImage (Image srcName QCow2 Ext4) NoPT KeepSize)
        (MountPoint mountPoint)

-- * 'ImageTarget' Transformations

-- | Split any image target into two image targets, one for creating an intermediate shared image and one
-- from the intermediate shared image to the output image.
splitToIntermediateSharedImage :: ImageTarget
                               -> SharedImageName
                               -> (ImageTarget, ImageTarget)
splitToIntermediateSharedImage (ImageTarget dst src mnt) (SharedImageName intermediateName) =
    (imgTargetShared, imgTargetExport)
  where
    imgTargetShared = ImageTarget intermediateTo src mnt
    imgTargetExport = ImageTarget dst intermediateFrom mnt
    intermediateTo =
        Share
            intermediateName
            (fromMaybe
                 sharedImageDefaultImageType
                 (getImageSourceImageType src))
            KeepSize
    intermediateFrom = From intermediateName KeepSize

-- * 'Arbitrary' instances for quickcheck

instance Arbitrary ImageTarget where
    arbitrary =
        ImageTarget <$> smaller arbitrary <*> smaller arbitrary <*>
        smaller arbitrary

instance Arbitrary ImageSource where
    arbitrary =
        oneof
            [ EmptyImage "img-label" <$> smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary
            , CopyOnWrite <$> smaller arbitrary
            , SourceImage <$> smaller arbitrary <*> smaller arbitrary <*>
              smaller arbitrary
            , From <$> arbitrarySharedImageName <*> smaller arbitrary]

instance Arbitrary ImageDestination where
    arbitrary =
        oneof
            [ Share <$> arbitrarySharedImageName <*> smaller arbitrary <*>
              smaller arbitrary
            , LiveInstallerImage "live-installer" "output-path" <$>
              smaller arbitrary
            , pure Transient]

instance Arbitrary MountPoint where
    arbitrary = elements [MountPoint "/mnt", NotMounted]

instance Arbitrary ImageResize where
    arbitrary =
        oneof
            [ ResizeImage <$> smaller arbitrary
            , Resize <$> smaller arbitrary
            , pure ShrinkToMinimum
            , pure KeepSize]

instance Arbitrary Partition where
    arbitrary = oneof [Partition <$> elements [0, 1, 2], pure NoPT]

instance Arbitrary Image where
    arbitrary =
        Image "img-file-name" <$> smaller arbitrary <*> smaller arbitrary

instance Arbitrary FileSystem where
    arbitrary = elements [Ext4]

instance Arbitrary ImageType where
    arbitrary = elements [Raw, QCow2, Vmdk]

instance Arbitrary ImageSize where
    arbitrary = ImageSize <$> smaller arbitrary <*> smaller arbitrary

instance Arbitrary SizeUnit where
    arbitrary = elements [B, KB, MB, GB]

instance Arbitrary SharedImageName where
    arbitrary = SharedImageName <$> arbitrarySharedImageName

arbitrarySharedImageName :: Gen String
arbitrarySharedImageName =
    elements [printf "arbitrary-shared-img-name-%d" x | x <- [0 :: Int .. 3]]
