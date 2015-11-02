-- | Internal module wrapping @qemu-img@
module B9.QemuImg where

import B9.B9Monad
import B9.ConfigUtils
import B9.Content
import B9.DiskImages
import Control.Lens (view)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import System.Directory
import System.FilePath
import Text.Printf (printf)


-- | Create an empty file with a given size.
createEmptyFile :: FilePath -> Int -> SizeUnit -> B9 ()
createEmptyFile f s su = do
    cmdRaw "truncate" ["--size", show s ++ formattedSizeUnit, f]
  where
    formattedSizeUnit =
        case su of
            GB -> "G"
            MB -> "M"
            KB -> "K"
            B -> ""

-- | Create a file system inside a file with a given list of file contents.
createFSWithFiles :: FilePath
                  -> FileSystemCreation
                  -> FilePath
                  -> [FileSpec]
                  -> B9 ()
createFSWithFiles dst (FileSystemCreation ISO9660 l _s _su) srcDir _fs = do
    cmdRaw "genisoimage" ["-output", dst, "-volid", l, "-rock", "-d", srcDir]
createFSWithFiles dst (FileSystemCreation VFAT l s su) srcDir fs = do
    createEmptyFile dst s su
    cmdRaw "mkfs.vfat" ["-n", l, dst]
    cmdRaw "mcopy" (("-oi" : dst : (((srcDir </>) . view fileSpecPath) <$> fs)) ++ ["::"])
createFSWithFiles dst (FileSystemCreation Ext4 l s su) _ fs = do
  when (not (null fs)) $
    fail "Creating non-empty Ext4 file systems is not yet implemented"
  createEmptyFile dst s su
  cmdRaw "mkfs.ext4" ["-L", l, "-q", dst]
createFSWithFiles dst c srcD fs =
    fail $
    printf
        "Not implemented: createFSWithFiles '%s' '%s' '%s' %s"
        dst
        (show c)
        srcD
        (show fs)

-- | Resize an image, including the file system inside the image.
resizeImage :: ImageResize -> Image -> B9 ()
resizeImage KeepSize _ = return ()
resizeImage (Resize newSize) (Image img Raw Ext4) = do
    let sizeOpt = toQemuSizeOptVal newSize
    cmd (printf "e2fsck -p '%s'" img)
    cmd (printf "resize2fs -f '%s' %s" img sizeOpt)
resizeImage (ResizeImage newSize) (Image img _ _) = do
    let sizeOpt = toQemuSizeOptVal newSize
    cmd (printf "qemu-img resize -q '%s' %s" img sizeOpt)
resizeImage ShrinkToMinimum (Image img Raw Ext4) = do
    cmd (printf "e2fsck -p '%s'" img)
    cmd (printf "resize2fs -f -M '%s'" img)
resizeImage _ img =
    error
        (printf
             "Invalid image type or filesystem, cannot resize image: %s"
             (show img))

-- | Convert vm images using @qemu-img convert@
convertImageType :: FilePath -> ImageType -> FilePath -> ImageType -> B9 ()
convertImageType fSrc tSrc fDst tDst = do
    cmdRaw
        "qemu-img"
        [ "convert"
        , "-q"
        , "-f"
        , toQemuImageType tSrc
        , "-O"
        , toQemuImageType tDst
        , fSrc
        , fDst]

-- | Return the __format__ parameter string for @qemu-img@.
toQemuImageType :: ImageType -> String
toQemuImageType Raw = "raw"
toQemuImageType QCow2 = "qcow2"
toQemuImageType Vmdk = "vmdk"

-- | Return the __size unit__ parameter string for @qemu-img@.
toQemuSizeOptVal :: ImageSize -> String
toQemuSizeOptVal (ImageSize amount u) =
    show amount ++
    case u of
        GB -> "G"
        MB -> "M"
        KB -> "K"
        B -> ""

-- * /old/ API

createCOWImage :: Image -> Image -> B9 ()
createCOWImage (Image backingFile _ _) (Image imgOut imgFmt imgFS) = do
  dbgL (printf "Creating COW image '%s' backed by '%s'" imgOut backingFile)
  cmd
    (printf
       "qemu-img create -f %s -o backing_file='%s' '%s'"
       (imageFileExtension imgFmt imgFS)
       backingFile
       imgOut)

-- | Import a disk image from some external source into the build directory
-- if necessary convert the image.
importImage :: Image -> Image -> B9 ()
importImage = convert False

-- | Export a disk image from the build directory; if necessary convert the image.
exportImage :: Image -> Image -> B9 ()
exportImage = convert False

-- | Export a disk image from the build directory; if necessary convert the image.
exportAndRemoveImage :: Image -> Image -> B9 ()
exportAndRemoveImage = convert True

-- | Convert an image in the build directory to another format and return the new image.
convertImage :: Image -> Image -> B9 ()
convertImage = convert True

-- | Return True if the first image format can be converted to the other.
canConvertTo :: (ImageType, FileSystem) -> (ImageType, FileSystem) -> Bool
canConvertTo f t =
    (canConvertImageTo `on` fst) f t && (canConvertFSTo `on` snd) f t
  where
    canConvertImageTo a b
      | a == b = True
      | a `elem` [Raw, QCow2, Vmdk] && b `elem` [Raw, QCow2, Vmdk] = True
      | otherwise = False
    canConvertFSTo NoFileSystem ISO9660 = True
    canConvertFSTo NoFileSystem VFAT = True
    canConvertFSTo a b
      | a == b = True
      | otherwise = False


-- | Convert/Copy/Move images
convert :: Bool -> Image -> Image -> B9 ()
convert doMove ii@(Image imgIn fmtIn fsIn) io@(Image imgOut fmtOut fsOut)
  | fsIn /= fsOut = do
      error
          (printf
               "FILE SYSTEM CONVERSION NOT SUPPORTED: '%s' -> '%s'"
               (show ii)
               (show io))
  | imgIn == imgOut = do
      ensureDir imgOut
      dbgL (printf "No need to convert: '%s'" imgIn)
  | doMove && fmtIn == fmtOut = do
      ensureDir imgOut
      dbgL (printf "Moving '%s' to '%s'" imgIn imgOut)
      liftIO (renameFile imgIn imgOut)
  | otherwise = do
      ensureDir imgOut
      dbgL
          (printf
               "Converting %s to %s: '%s' to '%s'"
               (imageFileExtension fmtIn fsIn)
               (imageFileExtension fmtOut fsOut)
               imgIn
               imgOut)
      cmd
          (printf
               "qemu-img convert -q -f %s -O %s '%s' '%s'"
               (imageFileExtension fmtIn fsIn)
               (imageFileExtension fmtOut fsOut)
               imgIn
               imgOut)
      when doMove $
          do dbgL (printf "Removing '%s'" imgIn)
             liftIO (removeFile imgIn)
