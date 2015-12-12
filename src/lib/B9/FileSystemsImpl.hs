module B9.FileSystemsImpl where

import           B9.B9Monad
import           B9.CommonTypes
import           B9.Content
import           B9.DiskImages
import           B9.FileSystems
import           Control.Lens    hiding (elements, (<.>))
import           Control.Monad
import           System.FilePath
import           Text.Printf

-- | Resize an image, including the file system inside the image.
resizeFS :: FileSystemResize -> FilePath -> FileSystem -> B9 ()
resizeFS (FileSystemResize newSize u) img Ext4 = do
    let sizeOpt = toExt4SizeOptVal (ImageSize newSize u)
    cmdRaw "e2fsck" ["-p", img]
    cmdRaw "resize2fs" ["-f", img, sizeOpt]
resizeFS ShrinkFileSystem img Ext4 = do
    cmdRaw "e2fsck" ["-p", img]
    cmdRaw "resize2fs" ["-f", "-M", img]
resizeFS _ img fsT =
    error
        (printf
             "Invalid filesystem, cannot resize image: %s of type %s"
             (show img)
             (show fsT))

-- | Return the __size unit__ parameter string for @resize2fs@.
toExt4SizeOptVal :: ImageSize -> String
toExt4SizeOptVal (ImageSize amount u) =
    show amount ++
    case u of
        GB -> "G"
        MB -> "M"
        KB -> "K"
        B -> ""

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
                  -> FileSystemSpec
                  -> FilePath
                  -> [FileSpec]
                  -> B9 ()
createFSWithFiles dst (FileSystemSpec ISO9660 l _s _su) srcDir _fs = do
    cmdRaw "genisoimage" ["-output", dst, "-volid", l, "-rock", "-d", srcDir]
createFSWithFiles dst (FileSystemSpec VFAT l s su) srcDir fs = do
    createEmptyFile dst s su
    cmdRaw "mkfs.vfat" ["-n", l, dst]
    cmdRaw
        "mcopy"
        (("-oi" : dst : (((srcDir </>) . view fileSpecPath) <$> fs)) ++ ["::"])
createFSWithFiles dst (FileSystemSpec Ext4 l s su) _ fs = do
    when (not (null fs)) $
        fail "Creating non-empty Ext4 file systems is not yet implemented"
    createEmptyFile dst s su
    cmdRaw "mkfs.ext4" ["-F", "-L", l, "-q", dst]
createFSWithFiles dst c srcD fs =
    fail $
    printf
        "Not implemented: createFSWithFiles '%s' '%s' '%s' %s"
        dst
        (show c)
        srcD
        (show fs)
