module B9.DiskImageBuilder (createImage
                           ,resizeImage
                           ,importImage
                           ,exportImage
                           ,exportAndRemoveImage
                           ,convertImage) where

import Control.Monad ( when )
import Control.Monad.IO.Class
import System.Directory ( createDirectoryIfMissing
                        , canonicalizePath
                        , renameFile
                        , removeFile
                        )
import System.FilePath ( takeDirectory
                       , takeFileName
                       , replaceExtension
                       , (<.>)
                       , (</>) )
import Text.Printf ( printf )
import B9.B9Monad
import B9.DiskImages
import B9.ConfigUtils
import qualified B9.PartitionTable as PartitionTable

createImage :: ImageSource -> Image -> B9 ()
createImage src = case src of
                   (FileSystem fsType size) ->
                     createFS fsType size
                   (SourceImage srcImg part size) ->
                     createImageFromImage srcImg part size
                   (CopyOnWrite backingImg) ->
                     createCOWImage backingImg

createImageFromImage :: Image -> Partition -> DiskResize -> Image -> B9 ()
createImageFromImage src part size out = do
  let tmp = if isPartitioned part
            then changeImageFormat Raw out
            else out
  importImage src tmp
  extractPartition part tmp
  resizeImage size tmp
  convertImage tmp out
  where
    extractPartition :: Partition -> Image -> B9 ()
    extractPartition NoPT _ = return ()
    extractPartition (Partition partIndex) (Image outFile Raw) = do
      (start, len, blockSize) <- liftIO $ PartitionTable.getPartition partIndex
                                 outFile
      let tmpFile = outFile <.> "extracted"
      dbgL $ printf "Extracting partition %i from '%s'" partIndex outFile
      cmd $ printf "dd if='%s' of='%s' bs=%i skip=%i count=%i &> /dev/null"
        outFile tmpFile blockSize start len
      cmd $ printf "mv '%s' '%s'" tmpFile outFile

    extractPartition (Partition partIndex) (Image outFile fmt) =
      error $ printf "Extract partition %i from image '%s': Invalid format %s"
                     partIndex outFile (show fmt)

createFS :: FileSystem -> DiskSize -> Image -> B9 ()
createFS imgFs imgSize out = do
  let imgTemp@(Image imgTempFile _) = changeImageFormat Raw out
      fsCmd = case imgFs of
               Ext4 -> "mkfs.ext4"
  dbgL $ printf "Creating empty raw image '%s' with size %s" imgTempFile
    (toQemuSizeOptVal imgSize)
  cmd $ printf "fallocate -l %s '%s'" (toQemuSizeOptVal imgSize) imgTempFile
  dbgL $ printf "Creating file system %s" (show imgFs)
  cmd $ printf "%s -q '%s'" fsCmd imgTempFile
  convertImage imgTemp out

createCOWImage (Image backingFile _) (Image imgOut imgFmt) = do
  dbgL $ printf "Creating COW image '%s' backed by '%s'" imgOut backingFile
  cmd $ printf"qemu-img create -f %s -o backing_file='%s' '%s'"
    (show imgFmt) backingFile imgOut

resizeImage :: DiskResize -> Image -> B9 ()
resizeImage KeepSize _ = return ()
resizeImage (ResizeImage newSize) (Image img _) = do
  dbgL $ printf "Resizing image to %s" $ toQemuSizeOptVal newSize
  cmd $ printf "qemu-img resize -q '%s' %s" img $ toQemuSizeOptVal newSize
resizeImage (ResizeFS Ext4 newSize) (Image img Raw) = do
  let sopt = toQemuSizeOptVal newSize
  dbgL $ printf "Resizing image and filesystem to %s" sopt
  cmd $ printf "qemu-img resize -q '%s' %s" img sopt
  cmd $ printf "resize2fs -f '%s'" img

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
convertImage imgIn imgOut = convert True imgIn imgOut

convert :: Bool -> Image -> Image -> B9 ()
convert doMove (Image imgIn fmtIn) (Image imgOut fmtOut)
  | imgIn == imgOut = do
    ensureDir imgOut
    dbgL $ printf "No need to convert: '%s'" imgIn

  | doMove && fmtIn == fmtOut = do
      ensureDir imgOut
      dbgL $ printf "Moving '%s' to '%s'" imgIn imgOut
      liftIO $ renameFile imgIn imgOut

  | otherwise = do
    ensureDir imgOut
    dbgL $ printf "Converting %s to %s: '%s' to '%s'" (show fmtIn)
      (show fmtOut) imgIn imgOut
    cmd $ printf "qemu-img convert -q -f %s -O %s '%s' '%s'" (show fmtIn)
      (show fmtOut) imgIn imgOut
    when doMove $ do
      dbgL $ printf "Removing '%s'" imgIn
      liftIO $ removeFile imgIn

toQemuSizeOptVal :: DiskSize -> String
toQemuSizeOptVal (DiskSize amount u) = show amount ++ case u of
  GB -> "G"
  MB -> "M"
  KB -> "K"
  B -> ""
