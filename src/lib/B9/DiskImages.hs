module B9.DiskImages where

import Control.Applicative ( (<$>) )
import Control.Exception ( bracket )
import Control.Monad ( when )
import Control.Monad.IO.Class
import Data.List ( nub )
import Data.Maybe ( isJust, fromJust )
import Data.Word ( Word32 )
import System.Directory ( createDirectoryIfMissing
                        , createDirectory
                        , setCurrentDirectory
                        , getCurrentDirectory
                        , canonicalizePath
                        , renameFile
                        , removeFile
                        , copyFile
                        , removeDirectoryRecursive
                        )
import System.FilePath ( takeDirectory
                       , takeFileName
                       , replaceExtension
                       , (<.>)
                       , (</>) )
import           System.Process ( callCommand )
import           System.Random ( randomIO )
import           Text.Printf ( printf )

import qualified B9.PartitionTable as PartitionTable
import           B9.B9Monad

data DiskSize = DiskSize Int SizeUnit deriving (Eq, Show, Read)

data DiskResize = KeepSize
                | ResizeImage DiskSize
                | ResizeFS FileSystem DiskSize deriving (Eq, Show, Read)

data Partition = NoPT | Partition Int deriving (Eq, Show, Read)

data SizeUnit = B | KB | MB | GB deriving (Eq, Show, Read, Ord)

isPartitioned :: Partition -> Bool
isPartitioned p | p == NoPT = False
                | otherwise = True

getPartition :: Partition -> Int
getPartition (Partition p) = p
getPartition NoPT = error "No partitions!"

newtype MountPoint = MountPoint FilePath deriving (Show, Read)

type Mounted a = (a, MountPoint)

data ImageSource = FileSystem FileSystem DiskSize
                 | SourceImage Image Partition DiskResize
                 | CopyOnWrite Image
                 deriving (Show, Read)

data FileSystem = NoFileSystem | Ext4 deriving (Eq, Show, Read)

data ImageType = Raw | QCow2 | Vmdk deriving (Eq, Read)

instance Show ImageType where
  show Raw = "raw"
  show QCow2 = "qcow2"
  show Vmdk = "vmdk"

data Image = Image FilePath ImageType deriving (Eq, Show, Read)

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
  convert False src tmp
  extractPartition part tmp
  resizeImage size tmp
  convert True tmp out
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
  convert True imgTemp out

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

convert :: Bool -> Image -> Image -> B9 ()
convert doMove (Image imgIn fmtIn) (Image imgOut fmtOut)
  | (imgIn ==imgOut) = dbgL $ printf "No need to convert: '%s'" imgIn

  | doMove && (fmtIn ==fmtOut) = do
      dbgL $ printf "Moving '%s' to '%s'" imgIn imgOut
      liftIO $ renameFile imgIn imgOut

  | otherwise = do
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
