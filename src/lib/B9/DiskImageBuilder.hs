{-# LANGUAGE ScopedTypeVariables #-}
{-| Effectful functions that create and convert disk image files. -}
module B9.DiskImageBuilder
       (materializeImageSource, substImageTarget, resolveImageSource,
        createDestinationImage, ensureAbsoluteImageDirExists)
       where

import           B9.B9Monad
import           B9.ConfigUtils
import           B9.Content
import           B9.DiskImages
import qualified B9.PartitionTable as P
import           B9.QemuImg
import           B9.RepositoryIO
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Data
import           Data.Function
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Text.Printf (printf)
import           Text.Show.Pretty (ppShow)


-- | Replace $... variables inside an 'ImageTarget'
substImageTarget :: [(String, String)] -> ImageTarget -> ImageTarget
substImageTarget env = everywhere gsubst
  where
    gsubst
        :: Data a
        => a -> a
    gsubst =
        mkT substMountPoint `extT` substImage `extT` substImageSource `extT`
        substDiskTarget
    substMountPoint NotMounted = NotMounted
    substMountPoint (MountPoint x) = MountPoint (sub x)
    substImage (Image fp t fs) = Image (sub fp) t fs
    substImageSource (From n s) = From (sub n) s
    substImageSource (EmptyImage l f t s) = EmptyImage (sub l) f t s
    substImageSource s = s
    substDiskTarget (Share n t s) = Share (sub n) t s
    substDiskTarget (LiveInstallerImage name outDir resize) =
        LiveInstallerImage (sub name) (sub outDir) resize
    substDiskTarget s = s
    sub = subst env

-- | Resolve an ImageSource to an 'Image'. Note however that this source will
-- may not exist as is the case for 'EmptyImage'.
resolveImageSource :: ImageSource -> B9 Image
resolveImageSource src =
    case src of
        (EmptyImage fsLabel fsType imgType _size) ->
            let img = Image fsLabel imgType fsType
            in return (changeImageFormat imgType img)
        (SourceImage srcImg _part _resize) ->
            liftIO (ensureAbsoluteImageDirExists srcImg)
        (CopyOnWrite backingImg) ->
            liftIO (ensureAbsoluteImageDirExists backingImg)
        (From name _resize) -> do
            latestImage <- getLatestImageByName (SharedImageName name)
            liftIO (ensureAbsoluteImageDirExists latestImage)

-- | Create an image from an image source. The destination image must have a
-- compatible image type and filesystem. The directory of the image MUST be
-- present and the image file itself MUST NOT alredy exist.
materializeImageSource :: ImageSource -> Image -> B9 ()
materializeImageSource src dest =
  case src of
    (EmptyImage fsLabel fsType _imgType size) ->
      let (Image _ imgType _) = dest
      in createEmptyImage fsLabel fsType imgType size dest
    (SourceImage srcImg part resize) ->
      createImageFromImage srcImg part resize dest
    (CopyOnWrite backingImg) ->
      createCOWImage backingImg dest
    (From name resize) -> do
      sharedImg <- getLatestImageByName (SharedImageName name)
      materializeImageSource (SourceImage sharedImg NoPT resize) dest

createImageFromImage :: Image -> Partition -> ImageResize -> Image -> B9 ()
createImageFromImage src part size out = do
    importImage src out
    extractPartition part out
    resizeImage size out
  where
    extractPartition :: Partition -> Image -> B9 ()
    extractPartition NoPT _ = return ()
    extractPartition (Partition partIndex) (Image outFile Raw _) = do
        (start,len,blockSize) <- liftIO (P.getPartition partIndex outFile)
        let tmpFile = outFile <.> "extracted"
        dbgL (printf "Extracting partition %i from '%s'" partIndex outFile)
        cmd
            (printf
                 "dd if='%s' of='%s' bs=%i skip=%i count=%i &> /dev/null"
                 outFile
                 tmpFile
                 blockSize
                 start
                 len)
        cmd (printf "mv '%s' '%s'" tmpFile outFile)
    extractPartition (Partition partIndex) (Image outFile fmt fs) =
        error
            (printf
                 "Extract partition %i from image '%s': Invalid format %s"
                 partIndex
                 outFile
                 (imageFileExtension fmt fs))

createDestinationImage :: Image -> ImageDestination -> B9 ()
createDestinationImage buildImg dest =
    case dest of
        (Share name imgType imgResize) -> do
            resizeImage imgResize buildImg
            let shareableImg = changeImageFormat imgType buildImg
            exportAndRemoveImage buildImg shareableImg
            void (shareImage shareableImg (SharedImageName name))
        (LocalFile destImg imgResize) -> do
            resizeImage imgResize buildImg
            exportAndRemoveImage buildImg destImg
        (LiveInstallerImage name repo imgResize) -> do
            resizeImage imgResize buildImg
            let destImg = Image destFile Raw buildImgFs
                (Image _ _ buildImgFs) = buildImg
                destFile =
                    repo </> "machines" </> name </> "disks" </> "raw" </>
                    "0.raw"
                sizeFile =
                    repo </> "machines" </> name </> "disks" </> "raw" </>
                    "0.size"
                versFile =
                    repo </> "machines" </> name </> "disks" </> "raw" </>
                    "VERSION"
            exportAndRemoveImage buildImg destImg
            cmd
                (printf
                     "echo $(qemu-img info -f raw '%s' | gawk -e '/virtual size/ {print $4}' | tr -d '(') > '%s'"
                     destFile
                     sizeFile)
            buildDate <- getBuildDate
            buildId <- getBuildId
            liftIO (writeFile versFile (buildId ++ "-" ++ buildDate))
        Transient -> return ()
