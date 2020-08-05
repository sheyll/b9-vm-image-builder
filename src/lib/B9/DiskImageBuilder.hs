{-# LANGUAGE ScopedTypeVariables #-}

-- | Effectful functions that create and convert disk image files.
module B9.DiskImageBuilder
  ( materializeImageSource,
    substImageTarget,
    preferredDestImageTypes,
    preferredSourceImageTypes,
    resolveImageSource,
    createDestinationImage,
    resizeImage,
    importImage,
    exportImage,
    exportAndRemoveImage,
    convertImage,
    shareImage,
    ensureAbsoluteImageDirExists,
    getVirtualSizeForRawImage
  )
where

import B9.Artifact.Content.StringTemplate
import B9.B9Config
import B9.B9Error
import B9.B9Exec
import B9.B9Logging
import B9.B9Monad
import B9.BuildInfo
import B9.DiskImages
import B9.Environment
import qualified B9.PartitionTable as P
import B9.RepositoryIO
import Control.Eff
import qualified Control.Exception as IO
import Control.Lens (view, (^.))
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as Strict
import Data.Char (isDigit)
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.List
import Data.Maybe
import qualified Foreign.C.Error as IO
import qualified GHC.IO.Exception as IO
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO.B9Extras
  ( ensureDir,
    prettyPrintToFile,
  )
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

-- -- | Convert relative file paths of images, sources and mounted host directories
-- -- to absolute paths relative to '_projectRoot'.
-- makeImagePathsAbsoluteToBuildDirRoot :: ImageTarget -> B9 ImageTarget
-- makeImagePathsAbsoluteToBuildDirRoot img =
--   getConfig >>= maybe (return img) (return . go) . _projectRoot
--   where
--     go rootDir = everywhere mkAbs img
--       where mkAbs = mkT

-- | Replace $... variables inside an 'ImageTarget'
substImageTarget ::
  forall e.
  (HasCallStack, Member EnvironmentReader e, Member ExcB9 e) =>
  ImageTarget ->
  Eff e ImageTarget
substImageTarget = everywhereM gsubst
  where
    gsubst :: GenericM (Eff e)
    gsubst =
      mkM substMountPoint `extM` substImage `extM` substImageSource
        `extM` substDiskTarget
    substMountPoint NotMounted = pure NotMounted
    substMountPoint (MountPoint x) = MountPoint <$> substStr x
    substImage (Image fp t fs) = Image <$> substStr fp <*> pure t <*> pure fs
    substImageSource (From n s) = From <$> substStr n <*> pure s
    substImageSource (EmptyImage l f t s) =
      EmptyImage <$> substStr l <*> pure f <*> pure t <*> pure s
    substImageSource s = pure s
    substDiskTarget (Share n t s) = Share <$> substStr n <*> pure t <*> pure s
    substDiskTarget (LiveInstallerImage name outDir resize) =
      LiveInstallerImage <$> substStr name <*> substStr outDir <*> pure resize
    substDiskTarget s = pure s

-- | Resolve an ImageSource to an 'Image'. The ImageSource might
-- not exist, as is the case for 'EmptyImage'.
resolveImageSource :: IsB9 e => ImageSource -> Eff e Image
resolveImageSource src =
  case src of
    (EmptyImage fsLabel fsType imgType _size) ->
      let img = Image fsLabel imgType fsType
       in return (changeImageFormat imgType img)
    (SourceImage srcImg _part _resize) -> ensureAbsoluteImageDirExists srcImg
    (CopyOnWrite backingImg) -> ensureAbsoluteImageDirExists backingImg
    (From name _resize) ->
      getLatestImageByName (SharedImageName name)
        >>= maybe
          ( errorExitL
              (printf "Nothing found for %s." (show (SharedImageName name)))
          )
          ensureAbsoluteImageDirExists

-- | Return all valid image types sorted by preference.
preferredDestImageTypes :: IsB9 e => ImageSource -> Eff e [ImageType]
preferredDestImageTypes src =
  case src of
    (CopyOnWrite (Image _file fmt _fs)) -> return [fmt]
    (EmptyImage _label NoFileSystem fmt _size) ->
      return (nub [fmt, Raw, QCow2, Vmdk])
    (EmptyImage _label _fs _fmt _size) -> return [Raw]
    (SourceImage _img (Partition _) _resize) -> return [Raw]
    (SourceImage (Image _file fmt _fs) _pt resize) ->
      return
        ( nub [fmt, Raw, QCow2, Vmdk]
            `intersect` allowedImageTypesForResize resize
        )
    (From name resize) ->
      getLatestImageByName (SharedImageName name)
        >>= maybe
          ( errorExitL
              (printf "Nothing found for %s." (show (SharedImageName name)))
          )
          ( \sharedImg ->
              preferredDestImageTypes (SourceImage sharedImg NoPT resize)
          )

-- | Return all supported source 'ImageType's compatible to a 'ImageDestinaion'
-- in the preferred order.
preferredSourceImageTypes :: HasCallStack => ImageDestination -> [ImageType]
preferredSourceImageTypes dest =
  case dest of
    (Share _ fmt resize) ->
      nub [fmt, Raw, QCow2, Vmdk] `intersect` allowedImageTypesForResize resize
    (LocalFile (Image _ fmt _) resize) ->
      nub [fmt, Raw, QCow2, Vmdk] `intersect` allowedImageTypesForResize resize
    Transient -> [Raw, QCow2, Vmdk]
    (LiveInstallerImage _name _repo _imgResize) -> [Raw]

allowedImageTypesForResize :: HasCallStack => ImageResize -> [ImageType]
allowedImageTypesForResize r =
  case r of
    Resize _ -> [Raw]
    ShrinkToMinimumAndIncrease _ -> [Raw]
    ShrinkToMinimum -> [Raw]
    ResizeImage _ -> [Raw, QCow2, Vmdk]
    KeepSize -> [Raw, QCow2, Vmdk]

-- | Create the parent directories for the file that contains the 'Image'.
-- If the path to the image file is relative, prepend '_projectRoot' from
-- the 'B9Config'.
ensureAbsoluteImageDirExists :: IsB9 e => Image -> Eff e Image
ensureAbsoluteImageDirExists img@(Image path _ _) = do
  b9cfg <- getConfig
  let dir =
        let dirRel = takeDirectory path
         in if isRelative dirRel
              then
                let prefix = fromMaybe "." (b9cfg ^. projectRoot)
                 in prefix </> dirRel
              else dirRel
  liftIO $ do
    createDirectoryIfMissing True dir
    dirAbs <- canonicalizePath dir
    return $ changeImageDirectory dirAbs img

-- | Create an image from an image source. The destination image must have a
-- compatible image type and filesystem. The directory of the image MUST be
-- present and the image file itself MUST NOT alredy exist.
materializeImageSource :: IsB9 e => ImageSource -> Image -> Eff e ()
materializeImageSource src dest =
  case src of
    (EmptyImage fsLabel fsType _imgType size) ->
      let (Image _ imgType _) = dest
       in createEmptyImage fsLabel fsType imgType size dest
    (SourceImage srcImg part resize) ->
      createImageFromImage srcImg part resize dest
    (CopyOnWrite backingImg) -> createCOWImage backingImg dest
    (From name resize) ->
      getLatestImageByName (SharedImageName name)
        >>= maybe
          ( errorExitL
              (printf "Nothing found for %s." (show (SharedImageName name)))
          )
          ( \sharedImg ->
              materializeImageSource (SourceImage sharedImg NoPT resize) dest
          )

createImageFromImage ::
  IsB9 e => Image -> Partition -> ImageResize -> Image -> Eff e ()
createImageFromImage src part size out = do
  importImage src out
  extractPartition part out
  resizeImage size out
  where
    extractPartition :: IsB9 e => Partition -> Image -> Eff e ()
    extractPartition NoPT _ = return ()
    extractPartition (Partition partIndex) (Image outFile Raw _) = do
      (start, len, blockSize) <- liftIO (P.getPartition partIndex outFile)
      let tmpFile = outFile <.> "extracted"
      dbgL (printf "Extracting partition %i from '%s'" partIndex outFile)
      cmd
        ( printf
            "dd if='%s' of='%s' bs=%i skip=%i count=%i &> /dev/null"
            outFile
            tmpFile
            blockSize
            start
            len
        )
      cmd (printf "mv '%s' '%s'" tmpFile outFile)
    extractPartition (Partition partIndex) (Image outFile fmt _) =
      error
        ( printf
            "Extract partition %i from image '%s': Invalid format %s"
            partIndex
            outFile
            (imageFileExtension fmt)
        )

-- | Convert some 'Image', e.g. a temporary image used during the build phase
-- to the final destination.
createDestinationImage :: IsB9 e => Image -> ImageDestination -> Eff e ()
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
            repo </> "machines" </> name </> "disks" </> "raw" </> "0.raw"
          sizeFile =
            repo </> "machines" </> name </> "disks" </> "raw" </> "0.size"
          versFile =
            repo </> "machines" </> name </> "disks" </> "raw" </> "VERSION"
      exportAndRemoveImage buildImg destImg
      eitherSize <- getVirtualSizeForRawImage destFile
      case eitherSize of
        Left err -> error err
        Right value -> liftIO (writeFile sizeFile (show value))
      buildDate <- getBuildDate
      buildId <- getBuildId
      liftIO (writeFile versFile (buildId ++ "-" ++ buildDate))
    Transient -> return ()

-- | Determine the virtual size of a raw image
getVirtualSizeForRawImage :: (IsB9 e) => FilePath -> Eff e (Either String Integer)
getVirtualSizeForRawImage file = do
      outPut <- cmdStdout (printf "qemu-img info -f raw '%s'" file)
      return (getVirtualSizeFromQemuImgInfoOutput outPut)

getVirtualSizeFromQemuImgInfoOutput :: Strict.ByteString -> Either String Integer
getVirtualSizeFromQemuImgInfoOutput qemuOutput = case filter (Strict.isPrefixOf (Strict.pack "virtual size"))  (Strict.lines qemuOutput) of
  [] -> Left ("no line starting with 'virtual size' in output while parsing " <> Strict.unpack qemuOutput)
  (_ : _ : _) -> Left ("multiple lines starting with 'virtual size' in output" <> Strict.unpack qemuOutput)
  [x] -> let (digits, rest) = (Strict.span isDigit . Strict.drop 1 . Strict.dropWhile (/= '(')) x
         in
          if Strict.isPrefixOf (Strict.pack " bytes)") rest
          then Right (read (Strict.unpack digits))
          else Left ("rest after digits didn't continue in ' bytes)'" <> Strict.unpack qemuOutput)

createEmptyImage ::
  IsB9 e =>
  String ->
  FileSystem ->
  ImageType ->
  ImageSize ->
  Image ->
  Eff e ()
createEmptyImage fsLabel fsType imgType imgSize dest@(Image _ imgType' fsType')
  | fsType /= fsType' =
    error
      ( printf
          "Conflicting createEmptyImage parameters. Requested is file system %s but the destination image has %s."
          (show fsType)
          (show fsType')
      )
  | imgType /= imgType' =
    error
      ( printf
          "Conflicting createEmptyImage parameters. Requested is image type %s but the destination image has type %s."
          (show imgType)
          (show imgType')
      )
  | otherwise = do
    let (Image imgFile imgFmt imgFs) = dest
        qemuImgOpts = conversionOptions imgFmt
    dbgL
      ( printf
          "Creating empty raw image '%s' with size %s and options %s"
          imgFile
          (toQemuSizeOptVal imgSize)
          qemuImgOpts
      )
    cmd
      ( printf
          "qemu-img create -f %s %s '%s' '%s'"
          (imageFileExtension imgFmt)
          qemuImgOpts
          imgFile
          (toQemuSizeOptVal imgSize)
      )
    case (imgFmt, imgFs) of
      (Raw, Ext4_64) -> do
        let fsCmd = "mkfs.ext4"
        dbgL (printf "Creating file system %s" (show imgFs))
        cmd (printf "%s -F -L '%s' -O 64bit -q '%s'" fsCmd fsLabel imgFile)
      (Raw, Ext4) -> do
        ext4Options <- view ext4Attributes <$> getB9Config
        let fsOptions = "-O " <> intercalate "," ext4Options
        let fsCmd = "mkfs.ext4"
        dbgL (printf "Creating file system %s" (show imgFs))
        cmd (printf "%s -F -L '%s' %s -q '%s'" fsCmd fsLabel fsOptions imgFile)
      (imageType, fs) ->
        error
          ( printf
              "Cannot create file system %s in image type %s"
              (show fs)
              (show imageType)
          )

createCOWImage :: IsB9 e => Image -> Image -> Eff e ()
createCOWImage (Image backingFile _ _) (Image imgOut imgFmt _) = do
  dbgL (printf "Creating COW image '%s' backed by '%s'" imgOut backingFile)
  cmd
    ( printf
        "qemu-img create -f %s -o backing_file='%s' '%s'"
        (imageFileExtension imgFmt)
        backingFile
        imgOut
    )

resizeExtFS :: (IsB9 e) => ImageSize -> FilePath -> Eff e ()
resizeExtFS newSize img = do
  let sizeOpt = toQemuSizeOptVal newSize
  dbgL (printf "Resizing ext4 filesystem on raw image to %s" sizeOpt)
  cmd (printf "e2fsck -p '%s'" img)
  cmd (printf "resize2fs -f '%s' %s" img sizeOpt)

shrinkToMinimumExtFS :: (IsB9 e) => FilePath -> Eff e ()
shrinkToMinimumExtFS img = do
  dbgL "Shrinking image to minimum size"
  cmd (printf "e2fsck -p '%s'" img)
  cmd (printf "resize2fs -f -M '%s'" img)

-- | Resize an image, including the file system inside the image.
resizeImage :: IsB9 e => ImageResize -> Image -> Eff e ()
resizeImage KeepSize _ = return ()
resizeImage (Resize newSize) (Image img Raw fs)
  | fs == Ext4 || fs == Ext4_64 = resizeExtFS newSize img
resizeImage (ShrinkToMinimumAndIncrease sizeIncrease) (Image img Raw fs)
  | fs == Ext4 || fs == Ext4_64 = do
    shrinkToMinimumExtFS img
    fileSize <- liftIO (getFileSize img)
    let newSize =
          addImageSize
            (bytesToKiloBytes (fromInteger fileSize))
            sizeIncrease
    resizeExtFS newSize img
resizeImage (ResizeImage newSize) (Image img _ _) = do
  let sizeOpt = toQemuSizeOptVal newSize
  dbgL (printf "Resizing image to %s" sizeOpt)
  cmd (printf "qemu-img resize -q '%s' %s" img sizeOpt)
resizeImage ShrinkToMinimum (Image img Raw fs)
  | fs == Ext4 || fs == Ext4_64 = shrinkToMinimumExtFS img
resizeImage _ img =
  error
    ( printf
        "Invalid image type or filesystem, cannot resize image: %s"
        (show img)
    )

-- | Import a disk image from some external source into the build directory
-- if necessary convert the image.
importImage :: IsB9 e => Image -> Image -> Eff e ()
importImage imgIn imgOut@(Image imgOutPath _ _) = do
  alreadyThere <- liftIO (doesFileExist imgOutPath)
  unless alreadyThere (convert False imgIn imgOut)

-- | Export a disk image from the build directory; if necessary convert the image.
exportImage :: IsB9 e => Image -> Image -> Eff e ()
exportImage = convert False

-- | Export a disk image from the build directory; if necessary convert the image.
exportAndRemoveImage :: IsB9 e => Image -> Image -> Eff e ()
exportAndRemoveImage = convert True

-- | Convert an image in the build directory to another format and return the new image.
convertImage :: IsB9 e => Image -> Image -> Eff e ()
convertImage imgIn imgOut@(Image imgOutPath _ _) = do
  alreadyThere <- liftIO (doesFileExist imgOutPath)
  unless alreadyThere (convert True imgIn imgOut)

-- | Convert/Copy/Move images
convert :: IsB9 e => Bool -> Image -> Image -> Eff e ()
convert doMove (Image imgIn fmtIn _) (Image imgOut fmtOut _)
  | imgIn == imgOut = do
    ensureDir imgOut
    dbgL (printf "No need to convert: '%s'" imgIn)
  | doMove && fmtIn == fmtOut = do
    ensureDir imgOut
    dbgL (printf "Moving '%s' to '%s'" imgIn imgOut)
    liftIO $ do
      let exdev e =
            if IO.ioe_errno e == Just ((\(IO.Errno a) -> a) IO.eXDEV)
              then copyFile imgIn imgOut >> removeFile imgIn
              else IO.throw e
      renameFile imgIn imgOut `IO.catch` exdev
  | otherwise = do
    ensureDir imgOut
    dbgL
      ( printf
          "Converting %s to %s: '%s' to '%s'"
          (imageFileExtension fmtIn)
          (imageFileExtension fmtOut)
          imgIn
          imgOut
      )
    cmd
      ( printf
          "qemu-img convert -q -f %s -O %s %s '%s' '%s'"
          (imageFileExtension fmtIn)
          (imageFileExtension fmtOut)
          (conversionOptions fmtOut)
          imgIn
          imgOut
      )
    when doMove $ do
      dbgL (printf "Removing '%s'" imgIn)
      liftIO (removeFile imgIn)

conversionOptions :: ImageType -> String
conversionOptions Vmdk = " -o adapter_type=lsilogic "
conversionOptions QCow2 = " -o compat=1.1,lazy_refcounts=on "
conversionOptions _ = " "

toQemuSizeOptVal :: ImageSize -> String
toQemuSizeOptVal (ImageSize amount u) =
  show amount
    ++ case u of
      GB -> "G"
      MB -> "M"
      KB -> "K"

-- | Publish an sharedImage made from an image and image meta data to the
-- configured repository
shareImage :: IsB9 e => Image -> SharedImageName -> Eff e SharedImage
shareImage buildImg sname@(SharedImageName name) = do
  sharedImage <- createSharedImageInCache buildImg sname
  infoL (printf "SHARED '%s'" name)
  pushToSelectedRepo sharedImage
  return sharedImage

-- TODO Move the functions below to RepositoryIO???

-- | Return a 'SharedImage' with the current build data and build id from the
-- name and disk image.
getSharedImageFromImageInfo ::
  IsB9 e => SharedImageName -> Image -> Eff e SharedImage
getSharedImageFromImageInfo name (Image _ imgType imgFS) = do
  buildId <- getBuildId
  date <- getBuildDate
  return
    ( SharedImage
        name
        (SharedImageDate date)
        (SharedImageBuildId buildId)
        imgType
        imgFS
    )

-- | Convert the disk image and serialize the base image data structure.
-- If the 'maxLocalSharedImageRevisions' configuration is set to @Just n@
-- also delete all but the @n - 1@ newest images from the local cache.
createSharedImageInCache ::
  IsB9 e => Image -> SharedImageName -> Eff e SharedImage
createSharedImageInCache img sname@(SharedImageName name) = do
  dbgL (printf "CREATING SHARED IMAGE: '%s' '%s'" (ppShow img) name)
  sharedImg <- getSharedImageFromImageInfo sname img
  dir <- getSharedImagesCacheDir
  convertImage img (changeImageDirectory dir (sharedImageImage sharedImg))
  prettyPrintToFile (dir </> sharedImageFileName sharedImg) sharedImg
  dbgL (printf "CREATED SHARED IMAGE IN CACHE '%s'" (ppShow sharedImg))
  cleanOldSharedImageRevisionsFromCache sname
  return sharedImg
--
--
--
--
--
--
--    imgDir <- getSharedImagesCacheDir
--    let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
--        infoFiles = sharedImageFileName <$> toDelete
--        imgFiles = imageFileName . sharedImageImage <$> toDelete
--    unless (null filesToDelete) $ do
--      traceL
--        ( printf
--            "DELETING %d OBSOLETE REVISIONS OF: %s"
--            (length filesToDelete)
--            (show sn)
--        )
--      mapM_ traceL filesToDelete
--      mapM_ removeIfExists filesToDelete
--  where
--    newestSharedImages :: IsB9 e => Eff e [SharedImage]
--    newestSharedImages =
--      reverse . map snd
--        <$> lookupSharedImages (== Cache) ((sn ==) . sharedImageName)
--    removeIfExists fileName = liftIO $ removeFile fileName `catch` handleExists
--      where
--        handleExists e
--          | isDoesNotExistError e = return ()
--          | otherwise = throwIO e
--
