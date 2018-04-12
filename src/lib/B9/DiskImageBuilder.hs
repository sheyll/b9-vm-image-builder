{-# LANGUAGE ScopedTypeVariables #-}
{-| Effectful functions that create and convert disk image files. -}
module B9.DiskImageBuilder (materializeImageSource
                           ,substImageTarget
                           ,preferredDestImageTypes
                           ,preferredSourceImageTypes
                           ,resolveImageSource
                           ,createDestinationImage
                           ,resizeImage
                           ,importImage
                           ,exportImage
                           ,exportAndRemoveImage
                           ,convertImage
                           ,shareImage
                           ,ensureAbsoluteImageDirExists
                           ,pushSharedImageLatestVersion
                           ,lookupSharedImages
                           ,getSharedImages
                           ,getSharedImagesCacheDir
                           ,getSelectedRepos
                           ,pullRemoteRepos
                           ,pullLatestImage
                           ,) where

import System.IO.Error (isDoesNotExistError)
import System.Directory (removeFile)
import System.IO.B9Extras (ensureDir, prettyPrintToFile, consult)
import Control.Lens ((^.))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Text.Printf (printf)
import Data.Maybe
import Data.Function
import Text.Show.Pretty (ppShow)
import Data.List
import Data.Data
import Data.Generics.Schemes
import Data.Generics.Aliases
import B9.B9Monad
import B9.B9Config
import B9.Repository
import B9.RepositoryIO
import B9.DiskImages
import qualified B9.PartitionTable as P
import B9.Content.StringTemplate


-- | Replace $... variables inside an 'ImageTarget'
substImageTarget :: [(String,String)] -> ImageTarget -> ImageTarget
substImageTarget env = everywhere gsubst
  where
    gsubst :: Data a => a -> a
    gsubst = mkT substMountPoint
             `extT` substImage
             `extT` substImageSource
             `extT` substDiskTarget

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

-- | Resolve an ImageSource to an 'Image'. The ImageSource might
-- not exist, as is the case for 'EmptyImage'.
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
    (From name _resize) ->
      getLatestImageByName (SharedImageName name)
        >>= maybe (errorExitL (printf "Nothing found for %s."
                                      (show (SharedImageName name))))
                  (liftIO . ensureAbsoluteImageDirExists)

-- | Return all valid image types sorted by preference.
preferredDestImageTypes :: ImageSource -> B9 [ImageType]
preferredDestImageTypes src =
  case src of
    (CopyOnWrite (Image _file fmt _fs)) -> return [fmt]
    (EmptyImage _label NoFileSystem fmt _size) ->
      return (nub [fmt, Raw, QCow2, Vmdk])
    (EmptyImage _label _fs _fmt _size) -> return [Raw]
    (SourceImage _img (Partition _) _resize) -> return [Raw]
    (SourceImage (Image _file fmt _fs) _pt resize) ->
      return
        (nub [fmt, Raw, QCow2, Vmdk]
         `intersect`
         allowedImageTypesForResize resize)
    (From name resize) ->
      getLatestImageByName (SharedImageName name)
        >>= maybe (errorExitL (printf "Nothing found for %s."
                                       (show (SharedImageName name))))
                  (\sharedImg ->
                    preferredDestImageTypes (SourceImage sharedImg NoPT resize))

-- | Return all supported source 'ImageType's compatible to a 'ImageDestinaion'
-- in the preferred order.
preferredSourceImageTypes :: ImageDestination -> [ImageType]
preferredSourceImageTypes dest =
  case dest of
    (Share _ fmt resize) ->
      nub [fmt, Raw, QCow2, Vmdk]
      `intersect` allowedImageTypesForResize resize
    (LocalFile (Image _ fmt _) resize) ->
      nub [fmt, Raw, QCow2, Vmdk]
      `intersect` allowedImageTypesForResize resize
    Transient ->
      [Raw, QCow2, Vmdk]
    (LiveInstallerImage _name _repo _imgResize) ->
      [Raw]

allowedImageTypesForResize :: ImageResize -> [ImageType]
allowedImageTypesForResize r =
  case r of
    Resize _ -> [Raw]
    ShrinkToMinimum -> [Raw]
    _ -> [Raw, QCow2, Vmdk]

-- | Create the parent directories for the file that contains the 'Image'.
ensureAbsoluteImageDirExists :: Image -> IO Image
ensureAbsoluteImageDirExists img@(Image path _ _) = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  dirAbs <- canonicalizePath dir
  return $ changeImageDirectory dirAbs img

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
      getLatestImageByName (SharedImageName name)
        >>= maybe (errorExitL (printf "Nothing found for %s."
                                       (show (SharedImageName name))))
                  (\sharedImg ->
                        materializeImageSource
                          (SourceImage sharedImg NoPT resize) dest)

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
    extractPartition (Partition partIndex) (Image outFile fmt _) =
        error
            (printf
                 "Extract partition %i from image '%s': Invalid format %s"
                 partIndex
                 outFile
                 (imageFileExtension fmt))

-- | Convert some 'Image', e.g. a temporary image used during the build phase
-- to the final destination.
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

createEmptyImage :: String
                 -> FileSystem
                 -> ImageType
                 -> ImageSize
                 -> Image
                 -> B9 ()
createEmptyImage fsLabel fsType imgType imgSize dest@(Image _ imgType' fsType')
  | fsType /= fsType' =
      error
          (printf
               "Conflicting createEmptyImage parameters. Requested is file system %s but the destination image has %s."
               (show fsType)
               (show fsType'))
  | imgType /= imgType' =
      error
          (printf
               "Conflicting createEmptyImage parameters. Requested is image type %s but the destination image has type %s."
               (show imgType)
               (show imgType'))
  | otherwise = do
      let (Image imgFile imgFmt imgFs) = dest
          qemuImgOpts = conversionOptions imgFmt
      dbgL
          (printf
               "Creating empty raw image '%s' with size %s and options %s"
               imgFile
               (toQemuSizeOptVal imgSize)
               qemuImgOpts)
      cmd
          (printf
               "qemu-img create -f %s %s '%s' '%s'"
               (imageFileExtension imgFmt)
               qemuImgOpts
               imgFile
               (toQemuSizeOptVal imgSize))
      case (imgFmt, imgFs) of
          (Raw,Ext4_64) -> do
              let fsCmd = "mkfs.ext4"
              dbgL (printf "Creating file system %s" (show imgFs))
              cmd (printf "%s -F -L '%s' -O 64bit -q '%s'" fsCmd fsLabel imgFile)
          (Raw,Ext4) -> do
              let fsCmd = "mkfs.ext4"
              dbgL (printf "Creating file system %s" (show imgFs))
              cmd (printf "%s -F -L '%s' -O ^64bit -q '%s'" fsCmd fsLabel imgFile)
          (it,fs) ->
              error
                  (printf
                       "Cannot create file system %s in image type %s"
                       (show fs)
                       (show it))


createCOWImage :: Image -> Image -> B9 ()
createCOWImage (Image backingFile _ _) (Image imgOut imgFmt _) = do
  dbgL (printf "Creating COW image '%s' backed by '%s'" imgOut backingFile)
  cmd
    (printf
       "qemu-img create -f %s -o backing_file='%s' '%s'"
       (imageFileExtension imgFmt)
       backingFile
       imgOut)

-- | Resize an image, including the file system inside the image.
resizeImage :: ImageResize -> Image -> B9 ()
resizeImage KeepSize _ = return ()
resizeImage (Resize newSize) (Image img Raw fs)
  | fs == Ext4 || fs == Ext4_64
    = do
        let sizeOpt = toQemuSizeOptVal newSize
        dbgL (printf "Resizing ext4 filesystem on raw image to %s" sizeOpt)
        cmd (printf "e2fsck -p '%s'" img)
        cmd (printf "resize2fs -f '%s' %s" img sizeOpt)

resizeImage (ResizeImage newSize) (Image img _ _) = do
  let sizeOpt = toQemuSizeOptVal newSize
  dbgL (printf "Resizing image to %s" sizeOpt)
  cmd (printf "qemu-img resize -q '%s' %s" img sizeOpt)

resizeImage ShrinkToMinimum (Image img Raw fs)
 | fs == Ext4 || fs == Ext4_64
      = do
          dbgL "Shrinking image to minimum size"
          cmd (printf "e2fsck -p '%s'" img)
          cmd (printf "resize2fs -f -M '%s'" img)

resizeImage _ img =
  error (printf "Invalid image type or filesystem, cannot resize image: %s" (show img))


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

-- | Convert/Copy/Move images
convert :: Bool -> Image -> Image -> B9 ()
convert doMove (Image imgIn fmtIn _) (Image imgOut fmtOut _)
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
           (imageFileExtension fmtIn)
           (imageFileExtension fmtOut)
           imgIn
           imgOut)
      cmd
        (printf
           "qemu-img convert -q -f %s -O %s %s '%s' '%s'"
           (imageFileExtension fmtIn)
           (imageFileExtension fmtOut)
           (conversionOptions fmtOut)
           imgIn
           imgOut)
      when doMove $ do
        dbgL (printf "Removing '%s'" imgIn)
        liftIO (removeFile imgIn)

conversionOptions :: ImageType -> String
conversionOptions Vmdk  = " -o adapter_type=lsilogic "
conversionOptions QCow2 = " -o compat=1.1,lazy_refcounts=on "
conversionOptions _     = " "

toQemuSizeOptVal :: ImageSize -> String
toQemuSizeOptVal (ImageSize amount u) = show amount ++ case u of
  GB -> "G"
  MB -> "M"
  KB -> "K"
  B -> ""

-- | Publish an sharedImage made from an image and image meta data to the
-- configured repository
shareImage :: Image -> SharedImageName -> B9 SharedImage
shareImage buildImg sname@(SharedImageName name) = do
  sharedImage <- createSharedImageInCache buildImg sname
  infoL (printf "SHARED '%s'" name)
  pushToSelectedRepo sharedImage
  return sharedImage

-- | Return a 'SharedImage' with the current build data and build id from the
-- name and disk image.
getSharedImageFromImageInfo :: SharedImageName -> Image -> B9 SharedImage
getSharedImageFromImageInfo name (Image _ imgType imgFS) = do
  buildId <- getBuildId
  date <- getBuildDate
  return (SharedImage name (SharedImageDate date) (SharedImageBuildId buildId) imgType imgFS)

-- | Convert the disk image and serialize the base image data structure.
-- If the 'maxLocalSharedImageRevisions' configuration is set to @Just n@
-- also delete all but the @n - 1@ newest images from the local cache.
createSharedImageInCache :: Image -> SharedImageName -> B9 SharedImage
createSharedImageInCache img sname@(SharedImageName name) = do
  dbgL (printf "CREATING SHARED IMAGE: '%s' '%s'" (ppShow img) name)
  sharedImg <- getSharedImageFromImageInfo sname img
  dir <- getSharedImagesCacheDir
  convertImage img (changeImageDirectory dir (sharedImageImage sharedImg))
  prettyPrintToFile (dir </> sharedImageFileName sharedImg) sharedImg
  dbgL (printf "CREATED SHARED IMAGE IN CACHE '%s'" (ppShow sharedImg))
  cleanOldSharedImageRevisionsFromCache sname
  return sharedImg

-- | Publish the latest version of a shared image identified by name to the
-- selected repository from the cache.
pushSharedImageLatestVersion :: SharedImageName -> B9 ()
pushSharedImageLatestVersion name@(SharedImageName imgName) =
  getLatestSharedImageByNameFromCache name
    >>= maybe (errorExitL (printf "Nothing found for %s." (show imgName)))
              (\sharedImage -> do
                    dbgL (printf "PUSHING '%s'" (ppShow sharedImage))
                    pushToSelectedRepo sharedImage
                    infoL (printf "PUSHED '%s'" imgName))

-- | Upload a shared image from the cache to a selected remote repository
pushToSelectedRepo :: SharedImage -> B9 ()
pushToSelectedRepo i = do
  c <- getSharedImagesCacheDir
  r <- getSelectedRemoteRepo
  when (isJust r) $ do
    let (Image imgFile' _imgType _imgFS) = sharedImageImage i
        cachedImgFile = c </> imgFile'
        cachedInfoFile = c </> sharedImageFileName i
        repoImgFile = sharedImagesRootDirectory </> imgFile'
        repoInfoFile = sharedImagesRootDirectory </> sharedImageFileName i
    pushToRepo (fromJust r) cachedImgFile repoImgFile
    pushToRepo (fromJust r) cachedInfoFile repoInfoFile

-- | Pull metadata files from all remote repositories.
pullRemoteRepos :: B9 ()
pullRemoteRepos = do
  repos <- getSelectedRepos
  mapM_ dl repos

  where
    dl = pullGlob sharedImagesRootDirectory (FileExtension sharedImageFileExtension)

-- | Pull the latest version of an image, either from the selected remote
-- repo or from the repo that has the latest version.
pullLatestImage :: SharedImageName -> B9 (Maybe SharedImageBuildId)
pullLatestImage name@(SharedImageName dbgName) = do
  repos <- getSelectedRepos
  let repoPredicate Cache = False
      repoPredicate (Remote repoId) = repoId `elem` repoIds
      repoIds = map remoteRepoRepoId repos
      hasName sharedImage = name == sharedImageName sharedImage
  candidates <- lookupSharedImages repoPredicate hasName
  let (Remote repoId, image) = last candidates
  if null candidates
    then do
      errorL
        (printf "No shared image named '%s' on these remote repositories: '%s'" dbgName
           (ppShow repoIds))
      return Nothing
    else do
      dbgL (printf "PULLING SHARED IMAGE: '%s'" (ppShow image))
      cacheDir <- getSharedImagesCacheDir
      let (Image imgFile' _imgType _fs) = sharedImageImage image
          cachedImgFile = cacheDir </> imgFile'
          cachedInfoFile = cacheDir </> sharedImageFileName image
          repoImgFile = sharedImagesRootDirectory </> imgFile'
          repoInfoFile = sharedImagesRootDirectory
                         </> sharedImageFileName image
          repo = fromJust (lookupRemoteRepo repos repoId)
      pullFromRepo repo repoImgFile cachedImgFile
      pullFromRepo repo repoInfoFile cachedInfoFile
      infoL (printf "PULLED '%s' FROM '%s'" dbgName repoId)
      cleanOldSharedImageRevisionsFromCache name
      return (Just (sharedImageBuildId image))

-- | Return the 'Image' of the latest version of a shared image named 'name'
-- from the local cache.
getLatestImageByName :: SharedImageName -> B9 (Maybe Image)
getLatestImageByName name = do
  sharedImage <- getLatestSharedImageByNameFromCache name
  cacheDir <- getSharedImagesCacheDir
  let image = changeImageDirectory cacheDir . sharedImageImage <$> sharedImage
  case image of
    Just i ->
      dbgL (printf "USING SHARED SOURCE IMAGE '%s'" (show i))
    Nothing ->
      errorL (printf "SOURCE IMAGE '%s' NOT FOUND" (show name))
  return image

-- | Return the latest version of a shared image named 'name' from the local cache.
getLatestSharedImageByNameFromCache :: SharedImageName -> B9 (Maybe SharedImage)
getLatestSharedImageByNameFromCache name@(SharedImageName dbgName) = do
  imgs <- lookupSharedImages (== Cache) ((== name) . sharedImageName)
  case reverse imgs of
    (Cache, sharedImage):_rest ->
      return (Just sharedImage)
    _ -> do
      errorL (printf "No image(s) named '%s' found." dbgName)
      return Nothing

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages :: B9 [(Repository, [SharedImage])]
getSharedImages = do
  reposAndFiles <- repoSearch sharedImagesRootDirectory (FileExtension sharedImageFileExtension)
  mapM (\(repo, files) -> ((repo,) . catMaybes) <$> mapM consult' files) reposAndFiles

  where
    consult' f = do
      r <- liftIO (try (consult f))
      case r of
        Left (e :: SomeException) -> do
          dbgL
            (printf "Failed to load shared image meta-data from '%s': '%s'" (takeFileName f)
               (show e))
          dbgL (printf "Removing bad meta-data file '%s'" f)
          liftIO (removeFile f)
          return Nothing
        Right c ->
          return (Just c)

-- | Find shared images and the associated repos from two predicates. The result
-- is the concatenated result of the sorted shared images satisfying 'imgPred'.
lookupSharedImages :: (Repository -> Bool)
                   -> (SharedImage -> Bool)
                   -> B9 [(Repository, SharedImage)]
lookupSharedImages repoPred imgPred = do
  xs <- getSharedImages
  let rs = [(r, s) | (r, ss) <- xs
                   , s <- ss]
      matchingRepo = filter (repoPred . fst) rs
      matchingImg = filter (imgPred . snd) matchingRepo
      sorted = sortBy (compare `on` snd) matchingImg
  return (mconcat (pure <$> sorted))

-- | Return either all remote repos or just the single selected repo.
getSelectedRepos :: B9 [RemoteRepo]
getSelectedRepos = do
  allRepos <- getRemoteRepos
  selectedRepo <- getSelectedRemoteRepo
  let repos = maybe
                allRepos
                return
                selectedRepo -- 'Maybe' a repo
  return repos

-- | Return the path to the sub directory in the cache that contains files of
-- shared images.
getSharedImagesCacheDir :: B9 FilePath
getSharedImagesCacheDir = do
  cacheDir <- localRepoDir <$> getRepoCache
  return (cacheDir </> sharedImagesRootDirectory)

-- | Depending on the 'maxLocalSharedImageRevisions' 'B9Config' settings either
-- do nothing or delete all but the configured number of most recent shared
-- images with the given name from the local cache.
cleanOldSharedImageRevisionsFromCache :: SharedImageName -> B9 ()
cleanOldSharedImageRevisionsFromCache sn = do
    b9Cfg <- getConfig
    forM_ (b9Cfg ^. maxLocalSharedImageRevisions) $ \maxRevisions -> do
      toDelete <- take maxRevisions <$> newestSharedImages
      imgDir <- getSharedImagesCacheDir
      let filesToDelete = (imgDir </>) <$> (infoFiles ++ imgFiles)
          infoFiles     = sharedImageFileName <$> toDelete
          imgFiles      = imageFileName . sharedImageImage <$> toDelete
      unless (null filesToDelete) $ do
          traceL (printf "DELETING %d OBSOLETE REVISIONS OF: %s"
                        (length filesToDelete) (show sn))
          mapM_ traceL filesToDelete
          mapM_ removeIfExists filesToDelete
   where
    newestSharedImages :: B9 [SharedImage]
    newestSharedImages =
      reverse . map snd <$> lookupSharedImages (== Cache) ((sn ==) . sharedImageName)

    removeIfExists fileName = liftIO $ removeFile fileName `catch` handleExists
     where
      handleExists e | isDoesNotExistError e = return ()
                     | otherwise             = throwIO e
