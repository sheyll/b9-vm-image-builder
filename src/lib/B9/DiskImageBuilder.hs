{-# LANGUAGE ScopedTypeVariables #-}
module B9.DiskImageBuilder (createImage
                           ,resizeImage
                           ,importImage
                           ,exportImage
                           ,exportAndRemoveImage
                           ,convertImage
                           ,shareImage
                           ,pushSharedImageLatestVersion
                           ,lookupSharedImages
                           ,getSharedImages
                           ,pullRemoteRepos
                           ,pullLatestImage
                           ,) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Text.Printf (printf)
import Data.Maybe
import Data.Monoid
import Data.Function
import Control.Applicative
import Text.Show.Pretty (ppShow)
import Data.List

import B9.B9Monad
import B9.Repository
import B9.RepositoryIO
import B9.DiskImages
import B9.ConfigUtils
import qualified B9.PartitionTable as PartitionTable

-- | Create an image as close as possible to 'dest' from 'src' and return the
-- actual image created. NOTE: This maybe diffrent to 'dest'
createImage :: ImageSource -> Image -> B9 Image
createImage src dest = case src of
                        (FileSystem fsType size) ->
                          createFS fsType size dest
                        (SourceImage srcImg part resize) ->
                          createImageFromImage srcImg part resize dest
                        (CopyOnWrite backingImg) ->
                          createCOWImage backingImg dest
                        (From name resize) -> do
                          sharedImg <- getLatestImageByName name
                          createImage (SourceImage sharedImg NoPT resize) dest

createImageFromImage :: Image -> Partition -> DiskResize -> Image -> B9 Image
createImageFromImage src part size out = do
  let tmp = if isPartitioned part
            then changeImageFormat Raw out
            else out
  importImage src tmp
  extractPartition part tmp
  resizeImage size tmp
  return tmp
  where
    extractPartition :: Partition -> Image -> B9 ()
    extractPartition NoPT _ = return ()
    extractPartition (Partition partIndex) (Image outFile Raw) = do
      (start, len, blockSize) <- liftIO $ PartitionTable.getPartition partIndex
                                 outFile
      let tmpFile = outFile <.> "extracted"
      dbgL (printf "Extracting partition %i from '%s'" partIndex outFile)
      cmd (printf "dd if='%s' of='%s' bs=%i skip=%i count=%i &> /dev/null"
                   outFile tmpFile blockSize start len)
      cmd (printf "mv '%s' '%s'" tmpFile outFile)

    extractPartition (Partition partIndex) (Image outFile fmt) =
      error (printf "Extract partition %i from image '%s': Invalid format %s"
                    partIndex outFile (imageFileExtension fmt))

createFS :: FileSystem -> DiskSize -> Image -> B9 Image
createFS imgFs imgSize out = do
  let imgTemp@(Image imgTempFile _) = changeImageFormat Raw out
  dbgL (printf "Creating empty raw image '%s' with size %s" imgTempFile
                (toQemuSizeOptVal imgSize))
  cmd (printf "fallocate -l %s '%s'" (toQemuSizeOptVal imgSize) imgTempFile)
  when (imgFs /= NoFileSystem) $ do
    let fsCmd = "mkfs.ext4"
    dbgL (printf "Creating file system %s" (show imgFs))
    cmd (printf "%s -q '%s'" fsCmd imgTempFile)
  return imgTemp

createCOWImage :: Image -> Image -> B9 Image
createCOWImage (Image backingFile _) out@(Image imgOut imgFmt) = do
  dbgL (printf "Creating COW image '%s' backed by '%s'"
               imgOut backingFile)
  cmd (printf "qemu-img create -f %s -o backing_file='%s' '%s'"
              (imageFileExtension imgFmt) backingFile imgOut)
  return out

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

-- | Convert/Copy/Move images
convert :: Bool -> Image -> Image -> B9 ()
convert doMove (Image imgIn fmtIn) (Image imgOut fmtOut)
  | imgIn == imgOut = do
    ensureDir imgOut
    dbgL (printf "No need to convert: '%s'" imgIn)

  | doMove && fmtIn == fmtOut = do
      ensureDir imgOut
      dbgL (printf "Moving '%s' to '%s'" imgIn imgOut)
      liftIO (renameFile imgIn imgOut)

  | otherwise = do
    ensureDir imgOut
    dbgL (printf "Converting %s to %s: '%s' to '%s'"
                 (imageFileExtension fmtIn) (imageFileExtension fmtOut)
                 imgIn imgOut)
    cmd (printf "qemu-img convert -q -f %s -O %s '%s' '%s'"
                (imageFileExtension fmtIn) (imageFileExtension fmtOut)
                imgIn imgOut)
    when doMove $ do
      dbgL (printf "Removing '%s'" imgIn)
      liftIO (removeFile imgIn)

toQemuSizeOptVal :: DiskSize -> String
toQemuSizeOptVal (DiskSize amount u) = show amount ++ case u of
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
getSharedImageFromImageInfo name (Image _ imgType) = do
   buildId <- getBuildId
   date <- getBuildDate
   return (SharedImage name
                       (SharedImageDate date)
                       (SharedImageBuildId buildId)
                       imgType)

-- | Convert the disk image and serialize the base image data structure.
createSharedImageInCache :: Image -> SharedImageName -> B9 SharedImage
createSharedImageInCache img sname@(SharedImageName name) = do
  dbgL (printf "CREATING SHARED IMAGE: '%s' '%s'" (ppShow img) name)
  sharedImg <- getSharedImageFromImageInfo sname img
  dir <- getSharedImagesCacheDir
  convertImage img (changeImageDirectory dir (sharedImageImage sharedImg))
  tell (dir </> sharedImageFileName sharedImg) sharedImg
  dbgL (printf "CREATED SHARED IMAGE IN CAHCE '%s'" (ppShow sharedImg))
  return sharedImg


-- | Publish the latest version of a shared image identified by name to the
-- selected repository from the cache.
pushSharedImageLatestVersion :: SharedImageName -> B9 ()
pushSharedImageLatestVersion (SharedImageName imgName) = do
  sharedImage <- getLatestSharedImageByNameFromCache imgName
  dbgL (printf "PUSHING '%s'" (ppShow sharedImage))
  pushToSelectedRepo sharedImage
  infoL (printf "PUSHED '%s'" imgName)

-- | Upload a shared image from the cache to a selected remote repository
pushToSelectedRepo :: SharedImage -> B9 ()
pushToSelectedRepo i = do
  c <- getSharedImagesCacheDir
  r <- getSelectedRemoteRepo
  when (isJust r) $ do
    let (Image imgFile' _imgType) = sharedImageImage i
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
     dl = pullGlob sharedImagesRootDirectory
                       (FileExtension sharedImageFileExtension)

-- | Pull the latest version of an image, either from the selected remote
-- repo or from the repo that has the latest version.
pullLatestImage :: SharedImageName -> B9 Bool
pullLatestImage (SharedImageName name) = do
  repos <- getSelectedRepos
  let repoPredicate Cache = False
      repoPredicate (Remote repoId) = repoId `elem` repoIds
      repoIds = map remoteRepoRepoId repos
      hasName sharedImage = name == siName sharedImage
  candidates <- lookupSharedImages repoPredicate hasName
  let (Remote repoId, image) = head (reverse candidates)
  if null candidates
     then do errorL (printf "No shared image named '%s'\
                            \ on these remote repositories: '%s'"
                            name
                            (ppShow repoIds))
             return False
     else do dbgL (printf "PULLING SHARED IMAGE: '%s'" (ppShow image))
             cacheDir <- getSharedImagesCacheDir
             let (Image imgFile' _imgType) = sharedImageImage image
                 cachedImgFile = cacheDir </> imgFile'
                 cachedInfoFile = cacheDir </> sharedImageFileName image
                 repoImgFile = sharedImagesRootDirectory </> imgFile'
                 repoInfoFile = sharedImagesRootDirectory
                                </> sharedImageFileName image
                 repo = fromJust (lookupRemoteRepo repos repoId)
             pullFromRepo repo repoImgFile cachedImgFile
             pullFromRepo repo repoInfoFile cachedInfoFile
             infoL (printf "PULLED '%s' FROM '%s'"
                           name
                           repoId)
             return True


-- | Return the 'Image' of the latest version of a shared image named 'name'
-- from the local cache.
getLatestImageByName :: String -> B9 Image
getLatestImageByName name = do
  sharedImage <- getLatestSharedImageByNameFromCache name
  cacheDir <- getSharedImagesCacheDir
  let image = changeImageDirectory cacheDir (sharedImageImage sharedImage)
  dbgL (printf "USING SHARED SOURCE IMAGE '%s'" (show image))
  return image

-- | Return the latest version of a shared image named 'name' from the local cache.
getLatestSharedImageByNameFromCache :: String -> B9 SharedImage
getLatestSharedImageByNameFromCache name = do
  imgs <- lookupSharedImages (== Cache) ((== name) . siName)
  case reverse imgs of
    [] ->
      error (printf "No image(s) named '%s' found." name)
    (Cache, sharedImage):_rest ->
      return sharedImage

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages :: B9 [(Repository, [SharedImage])]
getSharedImages = do
  reposAndFiles <- repoSearch sharedImagesRootDirectory
                              (FileExtension sharedImageFileExtension)
  mapM (\(repo,files) -> ((repo,) . catMaybes) <$> mapM consult' files) reposAndFiles
  where
    consult' f = do
      r <- liftIO (try (consult f))
      case r of
        Left (e :: SomeException) -> do
          dbgL (printf "Failed to load shared image meta-data from '%s': '%s'"
                       (takeFileName f) (show e))
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
   let rs = [(r,s) | (r,ss) <- xs, s <- ss]
       matchingRepo = filter (repoPred . fst) rs
       matchingImg = filter (imgPred . snd) matchingRepo
       sorted = sortBy (compare `on` snd) matchingImg
   return (mconcat (pure <$> sorted))

-- | Return either all remote repos or just the single selected repo.
getSelectedRepos :: B9 [RemoteRepo]
getSelectedRepos = do
  allRepos <- getRemoteRepos
  selectedRepo <- getSelectedRemoteRepo
  let repos = maybe allRepos     -- user has not selected a repo
                    return       -- user has selected a repo, return it as
                                 -- singleton list
                    selectedRepo -- 'Maybe' a repo
  return repos

-- | Return the path to the sub directory in the cache that contains files of
-- shared images.
getSharedImagesCacheDir :: B9 FilePath
getSharedImagesCacheDir = do
  cacheDir <- localRepoDir <$> getRepoCache
  return (cacheDir </> sharedImagesRootDirectory)
