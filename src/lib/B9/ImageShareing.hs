module B9.ImageShareing (shareImage
                        ,getSharedImages) where

import B9.B9Monad
import B9.ConfigUtils
import B9.DiskImages
import B9.DiskImageBuilder
import B9.Repository
import B9.RepositoryIO

import Control.Arrow
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Format
import System.FilePath
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)
import Data.List

-- | Publish an sharedImage made from an image and image meta data to the
-- configured repository
shareImage :: Image -> ImageInfo -> B9 SharedImage
shareImage buildImg info = do
  sharedImage <- createSharedImageInCache buildImg info
  infoL (printf "SHARED IMAGE '%s' TO LOCAL REPO" (show info))
  uploadToSelectedRepo sharedImage
  return sharedImage

-- | Return a list of all existing sharedImages from cached repositories.
getSharedImages :: B9 [(Repository, [SharedImage])]
getSharedImages = do
  reposAndFiles <- repoSearch sharedImagesRootDirectory
                              (FileNameEndsWith sharedImageFileExtension)
  mapM (\(repo,files) -> (repo,) <$> mapM consult files) reposAndFiles

-- * Internals:

uploadToSelectedRepo :: SharedImage -> B9 ()
uploadToSelectedRepo i = do
  c <- getSharedImagesCacheDir
  r <- getSelectedRemoteRepo
  when (isJust r) $ do
    let (Image imgFile' _imgType) = sharedImageImage i
        cachedImgFile = c </> imgFile'
        cachedInfoFile = c </> sharedImageFileName i
        repoImgFile = sharedImagesRootDirectory </> imgFile'
        repoInfoFile = sharedImagesRootDirectory </> sharedImageFileName i
    uploadToRepo (fromJust r) cachedImgFile repoImgFile
    uploadToRepo (fromJust r) cachedInfoFile repoInfoFile

-- | Convert the disk image and serialize the base image data structure.
createSharedImageInCache :: Image -> ImageInfo -> B9 SharedImage
createSharedImageInCache img info = do
  sharedImg <- sharedImage info
  dir <- getSharedImagesCacheDir
  convertImage img (changeImageDirectory dir (sharedImageImage sharedImg))
  tell (dir </> sharedImageFileName sharedImg) sharedImg
  return sharedImg
  where
    sharedImage :: ImageInfo -> B9 SharedImage
    sharedImage (ImageInfo name) = do
       buildId <- getBuildId
       date <- getBuildDate
       return (SharedImage (SharedImageName name)
                        (SharedImageDate date)
                        (SharedImageBuildId buildId)
                        info)

getSharedImagesCacheDir :: B9 FilePath
getSharedImagesCacheDir = do
  cacheDir <- localRepoDir <$> getRepoCache
  return (cacheDir </> sharedImagesRootDirectory)

-- | 'SharedImage' holds all data necessary to identify an image shared.
data SharedImage = SharedImage SharedImageName
                               SharedImageDate
                               SharedImageBuildId
                               ImageInfo
  deriving (Read,Show)
newtype SharedImageName = SharedImageName String deriving (Read,Show)
newtype SharedImageDate = SharedImageDate String deriving (Read,Show)
newtype SharedImageBuildId = SharedImageBuildId String deriving (Read,Show)

-- | Return the disk image of an sharedImage
sharedImageImage :: SharedImage -> Image
sharedImageImage (SharedImage (SharedImageName n)
                              _
                              (SharedImageBuildId bid)
                              _) =
  Image (n ++ "_" ++ bid <.> show sharedImageType) sharedImageType

-- | Calculate the path to the text file holding the serialized `ImageInfo`
-- relative to the directory of shared images in a repository.
sharedImageFileName :: SharedImage -> FilePath
sharedImageFileName (SharedImage (SharedImageName n)
                                     _
                                     (SharedImageBuildId bid) _) =
  n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension  = "b9bi"

sharedImageType :: ImageType
sharedImageType = QCow2
