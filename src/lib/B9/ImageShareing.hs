module B9.ImageShareing (shareImage
                        ,getSharedImages) where

import B9.B9Monad
import B9.ConfigUtils
import B9.DiskImages
import B9.DiskImageBuilder
import B9.Repository
import B9.RepositoryIO

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
shareImage :: Image -> ImageInfo -> B9 ()
shareImage buildImg info = do
  artRoot <- assembleSharedImageInBuildDir buildImg info
  infoL (printf "ASSEMBLED '%s' FOR SHARING" (show info))
  publishDirectoryRecursive artRoot

-- | Return a list of all existing sharedImages from the repository.
getSharedImages :: B9 [SharedImage]
getSharedImages = do
  sharedImagePaths <- repoSearch sharedImagesRootDirectory
                                 (FileNameEndsWith sharedImageFileExtension)
  mapM consult sharedImagePaths

-- * Internals:

-- | Convert the disk image and serialize the base image data structure.
assembleSharedImageInBuildDir :: Image -> ImageInfo -> B9 FilePath
assembleSharedImageInBuildDir buildImg info = do
  art <- sharedImage info
  buildDir <- getBuildDir
  let sharedImageImg = changeImageDirectory sharedImageDir (sharedImageImage art)
      sharedImageFile = sharedImageDir </> sharedImageFilePath art
      sharedImageDir = buildDir </> sharedImagesRootDirectory
  convertImage buildImg sharedImageImg
  tell sharedImageFile art
  return sharedImageDir
  where
    sharedImage :: ImageInfo -> B9 SharedImage
    sharedImage (ImageInfo name) = do
       buildId <- getBuildId
       date <- getBuildDate
       return (SharedImage (SharedImageName name)
                        (SharedImageDate date)
                        (SharedImageBuildId buildId)
                        info)

-- | `Info` hold all data necessary to identify file locations of a SharedImage's
-- sharedImages.
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
-- relative to the root of a repository.
sharedImageFilePath :: SharedImage -> FilePath
sharedImageFilePath (SharedImage (SharedImageName n)
                                 _
                                 (SharedImageBuildId bid) _) =
  n ++ "_" ++ bid <.> sharedImageFileExtension

sharedImagesRootDirectory :: FilePath
sharedImagesRootDirectory = "b9_shared_images"

sharedImageFileExtension :: String
sharedImageFileExtension  = "b9bi"

sharedImageType :: ImageType
sharedImageType = QCow2
