module B9.BaseImageBuilder (publishBaseImage
                           ,BaseImageInfo(..)) where

import B9.B9Monad
import B9.BaseImages
import B9.ConfigUtils
import B9.DiskImages
import B9.DiskImageBuilder
import B9.Repository
import B9.RepositoryIO

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Format
import System.FilePath ((</>),(<.>))
import Text.Printf (printf)
import Text.Show.Pretty (ppShow)

data BaseImageInfo = BaseImageInfo { bifBaseImage :: BaseImage
                                   , bifImgLoc :: FilePath
                                   , bifImgType :: ImageType
                                   , bifBuildDate :: String
                                   , bifBuildId :: String
                                   , bifV2 :: [()] }
                     deriving (Read,Show)

baseImageRootDirectory = "b9_base_images"
baseImageInfoFile = "BaseImageInfo"
baseImageType = QCow2

createBaseImage :: Image -> BaseImage -> B9 FilePath
createBaseImage buildImg baseImg@(BaseImage biName) = do
  buildId <- getBuildId
  buildDir <- getBuildDir
  now <- liftIO getCurrentTime
  let buildDate = formatTime undefined "%F-%T" now
      biRoot = baseImageRootDirectory </> biName
      imgRepoFile = buildId <.> show baseImageType
      imgRepoPath = biRoot </> imgRepoFile
      uploadImgPath = buildDir </> imgRepoPath
      uploadImg = Image uploadImgPath baseImageType
      infoRepoFile = baseImageInfoFile
      infoRepoPath = biRoot </> infoRepoFile
      infoUploadPath = buildDir </> infoRepoPath
      info = BaseImageInfo baseImg imgRepoFile baseImageType buildDate buildId []
  dbgL (printf "WRITE '%s' FOR BASE IMAGE UPLOAD: \n'%s'"
                infoUploadPath
                (ppShow info))
  tell infoUploadPath info
  dbgL (printf "CONVERT '%s' TO '%s' FOR BASE IMAGE UPLOAD"
               (show buildImg)
               (show uploadImg))
  convert True buildImg uploadImg
  return (buildDir </> baseImageRootDirectory)

publishBaseImage :: Image -> BaseImage -> B9 ()
publishBaseImage buildImg baseImg = do
  cacheRepo <- getRepositoryCache
  publishRepo <- getRepository
  let anyWorkRequired = isJust cacheRepo || isJust publishRepo
      (Just (Repository publishRepoId _)) = publishRepo
      (Just (Repository cacheRepoId _)) = cacheRepo
  when anyWorkRequired $ do
    directoryToUpload <- createBaseImage buildImg baseImg
    when (isJust cacheRepo) $ do
      publishDirectoryRecursive (fromJust cacheRepo) directoryToUpload
      infoL (printf "CACHED BASE IMAGE '%s' INTO '%s'" (show baseImg)
                                                       cacheRepoId)
    when (isJust publishRepo) $ do
      publishDirectoryRecursive (fromJust publishRepo) directoryToUpload
      infoL (printf "PUBLISHED BASE IMAGE '%s' TO '%s'" (show baseImg)
                                                         publishRepoId)
