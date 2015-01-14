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

-- List BIs: rsync fetch only the baseimage infos (=> need file pattern for this)
listBaseImagesFromCache :: B9 [BaseImageInfo]
listBaseImagesFromCache = do
  repoCacheDir <- getRepositoryCache
  --findFiles
  return []
  -- getBI: Fetch a BI from Repo and provide a local Image


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
  directoryToUpload <- createBaseImage buildImg baseImg
  publishDirectoryRecursive cacheRepo directoryToUpload
  infoL (printf "CACHED BASE IMAGE '%s'" (show baseImg))
  publishRepo <- getRepository
  when (isJust publishRepo) $ do
    let Repository publishRepoId _ = fromJust publishRepo
    publishDirectoryRecursive (fromJust publishRepo) directoryToUpload
    infoL (printf "PUBLISHED BASE IMAGE '%s' TO '%s'" (show baseImg)
                                                       publishRepoId)
