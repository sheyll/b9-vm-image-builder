{-# LANGUAGE ScopedTypeVariables #-}

module B9.Dsl.UpdateServerRoot where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.Repository
import Control.Lens
import Data.Singletons.TH
import System.FilePath
import Text.Printf

$(singletons
    [d|

  data UpdateServerRoot = UpdateServerRoot
                        deriving Show
  |])

type instance AddSpec 'UpdateServerRoot 'VmImage =
     (SharedImageName, Handle 'VmImage)

type instance ConvSpec 'LocalDirectory 'UpdateServerRoot = ()

type instance IoCompilerArtifactState 'UpdateServerRoot =
     Handle 'LocalDirectory

instance CanAdd IoCompiler 'UpdateServerRoot 'VmImage where
  runAdd hnd _ (sn,vmI) =
    do Just (destDirH :: Handle 'LocalDirectory) <- useArtifactState hnd
       Just tmpDirCtx <- useArtifactState destDirH
       let destDir = tmpDirCtx ^. dirTempDir
           vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
           SharedImageName snStr = sn
       Just (VmImgCtx srcFileH srcType) <- useArtifactState vmI
       srcFile <-
         freeFileTempCopy srcFileH
                          (Just snStr)
       vmI --> hnd
       addAction hnd
                 (liftIoProgram
                    (do let imgFile = vmDestDir </> "0.raw"
                            sizeFile = vmDestDir </> "0.size"
                            versionFile = vmDestDir </> "VERSION"
                        mkDir vmDestDir
                        if srcType /= Raw
                           then convertVmImage srcFile srcType imgFile Raw
                           else moveFile srcFile imgFile
                        imgSize <- B9.B9IO.readFileSize imgFile
                        renderContentToFile sizeFile
                                            (FromString (show imgSize))
                                            (Environment [])
                        bId <- B9.B9IO.getBuildId
                        bT <- B9.B9IO.getBuildDate
                        renderContentToFile versionFile
                                            (FromString (printf "%s-%s" bId bT))
                                            (Environment [])))

instance CanConvert IoCompiler 'LocalDirectory 'UpdateServerRoot where
  runConvert destDirH _ () =
    do (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
       hnd --> destDirH
       putArtifactState hnd destDirH
       return hnd
