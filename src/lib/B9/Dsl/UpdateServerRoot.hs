{-# LANGUAGE ScopedTypeVariables #-}

module B9.Dsl.UpdateServerRoot where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.Repository
import Data.Singletons.TH

$(singletons
    [d|

  data UpdateServerRoot = UpdateServerRoot
                        deriving Show
  |])

instance Show (Sing 'UpdateServerRoot) where show _ = "UpdateServerRoot"

type instance IoCompilerArtifactState 'UpdateServerRoot = Handle 'LocalDirectory

instance CanAdd IoCompiler 'UpdateServerRoot 'VmImage where
  type AddSpec IoCompiler 'UpdateServerRoot 'VmImage = (SharedImageName, Handle 'VmImage)
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
                        writeContentToFile sizeFile (showB imgSize)
                        bId <- B9.B9IO.getBuildId
                        bT <- B9.B9IO.getBuildDate
                        writeContentToFile versionFile
                                           (packB (printf "%s-%s" bId bT))))

instance CanExtract IoCompiler 'LocalDirectory 'UpdateServerRoot where
  runExtract destDirH _ () =
    do (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
       hnd --> destDirH
       putArtifactState hnd destDirH
       return hnd
