{-# LANGUAGE ScopedTypeVariables #-}
module B9.Dsl.PartitionedVmImage where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Dsl.Core
import B9.Dsl.File
import B9.PartitionTable
import Data.Singletons.TH

$(singletons
      [d|

  data PartitionedVmImage = PartitionedVmImage
                          deriving Show
  |])

type instance IoCompilerArtifactState 'PartitionedVmImage =
     Handle 'FreeFile

instance CanExtract IoCompiler 'FreeFile 'PartitionedVmImage where
    runExtract hnd@(Handle _ hndT) _ () = do
        let partVmImgHndT = hndT ++ "-partitioned-vm-image"
        (partVmImgHnd,_) <- allocHandle SPartitionedVmImage partVmImgHndT
        file <- runExtract hnd SFreeFile (Just "partitioned-vm-image")
        putArtifactState partVmImgHnd file
        hnd --> partVmImgHnd
        return partVmImgHnd

instance CanExtract IoCompiler 'PartitionedVmImage 'FreeFile where
    type ExtractionArg IoCompiler 'PartitionedVmImage 'FreeFile = PartitionSpec
    runExtract hnd@(Handle _ hndT) _ partSpec@(MBRPartition pIndex) = do
        let dest = hndT ++ "-partition-" ++ show pIndex
        Just (srcFileH :: Handle 'FreeFile) <- useArtifactState hnd
        Just (FileCtx srcFileName _) <- useArtifactState srcFileH
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction
            hnd
            (liftIoProgram (extractPartition partSpec srcFileName destFile))
        return destH
