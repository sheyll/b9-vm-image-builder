{-# LANGUAGE ScopedTypeVariables #-}
module B9.Dsl.PartitionedVmImage where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Dsl.Core
import B9.Dsl.File
import B9.PartitionTable
import Control.Monad.Trans
import Data.Singletons.TH

$(singletons
      [d|

  data PartitionedVmImage = PartitionedVmImage
                          deriving Show
  |])


type instance ConvSpec 'FreeFile 'PartitionedVmImage = ()

type instance ConvSpec 'PartitionedVmImage 'FreeFile =
     PartitionSpec

instance CanConvert IoCompiler 'FreeFile 'PartitionedVmImage where
    runConvert hnd@(Handle _ hndT) _ () = do
        let partVmImgHndT = hndT ++ "-partitioned-vm-image"
        (partVmImgHnd,_) <- allocHandle SPartitionedVmImage partVmImgHndT
        file <- runConvert hnd SFreeFile "partitioned-vm-image"
        putArtifactState partVmImgHnd file
        hnd --> partVmImgHnd
        return partVmImgHnd

instance CanConvert IoCompiler 'PartitionedVmImage 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ partSpec@(MBRPartition pIndex) = do
        let dest = hndT ++ "-partition-" ++ show pIndex
        Just (srcFileH :: Handle 'FreeFile) <- useArtifactState hnd
        Just (FileCtx srcFileName _) <- useArtifactState srcFileH
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction hnd (lift (extractPartition partSpec srcFileName destFile))
        return destH
