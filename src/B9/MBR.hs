module B9.MBR ( getPartition ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy as BL
import System.IO

getPartition :: Int -> FilePath -> IO (Word64, Word64)
getPartition n f = decodeMBR <$> BL.readFile f
  where
    decodeMBR input =
      let mbr = runGet getMBR input
          part = (case n of
                   1 -> part1
                   2 -> part2
                   3 -> part3
                   4 -> part4) mbr
          start = fromIntegral (lbaStart part)
          len = fromIntegral (sectors part)
      in (start * sectorSize, len * sectorSize)

sectorSize = 512

bootCodeSize = 446

data MBR = MBR { part1 :: !PrimaryPartition
               , part2 :: !PrimaryPartition
               , part3 :: !PrimaryPartition
               , part4 :: !PrimaryPartition
               } deriving Show

data PrimaryPartition = PrimaryPartition { status :: !Word8
                                         , chsStart :: !CHS
                                         , partType :: !Word8
                                         , chsEnd :: !CHS
                                         , lbaStart :: !Word32
                                         , sectors :: !Word32
                                         } deriving Show

data CHS = CHS { chsH :: !Word8
               , chs_CUpper2_S :: !Word8
               , chs_CLower8 :: !Word8
               } deriving Show

getMBR :: Get MBR
getMBR = skip bootCodeSize >>
         MBR <$> getPart
             <*> getPart
             <*> getPart
             <*> getPart

getPart :: Get PrimaryPartition
getPart = PrimaryPartition <$> getWord8
                           <*> getCHS
                           <*> getWord8
                           <*> getCHS
                           <*> getWord32le
                           <*> getWord32le

getCHS :: Get CHS
getCHS = CHS <$> getWord8
             <*> getWord8
             <*> getWord8
