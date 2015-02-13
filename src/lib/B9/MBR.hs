module B9.MBR ( getPartition
              , PrimaryPartition (..)
              , MBR(..)
              , CHS(..)) where

import Control.Applicative
import Data.Binary.Get
import Data.Word
import Text.Printf
import qualified Data.ByteString.Lazy as BL

getPartition :: Int -> FilePath -> IO (Word64, Word64)
getPartition n f = decodeMBR <$> BL.readFile f
  where
    decodeMBR input =
      let mbr = runGet getMBR input
          part = (case n of
                   1 -> mbrPart1
                   2 -> mbrPart2
                   3 -> mbrPart3
                   4 -> mbrPart4
                   b -> error (printf "Error: Invalid partition index %i\
                                     \ only partitions 1-4 are allowed.\
                                     \ Image file: '%s'"
                                     b
                                     f))
                 mbr
          start = fromIntegral (primPartLbaStart part)
          len = fromIntegral (primPartSectors part)
      in (start * sectorSize, len * sectorSize)

sectorSize :: Word64
sectorSize = 512

bootCodeSize :: Int
bootCodeSize = 446

data MBR = MBR { mbrPart1 :: !PrimaryPartition
               , mbrPart2 :: !PrimaryPartition
               , mbrPart3 :: !PrimaryPartition
               , mbrPart4 :: !PrimaryPartition
               } deriving Show

data PrimaryPartition = PrimaryPartition { primPartStatus :: !Word8
                                         , primPartChsStart :: !CHS
                                         , primPartPartType :: !Word8
                                         , primPartChsEnd :: !CHS
                                         , primPartLbaStart :: !Word32
                                         , primPartSectors :: !Word32
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
