{-| Function to find the file offsets of primary partitions in raw disk
    images. Currently only MBR partitions are supported. See 'B9.MBR' -}
module B9.PartitionTable ( getPartition ) where

import Control.Applicative
import Data.Word ( Word64 )

import qualified B9.MBR as MBR

getPartition :: Int -> FilePath -> IO (Word64, Word64, Word64)
getPartition partitionIndex diskImage =
  blockSized <$> MBR.getPartition partitionIndex diskImage

blockSized :: (Integral a) => (a, a) -> (a, a, a)
blockSized (s, l) = let bs = gcd2 1 s l
                    in (s `div` bs, l `div` bs, bs)
  where
    gcd2 n x y = let next = 2 * n in
                  if x `rem` next == 0 && y `rem` next == 0
                  then gcd2 next x y
                  else n
