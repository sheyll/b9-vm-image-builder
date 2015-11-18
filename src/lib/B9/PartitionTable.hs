{-| Function to find the file offsets of primary partitions in raw disk
    images. Currently only MBR partitions are supported. See 'B9.MBR' -}
module B9.PartitionTable where

import           B9.Logging
import qualified B9.MBR as MBR
import           Control.Parallel.Strategies
import           Data.Binary
import           Data.Data
import           Data.Hashable
import           GHC.Generics (Generic)
import           Test.QuickCheck

-- | Extract the start offset and length of a given partition inside a raw image
-- file in bytes.
getPartition :: PartitionSpec -> FilePath -> IO (Word64, Word64)
getPartition (MBRPartition partitionIndex) diskImage =
    MBR.getPartition partitionIndex diskImage

-- | The partition to extract.
data PartitionSpec =
    MBRPartition Int
    deriving (Eq,Show,Read,Typeable,Data,Generic)

instance Hashable PartitionSpec
instance Binary PartitionSpec
instance NFData PartitionSpec
instance LogArg PartitionSpec

-- * QuickCheck instances

instance Arbitrary PartitionSpec where
    arbitrary = MBRPartition <$> elements [0, 1, 2]
