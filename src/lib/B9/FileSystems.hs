-- | A module for general file system creation and resize.
module B9.FileSystems where

import B9.CommonTypes
import B9.QCUtil
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
import Test.QuickCheck

-- | Descibe how a 'FileSystem' should be created.
data FileSystemSpec =
    FileSystemSpec FileSystem
                   FSLabel
                   Int
                   SizeUnit
    deriving (Read,Show,Generic,Eq,Data,Typeable)

type FSLabel = String

instance Hashable FileSystemSpec
instance Binary FileSystemSpec
instance NFData FileSystemSpec

-- | The file systems that b9 can use and convert. TODO move to FileSystems
data FileSystem
    = NoFileSystem
    | Ext4
    | ISO9660
    | VFAT
    deriving (Eq,Show,Read,Typeable,Data,Generic)

instance Hashable FileSystem
instance Binary FileSystem
instance NFData FileSystem
instance CoArbitrary FileSystem

-- | How to resize a file system.
data FileSystemResize
    =
      -- | Resize an image and the contained file system.
      FileSystemResize Int SizeUnit
    |
      -- | Resize a file system and the contained file system to the
      -- smallest size to fit the contents of the file system.
      ShrinkFileSystem
    deriving (Eq,Show,Read,Typeable,Data,Generic)

instance Hashable FileSystemResize
instance Binary FileSystemResize
instance NFData FileSystemResize

-- * 'Arbitrary' instances for quickcheck

instance Arbitrary FileSystemSpec where
    arbitrary =
        FileSystemSpec <$> smaller arbitrary <*> smaller arbitrary <*>
        smaller arbitrary <*>
        smaller arbitrary

instance Arbitrary FileSystemResize where
    arbitrary =
        oneof
            [ FileSystemResize <$> smaller arbitrary <*> smaller arbitrary
            , pure ShrinkFileSystem]

instance Arbitrary FileSystem where
    arbitrary = elements [Ext4, ISO9660, VFAT]
