module B9.GPT where

import           Control.Lens.TH
import           Data.Word

{-
Structure of GTP for an set of images;

The total size of the GTP is the smallest
-}

type Signature = (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
type Revision = Word32
type LBA = Word64
type Size = Word32

data GptHeader = GptHeader { _signature              :: !Signature
                           , _revision               :: !Revision
                           , _headerSize             :: !Size
                           , gptHeaderReserved1      :: !Word32
                           , _currentLba             :: !LBA
                           , _backupLba              :: !LBA
                           , _partitionTableFirstLba :: !LBA
                           , _partitionTableLastLba  :: !LBA
                           }
