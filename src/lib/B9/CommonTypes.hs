module B9.CommonTypes where

import B9.Logging
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
import Test.QuickCheck

-- | Enumeration of size multipliers. The exact semantics may vary depending on
-- what external tools look at these. E.g. the size unit is convert to a size
-- parameter of the @qemu-img@ command line tool.
data SizeUnit
    = B
    | KB
    | MB
    | GB
    deriving (Eq,Show,Read,Ord,Typeable,Data,Generic)

instance Hashable SizeUnit
instance Binary SizeUnit
instance NFData SizeUnit
instance LogArg SizeUnit

-- * QuickCheck instances

instance Arbitrary SizeUnit where
    arbitrary = elements [B, KB, MB, GB]
