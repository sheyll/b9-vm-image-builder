-- Instances of 'B9.ConcatableSyntax' for some commonly used
-- syntax types.
module B9.PropLists (YamlPList (..)) where

import Data.Yaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict
import Control.Applicative
import Text.Show.Pretty
import Data.Vector ((++))
import Prelude hiding ((++))
import Data.Semigroup

import B9.ErlTerms
import B9.ConcatableSyntax

newtype YamlPList = YamlPList Data.Yaml.Value
  deriving (Eq,Show)

instance Semigroup YamlPList where
  (YamlPList (Object o1)) <> (YamlPList (Object o2)) =
    YamlPList (Object (unionWith combine o1 o2))
    where
      combine (Array a1) (Array a2) = Array (a1 ++ a2)
      combine _ v2 = v2

instance ConcatableSyntax YamlPList where
  decodeSyntax str = do
    o <- decodeEither str
    return (YamlPList o)

  encodeSyntax (YamlPList o) = encode o
