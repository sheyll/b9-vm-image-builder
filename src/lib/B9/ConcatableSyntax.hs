module B9.ConcatableSyntax (ConcatableSyntax(..)
                           ,concatSources) where

import qualified Data.ByteString as B
import Data.Semigroup

-- | Imagine you would want to create a cloud-init 'user-data' file from a set
-- of 'user-data' snippets which each are valid 'user-data' files in yaml syntax
-- and e.g. a 'writefiles' section. Now the goal is, for b9 to be able to merge
-- these snippets into one, such that all writefiles sections are combined into
-- a single writefile section. Another example is OTP/Erlang sys.config files.
-- This type class is the greatest commonon denominator of types describing a
-- syntax that can be parsed, concatenated e.g. like in the above example and
-- rendered. The actual concatenation operation is the append from Monoid,
-- i.e. like monoid but without the need for an empty element.
class Semigroup a => ConcatableSyntax a where
  decodeSyntax :: B.ByteString -> Either String a
  encodeSyntax :: a -> B.ByteString


instance ConcatableSyntax B.ByteString where
  decodeSyntax = Right
  encodeSyntax = id

concatSources :: (Semigroup a, ConcatableSyntax a) => [B.ByteString] -> Either String a
concatSources srcs = mapM decodeSyntax srcs >>= return . (foldl1 (<>))
