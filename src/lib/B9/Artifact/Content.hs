-- | Content is what is written to files in the generated VM images and cloud configuration.
--
-- Contains the monadic actions that generate the content that
-- is written to the generated artifacts.
--
-- @since 0.5.62
module B9.Artifact.Content
  ( ByteStringGenerator
  , ToContentGenerator(..)
  )
where

import           B9.B9Monad
import           Control.Eff
import           Data.ByteString.Lazy          as Lazy

-- | A 'B9' action that procuces a 'Lazy.ByteString'.
--
-- @since 0.5.62
type ByteStringGenerator = B9 Lazy.ByteString

-- | Types whose values can be turned into a 'ContentGenerator'
--
-- @since 0.5.62
class ToContentGenerator c a where
    toContentGenerator :: IsB9 e => c -> Eff e a
