-- | Content is what is written to files in the generated VM images and cloud configuration.
--
-- Contains the monadic actions that generate the content that
-- is written to the generated artifacts.
--
-- @since 0.5.62
module B9.Artifact.Content
  ( ContentGenerator,
    ToContentGenerator (..),
    Text,
  )
where

import B9.B9Monad
import Control.Eff
import Data.Text (Text)
import GHC.Stack

-- | A 'B9' action that procuces a 'Text'.
--
-- @since 0.5.62
type ContentGenerator = B9 Text

-- | Types whose values can be turned into an 'Eff'ect that produces
-- 'Text', e.g. 'ContentGenerator'
--
-- @since 0.5.62
class ToContentGenerator c where
  toContentGenerator :: (HasCallStack, IsB9 e) => c -> Eff e Text
