-- | Monadic actions that generate the content that
-- is written to the generated artifacts.
--
-- @since 0.5.62
module B9.Content.Generator
  ( ContentGeneratorT(MkContentGeneratorT)
  , ContentGenerator
  , ByteStringGenerator
  , renderContentGenerator
  , ToContentGenerator (..)
  ) where

import B9.B9Monad (B9)
import B9.Environment
import Control.Applicative (Alternative)
import Control.Monad.Reader
import Data.ByteString.Lazy as Lazy

-- | A monadic action that generates content by using the 'Environment'
-- as additional input, e.g. when interpolating string templates.
--
-- Most intersting is the fact the 'Semigroup' and 'Monoid' instances
-- are available.
--
-- @since 0.5.62
newtype ContentGeneratorT m a = MkContentGeneratorT
  { getContentGenerator :: EnvironmentReaderT m a
  } deriving (MonadTrans, Alternative, MonadReader Environment, MonadIO, Monad, Applicative, Functor)

-- | A 'B9' action that generates content by using the 'Environment'
-- as additional input, e.g. when interpolating string templates.
--
-- @since 0.5.62
type ContentGenerator a = ContentGeneratorT B9 a

-- | A 'B9' action that procuces a 'Lazy.ByteString'.
--
-- @since 0.5.62
type ByteStringGenerator = ContentGenerator Lazy.ByteString

instance (Monad m, Semigroup a) => Semigroup (ContentGeneratorT m a) where
  l <> r = (<>) <$> l <*> r

instance (Monad m, Monoid a) => Monoid (ContentGeneratorT m a) where
  mempty = pure mempty

-- | Types whose values can be turned into a 'ContentGenerator'
--
-- @since 0.5.62
class ToContentGenerator c where
    toContentGenerator :: c -> ByteStringGenerator

-- | Execute a 'ContentGenerator'.
-- The 'Environment' contains the bindings of any string template variables.
--
-- @since 0.5.62
renderContentGenerator :: Environment -> ContentGenerator a -> B9 a
renderContentGenerator e = runEnvironmentReaderT e . getContentGenerator