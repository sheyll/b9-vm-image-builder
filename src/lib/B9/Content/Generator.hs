module B9.Content.Generator
  ( ContentGenerator(MkContentGenerator)
  , ByteStringGenerator
  , renderContentGenerator
  , CanRender (..)
  ) where

import B9.B9Monad (B9)
import B9.Content.Environment
import Control.Applicative (Alternative)
import Control.Monad.Reader
import Data.ByteString.Lazy as Lazy

newtype ContentGenerator a = MkContentGenerator
  { getContentGenerator :: EnvironmentReaderT B9 a
  } deriving (Alternative, MonadReader Environment, MonadIO, Monad, Applicative, Functor)

type ByteStringGenerator = ContentGenerator Lazy.ByteString

instance Semigroup a => Semigroup (ContentGenerator a) where
  l <> r = (<>) <$> l <*> r

instance Monoid a => Monoid (ContentGenerator a) where
  mempty = pure mempty


-- | Types of values that can be /rendered/ into a 'ByteString'
class CanRender c  where
    render
        :: c -> EnvironmentReaderT B9 Lazy.ByteString

renderContentGenerator :: Environment -> ContentGenerator a -> B9 a
renderContentGenerator e = runEnvironmentReaderT e . getContentGenerator