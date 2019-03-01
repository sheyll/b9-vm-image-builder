-- | Content is what is written to files in the generated VM images and cloud configuration.
--
-- Contains the monadic actions that generate the content that
-- is written to the generated artifacts.
--
-- @since 0.5.62
module B9.Artifact.Content
  ( IsContentGenerator
  , runContentGenerator
  , ContentGenerator
  , ByteStringGenerator
  , renderContentGenerator
  , ToContentGenerator(..)
  )
where

import           B9.B9Error
import           B9.B9Monad                     ( B9 )
import           B9.Environment
import           Control.Eff
import           Control.Exception              ( SomeException )
import           Data.ByteString.Lazy          as Lazy

-- | A monadic action that generates content by using the 'Environment'
-- as additional input, e.g. when interpolating string templates.
--
-- @since 0.5.64
type IsContentGenerator e
  = (Lifted B9 e, Member EnvironmentReader e, Member ExcB9 e)

-- | Execute a 'ContentGenerator'.
--
-- @since 0.5.64
runContentGenerator
  :: Environment -> ContentGenerator a -> B9 (Either SomeException a)
runContentGenerator e = runLift . runExcB9 . runEnvironmentReader e

-- | A 'B9' action that generates content by using the 'Environment'
-- as additional input, e.g. when interpolating string templates.
--
-- @since 0.5.64
type ContentGenerator a = Eff '[EnvironmentReader, ExcB9, Lift B9] a

-- | A 'B9' action that procuces a 'Lazy.ByteString'.
--
-- @since 0.5.62
type ByteStringGenerator = ContentGenerator Lazy.ByteString

-- | Types whose values can be turned into a 'ContentGenerator'
--
-- @since 0.5.62
class ToContentGenerator c a where
    toContentGenerator :: IsContentGenerator e => c -> Eff e a

-- | Convert and execute a 'ContentGenerator'.
-- This combines 'ToContentGenerator' and 'runContentGenerator'.
-- The 'Environment' contains the bindings of any string template variables.
--
-- @since 0.5.64
renderContentGenerator :: ToContentGenerator c a => Environment -> c -> B9 a
renderContentGenerator e =
  runLift . errorOnException . runEnvironmentReader e . toContentGenerator
