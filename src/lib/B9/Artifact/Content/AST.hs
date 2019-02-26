{-|

B9 produces not only VM-Images but also text documents such as configuration
files required by virtual machines. This module is about creating and merging
files containing parsable syntactic structures, such as most configuration files
do.

B9 can be used to create configuration files by assembling structured documents,
for example Yaml, JSON, Erlang Terms.

One example is creating a single cloud-init 'user-data' file from a set of
'user-data' snippets - all of which using yaml syntax to declare the same
object (e.g @"user-data"@).

The goal is, that b9 is able to merge these snippets into one, intelligently
merging fields as one would expect, e.g. when merging multiple snippets with
@writefiles@ fields, the output object's @writefiles@ field contains all the
@write_file@ objects.

Another example is the OTP/Erlang sys.config for configuring OTP/Erlang releases.
-}

module B9.Artifact.Content.AST ( FromAST(..)
                      , AST(..)
                      , decodeOrFail'
                      ) where

import           Control.Parallel.Strategies
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.Data
import           Data.Hashable
import           GHC.Generics (Generic)
import           Test.QuickCheck

import           B9.Artifact.Content.StringTemplate
import           B9.Artifact.Content
import           B9.QCUtil

-- | Parse a bytestring into an 'a', and return @Left errorMessage@ or @Right a@
-- This is like 'Binary.decodeOrFail' exception that it allows to add and extra
-- error message
decodeOrFail'
    :: Binary a
    => String -- ^ An arbitrary string for error messages
    -> Lazy.ByteString
    -> Either String a
decodeOrFail' errorMessage b =
  case Binary.decodeOrFail b of
    Left (_,_,e) -> Left (unwords [errorMessage, e])
    Right (_,_,a) -> Right a

-- | Describe how to create structured content that has a tree-like syntactic
-- structure, e.g. yaml, JSON and erlang-proplists. The first parameter defines
-- a /context/ into which the 'AST' is embedded,
-- e.g. B9.Artifact.Content'. The second parameter defines a specifix
-- syntax, e.g 'B9.Artifact.Content.ErlangPropList' that the 'AST' value generates.
data AST c a
    = ASTObj [(String, AST c a)] -- ^ Create an object similar to a
                                 -- Json object.
    | ASTArr [AST c a] -- ^ An array.
    | ASTMerge [AST c a] -- ^ Merge the nested elements, this is a very
                         -- powerful tool that allows to combine
    |
      -- several inputs in a smart and safe way,
      -- e.g. by merging the values of the same
      -- fields in yaml objects.
      ASTEmbed c -- Embed more impure content.
    | ASTString String -- A string literal.
    | ASTInt Int -- An Int literal.
    | ASTParse SourceFile -- An 'AST' obtained from parsing a source
                          -- file that contains a string corresponding
    |
                -- to the type parameter @a@, e.g. 'YamlObject's
      AST a -- Embed a literal @a@.
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Functor (AST c) where
  fmap f (AST a)          = AST (f a)
  fmap f (ASTObj x)       = ASTObj ((fmap . fmap . fmap) f x)
  fmap f (ASTArr x)       = ASTArr ((fmap . fmap) f x)
  fmap f (ASTMerge x)     = ASTMerge ((fmap . fmap) f x)
  fmap _ (ASTEmbed x)     = ASTEmbed x
  fmap _ (ASTString x)    = ASTString x
  fmap _ (ASTInt x)       = ASTInt x
  fmap _ (ASTParse x)     = ASTParse x

instance (Hashable c, Hashable a) => Hashable (AST c a)
instance (Binary c, Binary a) => Binary (AST c a)
instance (NFData c, NFData a) => NFData (AST c a)

-- | Types of values that describe content, that can be created from an 'AST'.
class FromAST a  where
    fromAST
        :: (ToContentGenerator c)
        => AST c a -> ContentGenerator a

instance (Arbitrary c, Arbitrary a) => Arbitrary (AST c a) where
    arbitrary =
        oneof
            [ ASTObj <$> smaller (listOf ((,) <$> arbitrary <*> arbitrary))
            , ASTArr <$> smaller (listOf arbitrary)
            , ASTMerge <$>
              sized
                  (\s ->
                        resize (max 2 s) (listOf (halfSize arbitrary)))
            , ASTEmbed <$> smaller arbitrary
            , ASTString <$> arbitrary
            , ASTParse <$> smaller arbitrary
            , AST <$> smaller arbitrary]
