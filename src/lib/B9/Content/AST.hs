{-|

B9 produces not only VM-Images but also text documents such as configuration
files required by virtual machines. This module is about creating and merging
files containing parsable syntactic structures, such as most configuration files
do.

Imagine you would want to create a cloud-init 'user-data' file from a set of
'user-data' snippets which each are valid 'user-data' files in yaml syntax and
e.g. a 'write_files' section. Now the goal is, for b9 to be able to merge these
snippets into one, such that all writefiles sections are combined into a single
writefile section. Another example is OTP/Erlang sys.config files.  This type
class is the greatest commonon denominator of types describing a syntax that can
be parsed, concatenated e.g. like in the above example and rendered. The actual
concatenation operation is the append from Monoid, i.e. like monoid but without
the need for an empty element.
-}

module B9.Content.AST ( ConcatableSyntax (..)
                      , ASTish(..)
                      , AST(..)
                      , CanRender(..)
                      , astMerge
                      ) where

import           Control.Parallel.Strategies
import           Data.Binary
import qualified Data.ByteString as B
import           Data.Data
import           Data.Hashable
import           Data.Semigroup
import           GHC.Generics (Generic)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           B9.Content.StringTemplate

import           Test.QuickCheck
import           B9.QCUtil

-- | Types of values that can be parsed/rendered from/to 'ByteString's. This
-- class is used as basis for the 'ASTish' class.
class (Semigroup a) => ConcatableSyntax a  where
    -- Parse a bytestring into an 'a', and return @Left errorMessage@ or @Right a@
    decodeSyntax
        :: FilePath -- ^ An arbitrary string for error messages that
        -> B.ByteString -- ^ The raw input to parse
        -> Either String a
    -- Generate a string representation of @a@
    encodeSyntax
        :: a -> B.ByteString

instance ConcatableSyntax B.ByteString where
    decodeSyntax _ = Right
    encodeSyntax = id

-- | Describe how to create structured content that has a tree-like syntactic
-- structure, e.g. yaml, JSON and erlang-proplists. The first parameter defines
-- a /context/ into which the 'AST' is embeded,
-- e.g. B9.Content.Generator.Content'. The second parameter defines a specifix
-- syntax, e.g 'B9.Content.ErlangPropList' that the 'AST' value generates.
data AST c a
    = ASTObj [(String, AST c a)]
      -- ^ Create an object similar to a Json object.
    | ASTArr [AST c a]
      -- ^ An array.
    | ASTMerge [AST c a]
      -- ^ Merge the nested elements, this is a very powerful tool that allows
      -- to combine several inputs in a smart and safe way, e.g. by merging the
      -- values of the same fields in yaml objects.
    | ASTEmbed c
      -- ^ Embed some pure content.
    | ASTString String
      -- ^ A string literal.
    | ASTParse SourceFile
      -- ^ An 'AST' obtained from parsing a source file that contains a string
      -- corresponding to the type parameter @a@, e.g. 'YamlObject's
    | AST a
      -- ^ Embed a literal @a@.
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance (Hashable c, Hashable a) => Hashable (AST c a)
instance (Binary c, Binary a) => Binary (AST c a)
instance (NFData c, NFData a) => NFData (AST c a)

-- | Merge two 'AST's.
astMerge :: AST c a -> AST c a -> AST c a
astMerge (ASTMerge l) (ASTMerge r) = ASTMerge (l ++ r)
astMerge (ASTMerge l) r = ASTMerge (l ++ [r])
astMerge l (ASTMerge r) = ASTMerge (l:r)
astMerge l r = ASTMerge [l,r]

-- | Types of values that describe content, that can be created from an 'AST'.
class (ConcatableSyntax a) => ASTish a  where
    fromAST
        :: (CanRender c, Applicative m, Monad m, MonadIO m, MonadReader Environment m)
        => AST c a -> m a

-- | Types of values that can be /rendered/ into a 'ByteString'
class CanRender c  where
    render
        :: (Functor m, Applicative m, MonadIO m, MonadReader Environment m)
        => c -> m B.ByteString

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
