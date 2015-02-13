module B9.Content.AST ( ConcatableSyntax (..)
                      ) where

import qualified Data.ByteString as B
import Data.Semigroup
import Data.Data
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader

import B9.Content.StringTemplate

-- | Imagine you would want to create a cloud-init 'user-data' file from a set
-- of 'user-data' snippets which each are valid 'user-data' files in yaml syntax
-- and e.g. a 'writefiles' section. Now the goal is, for b9 to be able to merge
-- these snippets into one, such that all writefiles sections are combined into
-- a single writefile section. Another example is OTP/Erlang sys.config files.
-- This type class is the greatest commonon denominator of types describing a
-- syntax that can be parsed, concatenated e.g. like in the above example and
-- rendered. The actual concatenation operation is the append from Monoid,
-- i.e. like monoid but without the need for an empty element.
class (Monoid a) => ConcatableSyntax a where
  decodeSyntax :: FilePath -> B.ByteString -> Either String a
  encodeSyntax :: a -> B.ByteString

instance ConcatableSyntax B.ByteString where
  decodeSyntax _ = Right
  encodeSyntax   = id

data AST c a = ASTObj [(String, AST c a)]
             | ASTArr [AST c a]
             | ASTMerge [AST c a]
             | ASTEmbed c
             | ASTString String
             | ASTParse SourceFile
             | AST a
             | ASTNoOp
  deriving (Read, Show, Typeable, Data, Eq)

class (ConcatableSyntax a) => ASTish a where
  fromAST :: (Applicative m, Monad m, MonadIO m, MonadReader Environment m) => AST c a -> m a
  encodeAST :: (Applicative m, Monad m, MonadIO m, MonadReader Environment m) => AST c a -> m B.ByteString
