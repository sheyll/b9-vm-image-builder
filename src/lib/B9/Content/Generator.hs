{-| The basic data structure that ties together syntax trees making them
    composable and addressable in B9 artifacts. -}
module B9.Content.Generator where

import           Control.Parallel.Strategies
import           Data.Binary
import           Data.Data
import           Data.Hashable
import           GHC.Generics (Generic)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           B9.Content.AST
import           B9.Content.ErlangPropList
import           B9.Content.StringTemplate
import           B9.Content.YamlObject
import qualified Data.ByteString.Char8 as B

import           Test.QuickCheck
import           B9.QCUtil

data Content
    = RenderErlang (AST Content ErlangPropList)
    | RenderYaml (AST Content YamlObject)
    | FromString String
    | FromTextFile SourceFile
    deriving (Read,Show,Typeable,Eq,Data,Generic)

instance Hashable Content
instance Binary Content
instance NFData Content

instance Arbitrary Content where
  arbitrary = oneof [FromTextFile <$> smaller arbitrary
                    ,RenderErlang <$> smaller arbitrary
                    ,RenderYaml <$> smaller arbitrary
                    ,FromString <$> smaller arbitrary]

instance CanRender Content where
  render (RenderErlang ast) = encodeSyntax <$> fromAST ast
  render (RenderYaml ast) = encodeSyntax <$> fromAST ast
  render (FromTextFile s) = readTemplateFile s
  render (FromString str) = return (B.pack str)
