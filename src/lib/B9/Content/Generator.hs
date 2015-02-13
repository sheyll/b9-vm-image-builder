module B9.Content.Generator where

import Data.Data
import Control.Applicative

import B9.Content.AST
import B9.Content.ErlangPropList
import B9.Content.YamlObject
import B9.Content.StringTemplate

import Test.QuickCheck
import B9.QCUtil

data Content = RenderErlang (AST Content ErlangPropList)
             | RenderYaml (AST Content YamlObject)
             | FromTextFile SourceFile
  deriving (Read, Show, Typeable, Eq)

instance Arbitrary Content where
  arbitrary = oneof [FromTextFile <$> smaller arbitrary
                    ,RenderErlang <$> smaller arbitrary
                    ,RenderYaml <$> smaller arbitrary]

instance CanRender Content where
  render (RenderErlang ast) = encodeSyntax <$> fromAST ast
  render (RenderYaml ast) = encodeSyntax <$> fromAST ast
  render (FromTextFile s) = readTemplateFile s
