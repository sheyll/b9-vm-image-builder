-- | Content I/O
module B9.Dsl.Content where

import B9.Content
import B9.DslCore
import B9.B9IO.DslCompiler
import Data.Singletons.TH

$(singletons [d|
  data GeneratedContent = GeneratedContent
                        deriving Show
  |])

type instance CreateSpec 'GeneratedContent = (Content, String)

type instance AddSpec 'GeneratedContent 'GeneratedContent = Content

instance CanCreate IoCompiler 'GeneratedContent where
    runCreate _ (c,title) = do
        (hnd,_) <- allocHandle SGeneratedContent title
        generatedContent . at hnd ?= c
        return hnd
