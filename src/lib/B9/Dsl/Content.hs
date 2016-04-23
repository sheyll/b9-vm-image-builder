{-# LANGUAGE ScopedTypeVariables #-}

-- | Content I/O
module B9.Dsl.Content where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import B9.Dsl.File
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Data.Singletons.TH hiding ((%~))

$(singletons
    [d|

  data GeneratedContent = GeneratedContent
                        | StaticContent
                        deriving Show
  |])

type instance CreateSpec 'GeneratedContent = (Content, String)

type instance AddSpec 'GeneratedContent 'GeneratedContent = Content

type instance ConvSpec 'GeneratedContent 'FreeFile = Environment

type instance ConvSpec 'FreeFile 'GeneratedContent = ()

-- * Implementation
type GeneratedContentList = [GeneratedContentChunk]

data GeneratedContentChunk
  = PureContent Content
  | ContentFromIo (IoProgram B.ByteString)

type instance IoCompilerArtifactState 'GeneratedContent =
     [GeneratedContentChunk]

instance CanCreate IoCompiler 'GeneratedContent where
  runCreate _ (c,title) =
    do (hnd,_) <- allocHandle SGeneratedContent title
       putArtifactState hnd
                        [PureContent c]
       return hnd 

instance CanConvert IoCompiler 'GeneratedContent 'FreeFile where
  runConvert hnd@(Handle _ dest) _ env =
    do (destH,destFile) <- createFreeFile dest
       hnd --> destH
       addAction hnd
                 (do Just contentList <- getArtifactState hnd
                     liftIoProgram $
                       do content <- renderContents contentList
                          renderContentToFile destFile content env)
       return destH
    where renderContents
            :: [GeneratedContentChunk] -> IoProgram Content
          renderContents = fmap Concat . mapM renderChunk
            where renderChunk (PureContent c) = return c
                  renderChunk (ContentFromIo m) = FromBinary <$> m

instance CanConvert IoCompiler 'FreeFile 'GeneratedContent where
  runConvert srcFileH@(Handle _ srcFileN) _ () =
    do (hnd,_) <- allocHandle SGeneratedContent srcFileN
       srcFileH --> hnd
       srcCopy <-
         freeFileTempCopy srcFileH
                          (Just "content-reader")
       putArtifactState hnd
                        [ContentFromIo (readContentFromFile srcCopy)]
       return hnd

instance CanAdd IoCompiler 'GeneratedContent 'GeneratedContent where
  runAdd hnd _ c =
    do modifyArtifactState
         hnd
         (Just .
          maybe [PureContent c]
                (<> [PureContent c]))
