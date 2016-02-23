{-# LANGUAGE ScopedTypeVariables #-}
-- | Content I/O
module B9.Dsl.Content where

import           B9.B9IO
import           B9.B9IO.IoCompiler
import           B9.Content
import           B9.Dsl.Core
import           B9.Dsl.File
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map
import           Data.Monoid
import           Data.Singletons.TH    hiding ((%~))

$(singletons
      [d|
  data GeneratedContent = GeneratedContent
                        | StaticContent
                        | VariableBindings
                        | TemplateVariable
                        deriving Show
  |])

type instance CreateSpec 'GeneratedContent = (Content, String)
type instance CreateSpec 'VariableBindings = [(String, String)]

type instance AddSpec 'GeneratedContent 'GeneratedContent = Content
type instance AddSpec 'VariableBindings 'TemplateVariable = (String, String)

type instance ConvSpec 'GeneratedContent 'FreeFile = ()
type instance ConvSpec 'FreeFile 'GeneratedContent = ()


-- * Implementation
type GeneratedContentList = [GeneratedContentChunk]

data GeneratedContentChunk
  = PureContent Content
  | ContentFromIo (IoProgram B.ByteString)

instance CanCreate IoCompiler 'GeneratedContent where
    runCreate _ (c,title) = do
         (hnd,_) <- allocHandle SGeneratedContent title
         putArtifactState hnd [PureContent c]
         return hnd

instance CanConvert IoCompiler 'GeneratedContent 'FreeFile where
  runConvert hnd@(Handle _ dest) _ () =
    do ensureGlobalVariableBindings
       (destH,destFile) <- createFreeFile dest
       hnd --> destH
       addAction hnd
                 (do Just contentList <- getArtifactState hnd
                     Just vars <- getArtifactState globalVars
                     let env = vars ^. to Map.toList . to Environment
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
    runConvert srcFileH@(Handle _ srcFileN) _ () = do
         ensureGlobalVariableBindings
         (hnd,_) <- allocHandle SGeneratedContent srcFileN
         srcFileH --> hnd
         srcCopy <- freeFileTempCopy srcFileH (Just "content-reader")
         putArtifactState hnd [ContentFromIo (readContentFromFile srcCopy)]
         return hnd

instance CanAdd IoCompiler 'GeneratedContent 'GeneratedContent where
    runAdd hnd _ c = do
        ensureGlobalVariableBindings
        modifyArtifactState hnd (Just . maybe [PureContent c] (<> [PureContent c]))

instance CanAdd IoCompiler 'VariableBindings 'TemplateVariable where
    runAdd _ _ (k,v) = do
        ensureGlobalVariableBindings
        modifyArtifactState globalVars setVar
          where
            setVar :: Maybe VarBindings -> Maybe VarBindings
            setVar = fmap (Map.insert k v)

ensureGlobalVariableBindings :: IoCompiler ()
ensureGlobalVariableBindings = do
    (mVars :: Maybe VarBindings) <- useArtifactState globalVars
    case mVars of
        Nothing -> do
            allocPredefinedHandle globalVars
            putArtifactState globalVars (Map.empty :: VarBindings)
        Just _ -> return ()

globalVars :: Handle 'VariableBindings
globalVars = globalHandle SVariableBindings

type VarBindings = Map.Map String String
