{-# LANGUAGE ScopedTypeVariables #-}
-- | Content I/O
module B9.Dsl.Content where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import B9.Dsl.File
import Control.Lens
import Data.Map            as Map
import Data.Singletons.TH hiding ((%~))

$(singletons
      [d|
  data GeneratedContent = GeneratedContent
                        | VariableBindings
                        | TemplateVariable
                        deriving Show
  |])

type instance CreateSpec 'GeneratedContent = (Content, String)
type instance CreateSpec 'VariableBindings = [(String, String)]

type instance AddSpec 'GeneratedContent 'GeneratedContent = Content
type instance AddSpec 'VariableBindings 'TemplateVariable = (String, String)

type instance ConvSpec 'GeneratedContent 'FreeFile = ()

-- * Implementation

instance CanCreate IoCompiler 'GeneratedContent where
    runCreate _ (c,title) = do
         (hnd,_) <- allocHandle SGeneratedContent title
         putArtifactState hnd c
         return hnd

instance CanConvert IoCompiler 'GeneratedContent 'FreeFile where
    runConvert hnd@(Handle _ dest) _ () = do
        ensureGlobalVariableBindings
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction
            hnd
            (do Just content <- getArtifactState hnd
                Just vars <- getArtifactState globalVars
                let env = vars ^. to Map.toList . to Environment
                liftIoProgram (renderContentToFile destFile content env))
        return destH

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
