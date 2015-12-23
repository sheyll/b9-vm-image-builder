{-# LANGUAGE ScopedTypeVariables #-}
-- | Content I/O
module B9.Dsl.Content where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.Dsl.Core
import B9.Dsl.File
import Control.Lens
import Control.Monad.Trans
import Control.Monad.State
import Data.Map            as Map
import Data.Singletons.TH hiding ((%~))
import Data.Word
import System.FilePath
import Text.Printf


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

-- * /Low-level/ 'Content' generation functions

-- | Create a handle for accumulating 'Content' with an initial 'Content'.
createContent :: (CanCreate m 'GeneratedContent)
                 => Content -> String -> ProgramT m (Handle 'GeneratedContent)
createContent c title = create SGeneratedContent (c, title)

-- | Accumulate/Append more 'Content' to the 'GeneratedContent'
--   handle obtained by e.g. 'createContent'
appendContent :: (CanAdd m 'GeneratedContent 'GeneratedContent)
                 => Handle 'GeneratedContent -> Content -> ProgramT m ()
appendContent hnd = add hnd SGeneratedContent

-- * Template variable definitions

-- | Create a template variable binding. The bindings play a role in generated
-- 'Content' and in the 'addTemplate' (and similar) functions.
($=) :: CanAdd m 'VariableBindings 'TemplateVariable
        => String -> String -> ProgramT m ()
key $= value =
    add globalVars STemplateVariable (key, value)

-- * 'Content' to file rendering functions

-- | Generate a file to an artifact from a local file template.
-- All occurences of @${var}@ will be replaced by the contents of @var@, which
-- is the last values assigned to @var@ using @"var" $= "123"@. The directory
-- part is stripped from the output file name, e.g. @template/blah/foo.cfg@ will
-- be @foo.cfg@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addTemplate
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addTemplate d f = addTemplateP d f (0,6,4,4)

-- | Same as 'addTemplate' but set the destination file permissions to @0755@
-- (executable for all).
addTemplateExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addTemplateExe d f = addTemplateP d f (0, 6, 4, 4)

-- | Same as 'addTemplate' but with an extra output file permission parameter.
addTemplateP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> ProgramT m ()
addTemplateP dstH f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
        srcFile = Source ExpandVariables f
    addFileFromContent dstH (FromTextFile srcFile) dstSpec

-- | Add an existing file from the file system, optionally with template
-- variable expansion to an artifact at a 'FileSpec'.
addFileFull
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent)
       => Handle e -> SourceFile -> FileSpec -> ProgramT m ()
addFileFull dstH srcFile dstSpec =
    case srcFile of
        (Source ExpandVariables _) ->
            addFileFromContent dstH (FromTextFile srcFile) dstSpec
        (Source NoConversion f) -> do
            origH <- create SExternalFile f
            tmpH <- convert origH SFreeFile ()
            add dstH SFreeFile (dstSpec, tmpH)

-- | Generate a file with a content and add that file to an artifact at a
-- 'FileSpec'.
addFileFromContent
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent)
       => Handle e -> Content -> FileSpec -> ProgramT m ()
addFileFromContent dstH content dstSpec = do
    cH <-
        createContent
            content
            (printf
                 "contents-of-%s"
                 (dstSpec ^. fileSpecPath . to takeFileName))
    tmpFileH <- convert cH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpFileH)

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
                let env = vars ^.  to Map.toList . to Environment
                lift (renderContentToFile destFile content env))
        return destH

instance CanAdd IoCompiler 'VariableBindings 'TemplateVariable where
    runAdd _ _ (k,v) = do
        ensureGlobalVariableBindings
        modifyArtifactState globalVars setVar
          where
            setVar :: Maybe VarBindings -> Maybe VarBindings
            setVar = fmap (Map.insert k v)

ensureGlobalVariableBindings :: StateT Ctx IoProgram ()
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
