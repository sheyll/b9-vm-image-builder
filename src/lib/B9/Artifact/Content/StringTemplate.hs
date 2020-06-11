{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions based on 'Data.Text.Template' to offer @ $var @ variable
--    expansion in string throughout a B9 artifact.
--
--    @deprecated
--
--    TODO remove this in the move to Dhall
module B9.Artifact.Content.StringTemplate
  ( subst,
    substStr,
    substFile,
    substPath,
    readTemplateFile,
    withSubstitutedStringBindings,
    SourceFile (..),
    SourceFileConversion (..),
  )
where

import B9.B9Error
import B9.Environment
import B9.QCUtil
import Control.Eff as Eff
import Control.Exception (displayException)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Identity ()
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
  ( toStrict,
  )
import Data.Text.Template
  ( Template,
    renderA,
    templateSafe,
  )
import GHC.Generics (Generic)
import System.IO.B9Extras
import Test.QuickCheck
import Text.Printf

-- | A wrapper around a file path and a flag indicating if template variable
-- expansion should be performed when reading the file contents.
data SourceFile
  = Source
      SourceFileConversion
      FilePath
  deriving (Read, Show, Typeable, Data, Eq, Generic)

instance Hashable SourceFile

instance Binary SourceFile

instance NFData SourceFile

data SourceFileConversion
  = NoConversion
  | ExpandVariables
  deriving (Read, Show, Typeable, Data, Eq, Generic)

instance Hashable SourceFileConversion

instance Binary SourceFileConversion

instance NFData SourceFileConversion

readTemplateFile ::
  (MonadIO (Eff e), '[ExcB9, EnvironmentReader] <:: e) =>
  SourceFile ->
  Eff e Text
readTemplateFile (Source conv f') = do
  let onErrorFileName e =
        error
          ( printf
              "Failed to substitute templates in source \
              \file name '%s'/\nError: %s\n"
              f'
              (displayException e)
          )
  f <- subst (Text.pack f') `catchB9Error` onErrorFileName
  c <- liftIO (Text.readFile (Text.unpack f))
  case conv of
    NoConversion -> return c
    ExpandVariables ->
      let onErrorFile e =
            error
              ( printf
                  "readTemplateFile '%s' failed: \n%s\n"
                  f
                  (displayException e)
              )
       in subst c `catchB9Error` onErrorFile

-- | 'Text' template substitution.
subst :: (Member ExcB9 e, Member EnvironmentReader e) => Text -> Eff e Text
subst templateStr = do
  t <- templateSafeExcB9 templateStr
  LazyText.toStrict <$> renderA t lookupOrThrow

-- | 'String' template substitution
substStr ::
  (Member ExcB9 e, Member EnvironmentReader e) => String -> Eff e String
substStr templateStr = do
  t <- templateSafeExcB9 (Text.pack templateStr)
  Text.unpack . LazyText.toStrict <$> renderA t lookupOrThrow

templateSafeExcB9 :: Member ExcB9 e => Text -> Eff e Template
templateSafeExcB9 templateStr = case templateSafe templateStr of
  Left (row, col) ->
    throwB9Error
      ( "Invalid template, error at row: "
          ++ show row
          ++ ", col: "
          ++ show col
          ++ " in: \""
          ++ show templateStr
      )
  Right t -> return t

substFile ::
  (Member EnvironmentReader e, Member ExcB9 e, MonadIO (Eff e)) =>
  FilePath ->
  FilePath ->
  Eff e ()
substFile src dest = do
  templatedText <- liftIO (Text.readFile src)
  let t = templateSafe templatedText
  case t of
    Left (r, c) ->
      let badLine = Text.unlines (take r (Text.lines templatedText))
          colMarker = Text.replicate (c - 1) "-" <> "^"
       in throwB9Error
            ( printf
                "Template error in file '%s' line %i:\n\n%s\n%s\n"
                src
                r
                badLine
                colMarker
            )
    Right template' -> do
      out <- renderA template' (templateEnvLookupSrcFile src)
      liftIO (Text.writeFile dest (LazyText.toStrict out))

templateEnvLookupSrcFile ::
  (Member EnvironmentReader e, Member ExcB9 e, MonadIO (Eff e)) =>
  FilePath ->
  Text ->
  Eff e Text
templateEnvLookupSrcFile src x = do
  r <- catchB9ErrorAsEither (lookupOrThrow x)
  either err pure r
  where
    err e = throwB9Error (show e ++ "\nIn file: \'" ++ src ++ "\'\n")

substPath ::
  (Member EnvironmentReader e, Member ExcB9 e) =>
  SystemPath ->
  Eff e SystemPath
substPath src = case src of
  Path p -> Path <$> substStr p
  InHomeDir p -> InHomeDir <$> substStr p
  InB9UserDir p -> InB9UserDir <$> substStr p
  InTempDir p -> InTempDir <$> substStr p

instance Arbitrary SourceFile where
  arbitrary =
    Source
      <$> elements [NoConversion, ExpandVariables]
      <*> smaller arbitraryFilePath

-- | Extend an 'Environment' with new bindings, where each value may contain
-- string templates with like @"Hello $name, how is life on $planet these days?"@.
--
-- @since 0.5.64
withSubstitutedStringBindings ::
  (Member EnvironmentReader e, Member ExcB9 e) =>
  [(String, String)] ->
  Eff e s ->
  Eff e s
withSubstitutedStringBindings bs nested = do
  let extend env (k, v) = localEnvironment (const env) $ do
        kv <- (Text.pack k,) <$> subst (Text.pack v)
        addBinding kv env
  env <- askEnvironment
  envExt <- foldM extend env bs
  localEnvironment (const envExt) nested
