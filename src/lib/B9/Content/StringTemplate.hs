{-# LANGUAGE FlexibleContexts #-}
{-| Utility functions based on 'Data.Text.Template' to offer @ $var @ variable
    expansion in string throughout a B9 artifact. -}
module B9.Content.StringTemplate
       (subst, substE, substEB, substFile, substPath, readTemplateFile,
        SourceFile(..), SourceFileConversion(..), Environment(..),
        withEnvironment)
       where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import           Control.Arrow hiding (second)
import           Control.Monad.Reader
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Data
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           Data.Text.Lazy.Encoding as LE
import           Data.Text.Template (render,templateSafe,renderA)
import           Test.QuickCheck
import           Text.Printf
import           Text.Show.Pretty (ppShow)

import           B9.ConfigUtils

import           B9.QCUtil
-- | A wrapper around a file path and a flag indicating if template variable
-- expansion should be performed when reading the file contents.
data SourceFile = Source SourceFileConversion FilePath
    deriving (Read, Show, Typeable, Data, Eq)

data SourceFileConversion = NoConversion | ExpandVariables
  deriving (Read, Show, Typeable, Data, Eq)

data Environment = Environment [(String,String)]

withEnvironment :: [(String,String)] -> ReaderT Environment m a -> m a
withEnvironment env action = runReaderT action (Environment env)

readTemplateFile :: (MonadIO m, MonadReader Environment m)
                 => SourceFile -> m B.ByteString
readTemplateFile (Source conv f') = do
  Environment env <- ask
  case substE env f' of
    Left e ->
      error (printf "Failed to substitute templates in source \
                    \file name '%s'/\nError: %s\n"
                    f' e)
    Right f -> do
      c <- liftIO (B.readFile f)
      convert f c

  where
    convert f c = case conv of
                    NoConversion -> return c
                    ExpandVariables -> do
                      Environment env <- ask
                      case substEB env c of
                        Left e ->
                          error (printf "readTemplateFile '%s' failed: \n%s\n"
                                         f
                                         e)
                        Right c' ->
                          return c'


-- String template substitution via dollar
subst :: [(String,String)] -> String -> String
subst env templateStr =
  case substE env templateStr of
    Left e -> error e
    Right r -> r

-- String template substitution via dollar
substE :: [(String,String)] -> String -> Either String String
substE env templateStr =
  second (T.unpack . E.decodeUtf8)
         (substEB env (E.encodeUtf8 (T.pack templateStr)))

-- String template substitution via dollar
substEB :: [(String,String)] -> B.ByteString -> Either String B.ByteString
substEB env templateStr = do
  t <- template'
  res <- renderA t env'
  return (LB.toStrict (LE.encodeUtf8 res))
  where
    env' t = case lookup (T.unpack t) env of
               Just v -> Right (T.pack v)
               Nothing -> Left ("Invalid template parameter: \""
                                ++ T.unpack t ++ "\" in: \""
                                ++ show templateStr
                                ++ "\".\nValid variables:\n" ++ ppShow env)

    template' = case templateSafe (E.decodeUtf8 templateStr) of
      Left (row,col) -> Left ("Invalid template, error at row: "
                             ++ show row ++ ", col: " ++ show col
                             ++ " in: \"" ++ show templateStr)
      Right t -> Right t


substFile :: MonadIO m => [(String, String)] -> FilePath -> FilePath -> m ()
substFile assocs src dest = do
    templateBs <- liftIO (B.readFile src)
    let t = templateSafe (E.decodeUtf8 templateBs)
    case t of
        Left (r,c) ->
            let badLine =
                    unlines
                        (take r (lines (T.unpack (E.decodeUtf8 templateBs))))
                colMarker = replicate (c - 1) '-' ++ "^"
            in error
                   (printf
                        "Template error in file '%s' line %i:\n\n%s\n%s\n"
                        src
                        r
                        badLine
                        colMarker)
        Right template' -> do
            let out = LE.encodeUtf8 (render template' envLookup)
            liftIO (LB.writeFile dest out)
            return ()
  where
    envT :: [(T.Text, T.Text)]
    envT = (T.pack *** T.pack) <$> assocs
    envLookup :: T.Text -> T.Text
    envLookup x = fromMaybe (err x) (lookup x envT)
    err x =
        error
            (printf
                 "Invalid template parameter: '%s'\nIn file: '%s'\n Valid variables:\n%s\n"
                 (T.unpack x)
                 src
                 (ppShow assocs))

substPath :: [(String, String)] -> SystemPath -> SystemPath
substPath assocs src =
          case src of
            Path p -> Path (subst assocs p)
            InHomeDir p -> InHomeDir (subst assocs p)
            InB9UserDir p -> InB9UserDir (subst assocs p)
            InTempDir p -> InTempDir (subst assocs p)


instance Arbitrary SourceFile where
  arbitrary = Source <$> elements [NoConversion, ExpandVariables]
                     <*> smaller arbitraryFilePath
