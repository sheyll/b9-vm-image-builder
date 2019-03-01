{-# LANGUAGE FlexibleContexts #-}

{-| Utility functions based on 'Data.Text.Template' to offer @ $var @ variable
    expansion in string throughout a B9 artifact. -}
module B9.Artifact.Content.StringTemplate
  ( subst
  , substFile
  , substPath
  , readTemplateFile
  , withSubstitutedStringBindings
  , SourceFile(..)
  , SourceFileConversion(..)
  )
where
import           B9.B9Error
import           B9.Environment
import           B9.QCUtil
import           Control.Exception              ( displayException )
import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Eff                   as Eff
import           Control.Monad.Trans.Identity   ( )
import           Control.Parallel.Strategies
import           Data.Binary
import qualified Data.ByteString               as Strict
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Data
import           Data.Hashable
import qualified Data.Text.Lazy                as LazyT
import qualified Data.Text                     as StrictT
import qualified Data.Text.Encoding            as StrictE
import qualified Data.Text.Lazy.Encoding       as LazyE
import           Data.Text.Template             ( renderA
                                                , templateSafe
                                                )
import           GHC.Generics                   ( Generic )
import           System.IO.B9Extras
import           Test.QuickCheck
import           Text.Printf

-- | A wrapper around a file path and a flag indicating if template variable
-- expansion should be performed when reading the file contents.
data SourceFile =
  Source SourceFileConversion
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

readTemplateFile
  :: (MonadIO (Eff e), '[ExcB9, EnvironmentReader] <:: e)
  => SourceFile
  -> Eff e Lazy.ByteString
readTemplateFile (Source conv f') = do
  let
    onErrorFileName e = error
      (printf
        "Failed to substitute templates in source \
                                    \file name '%s'/\nError: %s\n"
        f'
        (displayException e)
      )
  f <- subst f' `catchB9Error` onErrorFileName
  c <- liftIO (Lazy.readFile f)
  case conv of
    NoConversion -> return c
    ExpandVariables ->
      let onErrorFile e =
              error
                (printf "readTemplateFile '%s' failed: \n%s\n"
                        f
                        (displayException e)
                )
      in  substEB c `catchB9Error` onErrorFile

-- String template substitution via dollar
subst :: (Member ExcB9 e, Member EnvironmentReader e) => String -> Eff e String
subst templateStr = LazyT.unpack . LazyE.decodeUtf8 <$> substEB
  (LazyE.encodeUtf8 (LazyT.pack templateStr))

-- String template substitution via dollar
substEB
  :: (Member ExcB9 e, Member EnvironmentReader e)
  => Lazy.ByteString
  -> Eff e Lazy.ByteString
substEB templateStr = do
  t <- template'
  LazyE.encodeUtf8 <$> renderA t templateEnvLookup
 where
  template' =
    case templateSafe (LazyT.toStrict (LazyE.decodeUtf8 templateStr)) of
      Left (row, col) -> throwB9Error
        (  "Invalid template, error at row: "
        ++ show row
        ++ ", col: "
        ++ show col
        ++ " in: \""
        ++ show templateStr
        )
      Right t -> return t

substFile
  :: (Member EnvironmentReader e, Member ExcB9 e, MonadIO (Eff e))
  => FilePath
  -> FilePath
  -> Eff e ()
substFile src dest = do
  templateBs <- liftIO (Strict.readFile src)
  let t = templateSafe (StrictE.decodeUtf8 templateBs)
  case t of
    Left (r, c) ->
      let badLine =
              unlines
                (take r (lines (StrictT.unpack (StrictE.decodeUtf8 templateBs))))
          colMarker = replicate (c - 1) '-' ++ "^"
      in  throwB9Error
            (printf "Template error in file '%s' line %i:\n\n%s\n%s\n"
                    src
                    r
                    badLine
                    colMarker
            )
    Right template' -> do
      out <- LazyE.encodeUtf8
        <$> renderA template' (templateEnvLookupSrcFile src)
      liftIO (Lazy.writeFile dest out)


templateEnvLookup
  :: (Member EnvironmentReader e, Member ExcB9 e)
  => StrictT.Text
  -> Eff e StrictT.Text
templateEnvLookup x = LazyT.toStrict <$> lookupOrThrow (LazyT.fromStrict x)

templateEnvLookupSrcFile
  :: (Member EnvironmentReader e, Member ExcB9 e, MonadIO (Eff e))
  => FilePath
  -> StrictT.Text
  -> Eff e StrictT.Text
templateEnvLookupSrcFile src x = do
  r <- catchB9ErrorAsEither (lookupOrThrow (LazyT.fromStrict x))
  either err (pure . LazyT.toStrict) r
  where err e = throwB9Error (show e ++ "\nIn file: \'" ++ src ++ "\'\n")


substPath
  :: (Member EnvironmentReader e, Member ExcB9 e)
  => SystemPath
  -> Eff e SystemPath
substPath src = case src of
  Path        p -> Path <$> subst p
  InHomeDir   p -> InHomeDir <$> subst p
  InB9UserDir p -> InB9UserDir <$> subst p
  InTempDir   p -> InTempDir <$> subst p

instance Arbitrary SourceFile where
  arbitrary =
    Source
      <$> elements [NoConversion, ExpandVariables]
      <*> smaller arbitraryFilePath


-- | Extend an 'Environment' with new bindings, where each value may contain
-- string templates with like @"Hello $name, how is life on $planet these days?"@.
--
-- @since 0.5.64
withSubstitutedStringBindings
  :: (Member EnvironmentReader e, Member ExcB9 e)
  => [(String, String)]
  -> Eff e s
  -> Eff e s
withSubstitutedStringBindings bs nested = do
  let extend env (k, v) = localEnvironment (const env) $ do
        kv <- (k, ) <$> subst v
        addStringBinding kv env
  env    <- askEnvironment
  envExt <- foldM extend env bs
  localEnvironment (const envExt) nested
