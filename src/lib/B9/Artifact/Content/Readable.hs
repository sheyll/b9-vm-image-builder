{-| Content defined in text files (.b9 files), read with the 'Read' instances.

-}
module B9.Artifact.Content.Readable where

import Control.Monad.Trans (lift)
import Control.Parallel.Strategies
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Data.Binary as Binary
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Data
import GHC.Generics (Generic)
import System.Exit
import System.Process
import Test.QuickCheck

import B9.B9Monad
import B9.Artifact.Content
import B9.Artifact.Content.AST
import B9.Artifact.Content.CloudConfigYaml
import B9.Artifact.Content.ErlangPropList
import B9.Artifact.Content.StringTemplate
import B9.Artifact.Content.YamlObject
import B9.QCUtil

-- | This is content that can be 'read' via the generated 'Read' instance.
data Content
  = RenderErlang (AST Content ErlangPropList)
  | RenderYamlObject (AST Content YamlObject)
  | RenderCloudConfig (AST Content CloudConfigYaml)
    -- | This data will be passed through unaltered.
    -- This is used during the transition phase from having B9 stuff read from
    -- files via 'Read' instances towards programatic use or the use of HOCON.
    --
    -- @since 0.5.62
  | FromByteString Lazy.ByteString
    -- | Embed a literal string
  | FromString String
    -- | Embed the contents of the 'SourceFile' with template parameter substitution.
  | FromTextFile SourceFile
    -- | The data in the given file will be base64 encoded.
  | RenderBase64BinaryFile FilePath
    -- | This data will be base64 encoded.
  | RenderBase64Binary Lazy.ByteString
    -- | Download the contents of the URL
  | FromURL String
  deriving (Read, Show, Typeable, Eq, Data, Generic)

instance NFData Content

instance Arbitrary Content where
  arbitrary =
    oneof
      [ FromTextFile <$> smaller arbitrary
      , RenderBase64BinaryFile <$> smaller arbitrary
      , RenderErlang <$> smaller arbitrary
      , RenderYamlObject <$> smaller arbitrary
      , RenderCloudConfig <$> smaller arbitrary
      , FromString <$> smaller arbitrary
      , FromByteString . Lazy.pack <$> smaller arbitrary
      , RenderBase64Binary . Lazy.pack <$> smaller arbitrary
      , FromURL <$> smaller arbitrary
      ]

instance ToContentGenerator Content where
  toContentGenerator (RenderErlang ast) = Binary.encode <$> fromAST ast
  toContentGenerator (RenderYamlObject ast) = Binary.encode <$> fromAST ast
  toContentGenerator (RenderCloudConfig ast) = Binary.encode <$> fromAST ast
  toContentGenerator (FromTextFile s) = readTemplateFile s
  toContentGenerator (RenderBase64BinaryFile s) = readBinaryFileAsBase64 s
    where
      readBinaryFileAsBase64 :: MonadIO m => FilePath -> m Lazy.ByteString
      readBinaryFileAsBase64 f = Lazy.fromStrict . B64.encode <$> liftIO (Strict.readFile f)
  toContentGenerator (RenderBase64Binary b) = pure (Lazy.fromStrict $ B64.encode $ Lazy.toStrict b)
  toContentGenerator (FromString str) = pure (Lazy.pack str)
  toContentGenerator (FromByteString str) = pure str
  toContentGenerator (FromURL url) =
    lift $ do
      dbgL $ "Downloading: " ++ url
      (exitCode, out, err) <- liftIO $ readProcessWithExitCode "curl" [url] ""
      if exitCode == ExitSuccess
        then do
          dbgL $ "Download finished. Bytes read: " ++ show (length out)
          traceL $ "Downloaded (truncated to first 4K): \n\n" ++ take 4096 out ++ "\n\n"
          pure $ Lazy.pack out
        else do
          errorL $ "Download failed: " ++ err
          liftIO $ exitWith exitCode