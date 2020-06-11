-- | Content defined in text files (.b9 files), read with the 'Read' instances.
module B9.Artifact.Content.Readable where

import B9.Artifact.Content
import B9.Artifact.Content.AST
import B9.Artifact.Content.CloudConfigYaml
import B9.Artifact.Content.ErlangPropList
import B9.Artifact.Content.StringTemplate
import B9.Artifact.Content.YamlObject
import B9.B9Logging
import B9.QCUtil
import B9.Text
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as Lazy
import Data.Data
import GHC.Generics (Generic)
import GHC.Stack
import System.Exit
import System.Process
import Test.QuickCheck

-- | This is content that can be 'read' via the generated 'Read' instance.
data Content
  = RenderErlang (AST Content ErlangPropList)
  | RenderYamlObject (AST Content YamlObject)
  | RenderCloudConfig (AST Content CloudConfigYaml)
  | -- | This data will be passed through unaltered.
    -- This is used during the transition phase from having B9 stuff read from
    -- files via 'Read' instances towards programatic use or the use of HOCON.
    --
    -- @since 0.5.62
    FromByteString Lazy.ByteString
  | -- | Embed a literal string
    FromString String
  | -- | Embed the contents of the 'SourceFile' with template parameter substitution.
    FromTextFile SourceFile
  | -- | The data in the given file will be base64 encoded.
    RenderBase64BinaryFile FilePath
  | -- | This data will be base64 encoded.
    RenderBase64Binary Lazy.ByteString
  | -- | Download the contents of the URL
    FromURL String
  deriving (Read, Show, Typeable, Eq, Data, Generic)

instance NFData Content

instance Arbitrary Content where
  arbitrary =
    oneof
      [ FromTextFile <$> smaller arbitrary,
        RenderBase64BinaryFile <$> smaller arbitrary,
        RenderErlang <$> smaller arbitrary,
        RenderYamlObject <$> smaller arbitrary,
        RenderCloudConfig <$> smaller arbitrary,
        FromString <$> smaller arbitrary,
        FromByteString . Lazy.pack <$> smaller arbitrary,
        RenderBase64Binary . Lazy.pack <$> smaller arbitrary,
        FromURL <$> smaller arbitrary
      ]

instance ToContentGenerator Content where
  toContentGenerator (RenderErlang ast) = unsafeRenderToText <$> fromAST ast
  toContentGenerator (RenderYamlObject ast) =
    unsafeRenderToText <$> fromAST ast
  toContentGenerator (RenderCloudConfig ast) =
    unsafeRenderToText <$> fromAST ast
  toContentGenerator (FromTextFile s) = readTemplateFile s
  toContentGenerator (RenderBase64BinaryFile s) = readBinaryFileAsBase64 s
    where
      readBinaryFileAsBase64 :: (HasCallStack, MonadIO m) => FilePath -> m Text
      readBinaryFileAsBase64 f =
        unsafeRenderToText . B64.encode <$> liftIO (Strict.readFile f)
  toContentGenerator (RenderBase64Binary b) =
    pure (unsafeRenderToText . B64.encode . Lazy.toStrict $ b)
  toContentGenerator (FromString str) = pure (unsafeRenderToText str)
  toContentGenerator (FromByteString str) =
    pure (unsafeRenderToText . Lazy.toStrict $ str)
  toContentGenerator (FromURL url) = do
    dbgL ("Downloading: " ++ url)
    (exitCode, out, err) <- liftIO (readProcessWithExitCode "curl" [url] "")
    if exitCode == ExitSuccess
      then do
        dbgL ("Download finished. Bytes read: " ++ show (length out))
        traceL
          ( "Downloaded (truncated to first 4K): \n\n" ++ take 4096 out ++ "\n\n"
          )
        pure (unsafeRenderToText out)
      else do
        errorL ("Download failed: " ++ err)
        liftIO (exitWith exitCode)

-- ** Convenient Aliases

-- | An 'ErlangPropList' 'AST' with 'Content'
--
-- @since 0.5.67
type ErlangAst = AST Content ErlangPropList
