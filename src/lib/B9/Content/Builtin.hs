{-| The basic data structure that ties together syntax trees making them
    composable and addressable in B9 artifacts. -}
module B9.Content.Builtin where

import Control.Monad.Trans (lift)
import Control.Parallel.Strategies
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Data.Binary
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
import System.Exit
import System.Process
import Test.QuickCheck

import B9.B9Monad
import B9.Content.AST
import B9.Content.CloudConfigYaml
import B9.Content.ErlangPropList
import B9.Content.Generator
import B9.Content.StringTemplate
import B9.Content.YamlObject
import B9.QCUtil

data Content
  = RenderErlang (AST Content ErlangPropList)
  | RenderYamlObject (AST Content YamlObject)
  | RenderCloudConfig (AST Content CloudConfigYaml)
  | FromString String
  | FromTextFile SourceFile
    -- | The data in the given file will be base64 encoded.
  | RenderBase64BinaryFile FilePath
    -- | This data will be base64 encoded.
  | RenderBase64Binary Lazy.ByteString
  | FromURL String
  deriving (Read, Show, Typeable, Eq, Data, Generic)

instance Hashable Content

instance Binary Content

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
      , RenderBase64Binary . Lazy.pack <$> smaller arbitrary
      , FromURL <$> smaller arbitrary
      ]

instance ToContentGenerator Content where
  toContentGenerator (RenderErlang ast) = encodeSyntax <$> fromAST ast
  toContentGenerator (RenderYamlObject ast) = encodeSyntax <$> fromAST ast
  toContentGenerator (RenderCloudConfig ast) = encodeSyntax <$> fromAST ast
  toContentGenerator (FromTextFile s) = readTemplateFile s
  toContentGenerator (RenderBase64BinaryFile s) = readBinaryFileAsBase64 s
      -- | Read a binary file and encode it as base64
    where
      readBinaryFileAsBase64 :: MonadIO m => FilePath -> m Lazy.ByteString
      readBinaryFileAsBase64 f = Lazy.fromStrict . B64.encode <$> liftIO (Strict.readFile f)
  toContentGenerator (RenderBase64Binary b) = return (Lazy.fromStrict $ B64.encode $ Lazy.toStrict b)
  toContentGenerator (FromString str) = return (Lazy.pack str)
  toContentGenerator (FromURL url) =
    lift $ do
      dbgL $ "Downloading: " ++ url
      (exitCode, out, err) <- liftIO $ readProcessWithExitCode "curl" [url] ""
      if exitCode == ExitSuccess
        then do
          dbgL $ "Download finished. Bytes read: " ++ show (length out)
          traceL $ "Downloaded (truncated to first 4K): \n\n" ++ take 4096 out ++ "\n\n"
          return $ Lazy.pack out
        else do
          errorL $ "Download failed: " ++ err
          liftIO $ exitWith exitCode