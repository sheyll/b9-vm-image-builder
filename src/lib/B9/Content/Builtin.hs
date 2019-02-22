{-| The basic data structure that ties together syntax trees making them
    composable and addressable in B9 artifacts. -}
module B9.Content.Builtin where

import Control.Monad.Trans (lift)
import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import B9.B9Monad
import B9.Content.AST
import B9.Content.CloudConfigYaml
import B9.Content.ErlangPropList
import B9.Content.StringTemplate
import B9.Content.YamlObject
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import System.Exit

import B9.QCUtil
import Test.QuickCheck

import System.Process

data Content
  = RenderErlang (AST Content ErlangPropList)
  | RenderYamlObject (AST Content YamlObject)
  | RenderCloudConfig (AST Content CloudConfigYaml)
  | FromString String
  | FromTextFile SourceFile
    -- | The data in the given file will be base64 encoded.
  | RenderBase64BinaryFile FilePath
    -- | This data will be base64 encoded.
  | RenderBase64Binary B.ByteString
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
      , RenderBase64Binary . BS.pack <$> smaller arbitrary
      , FromURL <$> smaller arbitrary
      ]

instance CanRender Content where
  render (RenderErlang ast) = encodeSyntax <$> fromAST ast
  render (RenderYamlObject ast) = encodeSyntax <$> fromAST ast
  render (RenderCloudConfig ast) = encodeSyntax <$> fromAST ast
  render (FromTextFile s) = readTemplateFile s
  render (RenderBase64BinaryFile s) = readBinaryFileAsBase64 s
      -- | Read a binary file and encode it as base64
    where
      readBinaryFileAsBase64 :: MonadIO m => FilePath -> m B.ByteString
      readBinaryFileAsBase64 f = B64.encode <$> liftIO (B.readFile f)
  render (RenderBase64Binary b) = return (B64.encode b)
  render (FromString str) = return (B.pack str)
  render (FromURL url) =
    lift $ do
      dbgL $ "Downloading: " ++ url
      (exitCode, out, err) <- liftIO $ readProcessWithExitCode "curl" [url] ""
      if exitCode == ExitSuccess
        then do
          dbgL $ "Download finished. Bytes read: " ++ show (length out)
          traceL $ "Downloaded (truncated to first 4K): \n\n" ++ take 4096 out ++ "\n\n"
          return $ B.pack out
        else do
          errorL $ "Download failed: " ++ err
          liftIO $ exitWith exitCode