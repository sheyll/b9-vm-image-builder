-- | B9's very own 'Prelude'. Imported by every module.


module B9.Common (module B9.Common, module ReExport) where

-- import Control.Lens as ReExport hiding (argument, (#), from, use, (<.>), uncons, elements)
import Control.Lens as ReExport (makeLenses
                                ,(&)
                                ,(%~),(.~),(<>~),(&~)
                                ,(^.)
                                ,(<>=),(.=),(?=)
                                ,over,to,at,traverse)
import Control.Monad as ReExport
import Control.Monad.Reader as ReExport
import Control.Monad.State as ReExport
import Control.Arrow as ReExport
import Control.Parallel.Strategies as ReExport

import Data.Word as ReExport
import Data.Bits as ReExport
import Data.Either as ReExport
import Data.Monoid as ReExport
import Data.List as ReExport
import Data.Maybe as ReExport
import Data.Default as ReExport
import Data.Function as ReExport (on)
import Data.Version as ReExport
import Data.Proxy as ReExport
import Data.Data as ReExport
import Data.ByteString.Char8 as ReExport (ByteString)
import Data.Text as ReExport (Text)
import Data.Text.Encoding as ReExport (decodeUtf8, encodeUtf8)
import Data.Binary as ReExport (Binary)
import Data.Hashable as ReExport

import GHC.TypeLits as ReExport
import GHC.Generics as ReExport (Generic)

import Text.Show.Pretty as ReExport (ppShow)
import Text.Printf as ReExport (printf)

import System.Exit as ReExport (exitWith, ExitCode(..))
import System.FilePath as ReExport
       (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import System.Directory as ReExport


-- * Local imports
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

-- | Convert a 'ByteString' to a 'String', assuming 'UTF8' is used.
unpackUtf8 :: ByteString -> String
unpackUtf8 = T.unpack . decodeUtf8

-- | Get the length of a 'ByteString'.
lengthB :: ByteString -> Int
lengthB = B.length

-- | Convert a 'String' to a 'ByteString'.
packB :: String -> ByteString
packB = B.pack

-- | Convert the 'show' of something to a 'ByteString'.
showB :: Show a => a -> ByteString
showB = packB . show

-- | Convert a 'Text' to a 'String'.
unpackT :: Text -> String
unpackT = T.unpack

-- | Convert a 'String' to a 'Text'.
packT :: String -> Text
packT = T.pack

-- | Convert the 'show' of something to a 'Text'.
showT :: Show a => a -> Text
showT = packT . show

-- | 'when' with a monadic condition.
whenM :: Monad m => m Bool -> m a -> m ()
whenM cond a = cond >>= flip when (void a)

-- | 'unless' with a monadic condition.
unlessM :: Monad m => m Bool -> m a -> m ()
unlessM cond a = cond >>= flip unless (void a)
