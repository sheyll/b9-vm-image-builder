-- | B9's very own 'Prelude'.

module B9.Core.Prelude (module ReExport) where

-- import Control.Lens as ReExport hiding (argument, (#), from, use, (<.>), uncons, elements)
import Control.Lens as ReExport (makeLenses
                                ,(&)
                                ,(%~),(.~),(<>~),(&~)
                                ,(^.)
                                ,(<>=),(.=),(?=)
                                ,over,to,at,traverse,use,view)
import Control.Monad as ReExport
import Control.Monad.Reader as ReExport
import Control.Monad.State as ReExport
import Control.Arrow as ReExport
import Control.Parallel.Strategies as ReExport

import Data.Word as ReExport
import Data.Bits as ReExport
import Data.Either as ReExport
import Data.Monoid as ReExport
import Data.List as ReExport hiding (insert)
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
import Control.Monad.Free as ReExport (Free(..), liftF, foldFree)
import Control.Monad.Identity as ReExport
import Data.Foldable as ReExport
import Data.Kind as ReExport (Type)
import GHC.TypeLits as ReExport
import GHC.Generics as ReExport (Generic)

import Text.Show.Pretty as ReExport (ppShow)
import Text.Printf as ReExport (printf)

import System.Exit as ReExport (exitWith, ExitCode(..))
import System.FilePath as ReExport
       (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import System.Directory as ReExport
