-- | B9's very own 'Prelude'. Imported by every module.


module B9.Common (module B9.Common, module ReExport) where

import Control.Lens as ReExport hiding (argument, (#), from, use, (<.>), uncons)
import Control.Monad as ReExport
import Control.Monad.Reader as ReExport
import Control.Monad.State as ReExport

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

import GHC.TypeLits as ReExport

import Text.Show.Pretty as ReExport (ppShow)
import Text.Printf as ReExport (printf)

import System.Exit as ReExport (exitWith, ExitCode(..))
import System.FilePath as ReExport
       (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import System.Directory as ReExport

-- | 'when' with a monadic condition.
whenM :: Monad m => m Bool -> m a -> m ()
whenM cond a = cond >>= flip when (void a)

-- | 'unless' with a monadic condition.
unlessM :: Monad m => m Bool -> m a -> m ()
unlessM cond a = cond >>= flip unless (void a)
