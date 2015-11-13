{-| B9 is a library and build tool with primitive operations to rmrun a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.ArtifactGenerator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 (b9_version, module X) where
import Control.Monad as X
import Control.Monad.IO.Class as X
import Data.Monoid as X
import Data.List as X
import Data.Maybe as X
import Text.Show.Pretty as X (ppShow)
import System.Exit as X (exitWith, ExitCode(..))
import System.FilePath as X
       (takeDirectory, takeFileName, replaceExtension, (</>), (<.>))
import Text.Printf as X (printf)
import Data.Version as X
import B9.Builder as X
import Paths_b9 (version)
import Control.Monad.Reader as X
import Control.Monad.State as X
import Data.Default as X
import Data.Function as X (on)
import Control.Lens as X hiding (argument, (#), from, use, (<.>), uncons)

-- | Return the cabal package version of the B9 library.
b9_version :: Version
b9_version = version
