-- | This module marks the API to seperate unresolved 'ArtifactGenerator's
-- from those, that have all @${variable}@ resolved, and all
-- host directories made absolute to the '_buildDirRoot'.
module B9.ResolvedArtifactGenerator (ResolvedArtifactGenerator(),
            getResolvedArtifactGenerator)
    where

import B9.ArtifactGenerator
import B9.B9Monad
import B9.B9Config
import B9.VmBuilder
import B9.DiskImages
import B9.Vm
import B9.DiskImageBuilder
import System.IO.B9Extras (ensureDir, getDirectoryFiles)
import B9.Content.StringTemplate
import B9.Content.Generator
import B9.Content.AST
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Lens (view)
import Data.Data
import Data.Generics.Schemes
import GHC.Generics (Generic)
import Data.List
import Data.Function
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import System.FilePath
import System.Directory
import Text.Printf
import Text.Show.Pretty (ppShow)

-- | A newtype wrapper for 'ArtifactGenerator's that had all
-- variables resolved, and local relative paths made absolute
-- from, as if they were relative to '_buildDirRoot'.
newtype ResolvedArtifactGenerator =
    RA {getResolvedArtifactGenerator :: ArtifactGenerator}

data ArtifactAssemblyBuilder =
    ArtifactAssemblyBuilder
        { _artifactInstanceId :: InstanceId
        , _artifactBindings :: [(String, String)]
        , _artifactSources  :: [ArtifactSource]
        , _artifactAssembly :: ArtifactAssembly
        }
        deriving (Read,Show,Eq,Data,Typeable,Generic)

-- | Resolve and 'ArtifactGenerator' to a 'ResolvedArtifactGenerator'.
-- This uses '_buildDirRoot' and '' from 'getConfig' from the 'B9' monad,
--
