 module B9.ArtifactGenerator
  (Artifact (..)
  ,buildIdKey
  ,buildDateKey
  ) where

import Data.Data

import B9.DiskImages
import B9.Generator
import B9.ShellScript
import B9.Vm

-- | A generator specifies howto generate output files/directories. It consists
-- of a possible netsted set of variable bindings that are replaced inside the
-- text files. The `generate` function creates a list of variable substituted
-- 'a'.
type ArtifactGenerator = Generator Artifact

-- | An Artifact is some file system object, i.e. a directory or a regular
-- file. The content and relevance of that file/directory is determined by the
-- 'ArtifactSource'.
data Artifact = Artifact FilePath ArtifactFile
              | Directory FilePath [Artifact]
              | VmImages [ImageTarget] VmScript -- ^ Create VmImages. The
                                                -- filename if the artifact is
                                                -- the directory to where
  deriving (Read, Show, Typeable, Data, Eq)


-- | An artifact source provides the content of an artifact. There are direct
-- sources that refer files and higher order sources that represent a
-- combination of other sources.
data ArtifactFile = Content ArtifactContent
                  | Container ContainerType [Artifact]
                  | Permissions (Int,Int,Int) ArtifactFile
                  | WithArtifactFileName String ArtifactFile
                  | CreationCommand Script


  deriving (Read, Show, Typeable, Data, Eq)

data ContainerType = TarGz | CI_ISO | CI_VFAT
  deriving (Read, Show, Typeable, Data, Eq)

data ArtifactContent = Interpolated ArtifactContent
                     | FromFile FilePath
                     | FromTemplate FilePath
                     | FromURL String
                     | Joined Syntax [ArtifactContent]
                     | JoinedG Syntax (Generator ArtifactContent)
                     | Echo String
                     | Embed ArtifactFile
                     | FromStdOut Script
                     | FromStdIn
                     | ContentGenerator (Generator ArtifactContent)


  deriving (Read, Show, Typeable, Data, Eq)

data Syntax = ErlangTerms | YamlObjects | PlainText
  deriving (Read, Show, Typeable, Data, Eq)

buildIdKey :: String
buildIdKey = "build_id"

buildDateKey :: String
buildDateKey = "build_date"
