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
data Artifact = Target FilePath ArtifactFile
              | VmImages [ImageTarget] VmScript -- ^ Create VmImages. The
                                                -- filename if the artifact is
                                                -- the directory to where
              | WithIntermediateTarget String ArtifactFile [Artifact]
  deriving (Read, Show, Typeable, Data, Eq)


-- | An artifact source provides the content of an artifact. There are direct
-- sources that refer files and higher order sources that represent a
-- combination of other sources.
data ArtifactFile = Contains ArtifactContent
                  | SourceCommand Script
                  | SourceFile FilePath
                  | SourceURL String
                  | Permissions (Int,Int,Int) ArtifactFile
                  | Container ContainerType [Artifact]
                  | IntermediateArtifact String
  deriving (Read, Show, Typeable, Data, Eq)

data ArtifactContent = Literal String
                     | ContentsOf ArtifactFile
                     | Interpolated ArtifactContent
                     | Merged Syntax [ArtifactContent]
                     | OutputOf Script
                     | CurrentDate String
                     | CurrentBuildId
                     | LiteralFileSize ArtifactFile
  deriving (Read, Show, Typeable, Data, Eq)

xxx :: ArtifactGenerator
xxx = Build
        [WithIntermediateTarget
           "etc.tar.gz"
           (Container TarGz [Target "/etc" (Container Directory [Target "passwd" (Contains (Literal "blub"))])])
           [Target
               "user-data"
               (Contains
                     (Merged
                        PlainText
                        [Literal "#cloud-config content-type:multi-part\n"
                        ,Literal "#part 1 yaml user-data"
                        ,Merged YamlObjects [Interpolated (ContentsOf (SourceFile "common-user-data"))
                                            ,Interpolated (ContentsOf (SourceURL "http://xxx/v3-user-data"))]
                        ,Literal "#part 2 content-type: application/tar-gz, content-length: "
                        ,LiteralFileSize (IntermediateArtifact "etc.tar.gz")
                        ,Literal "\n"
                        ,ContentsOf (IntermediateArtifact "etc.tar.gz")
                        ])
                  )]
                  ]

data Syntax = ErlangTerms | YamlObjects | PlainText
  deriving (Read, Show, Typeable, Data, Eq)
data ContainerType = TarGz | Xargs Script | DestinationImage | Directory
  deriving (Read, Show, Typeable, Data, Eq)

buildIdKey :: String
buildIdKey = "build_id"

buildDateKey :: String
buildDateKey = "build_date"
