module B9.Dsl.FileSystem where

import B9.CommonTypes
import B9.DiskImages
import B9.Content
import B9.Dsl.Core
import B9.Dsl.File
import B9.FileSystems
import Control.Lens
import Data.Singletons.TH


-- * File System API

-- | Create a file sytem image with a given type, label and size in giga
-- byte. Use the supplied action to add files to the fileSystemBuilder

-- * File System API Implementation

$(singletons
      [d|
  data FileSystemArtifact = FileSystemBuilder
                          | FileSystemImage
                          deriving Show
  |])

type instance CreateSpec 'FileSystemBuilder = FileSystemSpec
type instance AddSpec 'FileSystemBuilder 'FreeFile =
     (FileSpec, Handle 'FreeFile)
type instance ConvSpec 'FileSystemBuilder 'FileSystemImage = ()
type instance ConvSpec 'FileSystemBuilder 'FreeFile = ()
type instance ConvSpec 'FileSystemImage 'FileSystemImage =
     FileSystemResize
type instance ConvSpec 'FileSystemImage 'FreeFile = ()
type instance ConvSpec 'FreeFile 'FileSystemImage = FileSystem
type instance ExportSpec 'FileSystemImage = FilePath
