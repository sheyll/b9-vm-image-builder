-- | Top-Level data types for B9 build artifacts.
module B9.AG2 where


import Data.Data
import Data.Monoid -- hiding ((<>))
import Control.Applicative

import B9.DiskImages
import B9.Vm
import B9.Content.StringTemplate
import B9.Content.Generator
import B9.QCUtil

import Test.QuickCheck

data Artifact c = VmImage ImageDestination c
                | VmBuild [ImageArtifact c] VmScript

data ArtifactContent c = Txt String
                       | FS FSType FSResize FSSource (FileSet c)
                       | PipeThroughCmd String c
                       | OutputOfCmd String
                       | DiskImage ImageType ImageResize c
                       | LXCRun VmScript c
                       | MBRPartition MBRBootFlag (Maybe c) (Maybe c) (Maybe c) (Maybe c)

data MBRBootFlag = NoBootablePartition
                 | Partition1Bootable
                 | Partition2Bootable
                 | Partition3Bootable
                 | Partition4Bootable

data VmBuildImage c = VmBuildImage ImageDestination MountPoint c
