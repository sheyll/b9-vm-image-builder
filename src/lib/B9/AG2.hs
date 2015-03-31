-- | __EXPERIMENTAL__ dont look!
module B9.AG2 where


import Data.Data
import Control.Monad.Trans.RWS.Lazy
import Data.Monoid -- hiding ((<>))
import Control.Applicative
import Control.Applicative.Free

import B9.DiskImages
import B9.Vm
import B9.Content.StringTemplate
import B9.Content.Generator
import B9.QCUtil

import Test.QuickCheck

type AssemblyM m = RWST AssemblySources AssemblyTargets AssemblyState m

data AssemblySources = ASources
data AssemblyTargets = ATargets
data AssemblyState = AState


data XAsm x = XAddSource FilePath FilePath x

data XCnt x = XSourceFile FilePath (XCntParser x)
            | XString String x
            | XObject [(String, x)]
            | XList [x]

type XCntParser a = String -> Either String a


data B9Assembly next = VM_IMAGE ImageType ImageResize next
                     | SRC ArtifactSource next
                     | LET String String next
                     | OUTPUT_FILE FilePath next
                     | SHARED_IMAGE ImageResize next
                     | FILESYSTEM FileSystemProgram next

data FileSystemAssembly file next = FROM ImageSource
                                  | ADD_FILE file next

-- UC1: Vm image from a set of files
testUC1 inFiles outFile =
  fileTarget outFile <*> (diskImage Raw KeepSize <*> (emptyFileSystem "my_fs_label" Ext4 64 GB <> additionalFiles "/" inFiles))

-- UC2: Vm Image based in shared image.
testUC2 outFile =
  fileTarget outFile <*> sharedImage "fedora-20-prod"

sharedImage :: String ->


-- UC3: A Vm Image modified by a custom MonadIO action


-- data Artifact c = VmImage ImageDestination c
--                 | VmBuild [ImageArtifact c] VmScript
--
--
-- data ArtifactContent c = Txt String
--                        | FS FSType FSResize FSSource (FileSet c)
--                        | PipeThroughCmd String c
--                        | OutputOfCmd String
--                        | DiskImage ImageType ImageResize c
--                        | LXCRun VmScript c
--                        | MBRPartition MBRBootFlag (Maybe c) (Maybe c) (Maybe c) (Maybe c)
--
-- data MBRBootFlag = NoBootablePartition
--                  | Partition1Bootable
--                  | Partition2Bootable
--                  | Partition3Bootable
--                  | Partition4Bootable
--
-- data VmBuildImage c = VmBuildImage ImageDestination MountPoint c
