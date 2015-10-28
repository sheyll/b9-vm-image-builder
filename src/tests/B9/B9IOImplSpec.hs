module B9.B9IOImplSpec (spec) where

import B9.B9IO hiding (run)
import B9.B9IOImpl
import B9.Builder
import System.Directory
import System.FilePath
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec =
    describe "executeIoProg" $
#ifdef INTEGRATION_TESTS
    do it "can create iso images" $
           createFS (FileSystemCreation ISO9660 "cidata" 10 MB) "/tmp" [] `shouldReturn`
           ()
       it "can create vfat images" $
           createFS (FileSystemCreation VFAT "cidata" 10 MB) "/tmp" [] `shouldReturn`
           ()
       it "can create ext4 images" $
           createFS (FileSystemCreation Ext4 "test" 100 MB) "/tmp" [] `shouldReturn`
           ()
       it "cannot create ext4 images with files" $
           createFS
               (FileSystemCreation Ext4 "test" 100 MB)
               "/tmp"
               [fileSpec "test"] `shouldReturn`
           ()
#else
    return ()
#endif

createFS :: FileSystemCreation -> IO ()
createFS c@(FileSystemCreation t _ _ _) = do
    let p = do
            srcDir <- mkTempDir "test"
            createFileSystem dest c srcDir []
        dest = "/tmp/test-fs-image" <.> show t
    (execInB9 p) `shouldReturn` ()
    (doesFileExist dest) `shouldReturn` True
    removeFile dest

execInB9 :: IoProgram b -> IO b
execInB9 p = do
    cp <- configure Nothing mempty
    run cp mempty (executeIoProg p)
