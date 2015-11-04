module B9.B9IOImplSpec (spec) where

import B9.B9IO hiding (run)
import B9.B9IOImpl
import B9.Builder
import B9.FileSystems
import B9.B9Config
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec =
    describe "executeIoProg" $
#ifdef INTEGRATION_TESTS
    do it "can create iso images" $
           do createDirectoryIfMissing True "/tmp/test-files"
              writeFile "/tmp/test-files/test" "Hello World"
              createFS
                  (FileSystemSpec ISO9660 "cidata" 10 MB)
                  "/tmp/test-files"
                  [fileSpec "test"] `shouldReturn`
                  ()
       it "can create vfat images" $
           createFS
               (FileSystemSpec VFAT "cidata" 10 MB)
               "/tmp/test-files"
               [fileSpec "test"] `shouldReturn`
           ()
       it "can create ext4 images" $
           createFS (FileSystemSpec Ext4 "test" 100 MB) "/tmp/test-files" [] `shouldReturn`
           ()
       it "cannot create ext4 images with files" $
           createFS
               (FileSystemSpec Ext4 "test" 100 MB)
               "/tmp/test-files"
               [fileSpec "test"] `shouldThrow`
           anyException
       it "can convert images" $ do
           createFS (FileSystemSpec Ext4 "test-in.ext4" 10 MB) "/tmp/test-files" []
           execInB9 $ do
             createFileSystem "/tmp/convert-in.raw" (FileSystemSpec Ext4 "test" 1 MB) "" []
             convertVmImage "/tmp/convert-in.raw" Raw "/tmp/convert-out.qcow2" QCow2
           doesFileExist "/tmp/convert-out.qcow2" `shouldReturn` True
       it "always returns the same build id" $ do
          (b1,b2) <- (execInB9 $ (,) <$> B9.B9IO.getBuildId <*> B9.B9IO.getBuildId)
          b1 `shouldBe` b2
       it "always returns the same build date" $ do
          (b1,b2) <- (execInB9 $ (,) <$> B9.B9IO.getBuildDate <*> B9.B9IO.getBuildDate)
          b1 `shouldBe` b2
       it "can read a file size" $ do
         execInB9 (do renderContentToFile "/tmp/reaadFileSizeTest" (FromString "hello") (Environment [])
                      readFileSize "/tmp/reaadFileSizeTest") `shouldReturn` 5
#else
    return ()
#endif

createFS :: FileSystemSpec -> FilePath -> [FileSpec] -> IO ()
createFS c@(FileSystemSpec t _ _ _) srcDir fs = do
    let p = do
            createFileSystem dest c srcDir fs
        dest = "/tmp/test-fs-image" <.> show t
    (execInB9 p) `shouldReturn` ()
    (doesFileExist dest) `shouldReturn` True
    removeFile dest

execInB9 :: IoProgram b -> IO b
execInB9 p = do
    cp <- configure Nothing mempty
    run
        cp
        (mempty
         { verbosity = Just B9.B9Config.LogTrace
         })
        (executeIoProg p)
