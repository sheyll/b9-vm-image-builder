module B9.B9IO.ImplSpec (spec) where

#ifdef INTEGRATION_TESTS
import B9.B9IO
import B9.Builder
#endif
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
           runIoProgramNoConfig $ do
             createFileSystem "/tmp/convert-in.raw" (FileSystemSpec Ext4 "test" 1 MB) "" []
             convertVmImage "/tmp/convert-in.raw" Raw "/tmp/convert-out.qcow2" QCow2
           doesFileExist "/tmp/convert-out.qcow2" `shouldReturn` True
       it "always returns the same build id" $ do
          (b1,b2) <- (runIoProgramNoConfig $ (,) <$> getBuildId <*> getBuildId)
          b1 `shouldBe` b2
       it "always returns the same build date" $ do
          (b1,b2) <- (runIoProgramNoConfig $ (,) <$> getBuildDate <*> getBuildDate)
          b1 `shouldBe` b2
       it "can read a file size" $ do
         runIoProgramNoConfig (do writeContentToFile "/tmp/reaadFileSizeTest" (packB "hello")
                                  readFileSize "/tmp/reaadFileSizeTest") `shouldReturn` 5
#else
    return ()
#endif

#ifdef INTEGRATION_TESTS

createFS :: FileSystemSpec -> FilePath -> [FileSpec] -> IO ()
createFS c@(FileSystemSpec t _ _ _) srcDir fs = do
    let p = do
            createFileSystem dest c srcDir fs
        dest = "/tmp/test-fs-image" <.> show t
    (runIoProgramNoConfig p) `shouldReturn` ()
    (doesFileExist dest) `shouldReturn` True
    removeFile dest

#endif
