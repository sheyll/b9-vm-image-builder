module B9.BuilderSpec (spec) where

import B9
import B9.DSL
import System.Directory
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec =
    describe "runProgramWithConfigAndCliArgs" $
    do it "creates a cloud-init directory with user-data and meta-data" $
           do res <- runProgramWithConfigAndCliArgs generateCloudInitDir
              res `shouldBe` True
              destPathExists <- doesDirectoryExist "/tmp/instance-xyz"
              destPathExists `shouldBe` True
              metaDataExists <- doesFileExist "/tmp/instance-xyz/meta-data"
              metaDataExists `shouldBe` True
              userDataExists <- doesFileExist "/tmp/instance-xyz/user-data"
              userDataExists `shouldBe` True
       it "creates a cloud-init ISO9660 image file" $
           do res <- runProgramWithConfigAndCliArgs generateCloudInitIso
              res `shouldBe` True
              destPathExists <- doesFileExist "/tmp/instance-abc.iso"
              destPathExists `shouldBe` True

generateCloudInitDir :: Program ()
generateCloudInitDir = do
    doc "test"
    c <- newCloudInit "instance-xyz"
    writeCloudInitDir c "/tmp/instance-xyz"
    addTemplate c "src/tests/B9/BuilderSpec.test.template"
    addFile c "/etc/passwd"
    "var" $= "value1" -- it doesn't where the variable binding occurs

generateCloudInitIso :: Program ()
generateCloudInitIso = do
    doc "test"
    c <- newCloudInit "instance-abc"
    writeCloudInit c ISO9660 "/tmp/instance-abc.iso"
