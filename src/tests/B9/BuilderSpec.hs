module B9.BuilderSpec (spec) where

import B9
import B9.DSL
import System.Directory
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec =
#ifdef INTEGRATION_TESTS
    describe "runProgramWithConfigAndCliArgs" $
    do it "creates a cloud-init directory with user-data and meta-data" $
           do runProgramWithConfigAndCliArgs ciDir `shouldReturn` True
              doesDirectoryExist "/tmp/instance-xyz" `shouldReturn` True
              doesFileExist "/tmp/instance-xyz/meta-data" `shouldReturn` True
              doesFileExist "/tmp/instance-xyz/user-data" `shouldReturn` True
              removeDirectoryRecursive "/tmp/instance-xyz"
       it "creates a cloud-init ISO9660 image file" $
           do runProgramWithConfigAndCliArgs ciIso `shouldReturn` True
              doesFileExist "/tmp/instance-abc.iso" `shouldReturn` True
              removeFile "/tmp/instance-abc.iso"
       it "creates a cloud-init VFAT image file" $
           do runProgramWithConfigAndCliArgs ciVfat `shouldReturn` True
              doesFileExist "/tmp/instance-123.vfat" `shouldReturn` True
              removeFile "/tmp/instance-123.vfat"
#else
    describe "runProgramWithConfigAndCliArgs *DISABLED*" $ return ()
#endif

ciDir :: Program ()
ciDir = do
    doc "test"
    c <- newCloudInit "instance-xyz"
    writeCloudInitDir c "/tmp/instance-xyz"
    addTemplate c "src/tests/B9/BuilderSpec.test.template"
    addFile c "/etc/passwd"
    "var" $= "value1" -- it doesn't matter where the variable binding occurs

ciIso :: Program ()
ciIso = do
    doc "test"
    c <- newCloudInit "instance-abc"
    writeCloudInit c ISO9660 "/tmp/instance-abc.iso"

ciVfat :: Program ()
ciVfat = do
    doc "test"
    c <- newCloudInit "instance-123"
    writeCloudInit c ISO9660 "/tmp/instance-123.vfat"
