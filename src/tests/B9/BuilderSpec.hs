module B9.BuilderSpec (spec) where

import B9
import B9.DSL
import B9.FileSystems
import System.Directory
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec =
    describe "runProgramWithConfigAndCliArgs" $
#ifdef INTEGRATION_TESTS
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
       it "extracts a partition from a qcow2 image" $
           do (runProgramWithConfigAndCliArgs $
                 extractPartitionOfQCow2 "/tmp/test-parted.raw" "/tmp/test.qcow2")
                 `shouldReturn` True
              doesFileExist "/tmp/test.qcow2" `shouldReturn` True
              removeFile "/tmp/test.qcow2"
#else
    return ()
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

extractPartitionOfQCow2 :: FilePath -> FilePath -> Program ()
extractPartitionOfQCow2 srcFile dstFile = do
    doc
        "It's expected that there is a raw image laying around here some where with a partition 1 inside it"
    p <- create SReadOnlyFile srcFile
    partImg <- create SPartitionedVmImage p
    p1 <- export partImg (Nothing, MBRPartition 1)
    destI <- create SVmImage (p1, Raw)
    void $ export destI (Just dstFile, Just QCow2, Nothing)

-- copyEtcPasswdOntoSharedImage :: Program ()
-- copyEtcPasswdOntoSharedImage = do
--   root <- fromShared "prod-el7.centos-15.3.0"
--   e <- lxc "juhu"
--   addFileFull e (Source NoConversion "/home/sven/Downloads/wdrhoerspielspeicher_2014-10-31_00-02.mp3") (fileSpec "/test.mp3")
--   outImgRaw <- mount e root "/"
--
--   rwFs <- convert outImgRaw SFileSystemImageRO ()
--   vmImg <- convert rwFs  SVmImage (Just ShrinkFileSystem)
--   vmQCow <- convert vmImg SVmImage (Just QCow2, Nothing)
--   vmQCow `sharedAs` "juhu-out"
-- TODO
