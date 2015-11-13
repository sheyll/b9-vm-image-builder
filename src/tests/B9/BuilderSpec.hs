module B9.BuilderSpec (spec) where

import Test.Hspec
#ifdef INTEGRATION_TESTS
import B9
import B9.DSL
import B9.FileSystems
import System.Directory
import Test.QuickCheck
import Text.Printf
#endif

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
                 extractPartitionOfQCow2 1 "/tmp/test-parted.raw" "/tmp/test.qcow2")
                 `shouldReturn` True
              doesFileExist "/tmp/test.qcow2" `shouldReturn` True
              removeFile "/tmp/test.qcow2"
#else
    return ()
#endif


#ifdef INTEGRATION_TESTS

-- * DSL examples

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

extractPartitionOfQCow2 :: Int -> FilePath -> FilePath -> Program ()
extractPartitionOfQCow2 p srcFile dstFile = do
    doc
        "It's expected that there is a raw image laying around here some where with a partition 1 inside it"
    inQCow <- fromFile srcFile SVmImage QCow2
    inRaw <- convert inQCow SVmImage (Left Raw)
    partedRawF <- convert inRaw SFreeFile ()
    partedRaw <- convert partedRawF SPartitionedVmImage ()
    outputFile partedRaw (MBRPartition p) (dstFile <.> "RAW" <.> show p)

copyEtcPasswdOntoSharedImage :: Program ()
copyEtcPasswdOntoSharedImage = do
   root <- fromShared "prod-fc22-15.3.0"
   e <- lxc "juhu"
   addFileFull e (Source NoConversion "test.mp3") (fileSpec "/test.mp3")
   outImgRaw <- mount e root "/"

   rwFs <- convert outImgRaw SFileSystemImage ()
   vmImg <- convert rwFs  SVmImage ()
   vmQCow <- convert vmImg SVmImage (Left QCow2)
   vmQCow `sharedAs` "juhu-out"

   outputFile e "/etc/passwd" "/home/sven/fc-passwd"

dslExample1 :: Program ()
dslExample1 = do
    "x" $= "3"
    c <- newCloudInit "blah-ci"
    writeCloudInit c ISO9660 "test.iso"
    writeCloudInitDir c "test"
    writeCloudInit c VFAT "test.vfat"
    addMetaData c (ASTString "test")
    addUserData c (ASTString "test")
    e <- lxc "container-id"
    mountDirRW e "tmp" "/mnt/HOST_TMP"
    addFileFull
        e
        (Source ExpandVariables "httpd.conf.in")
        (fileSpec "/etc/httpd.conf")
    sh e "ls -la"
    addTemplate c "httpd.conf"
    sh c "ls -la"
    doc "From here there be dragons:"
    rootImage "fedora" "testv1-root" e
    dataImage "testv1-data" e
    {-
    img <- from "schlupfi"
    mountDir e "/tmp" "/mnt/HOST_TMP"
    sharedAs img "wupfi"
    resize img 64 GB
    resizeToMinimum img
    -}

dslExample2 :: Program ()
dslExample2 = do
    env <- lxc "c1"
    sh env "ls -lR /"

#endif
