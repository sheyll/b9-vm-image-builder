module B9.BuilderSpec (spec) where

import Test.Hspec
#ifdef INTEGRATION_TESTS
import B9
#endif

spec :: Spec
spec =
    describe "runProgramWithConfigAndCliArgs" $
#ifdef INTEGRATION_TESTS
    do it "extracts a partition from a qcow2 image" $
           do (runProgramWithConfigAndCliArgs $
                lift $
                 extractPartitionOfQCow2 1
                                         "src/tests/B9/test-parted.qcow2"
                                         "/tmp/test.qcow2")
                `shouldReturn` True
              doesFileExist "/tmp/test.qcow2" `shouldReturn` True
              removeFile "/tmp/test.qcow2"
#else
    return ()
#endif


#ifdef INTEGRATION_TESTS

-- * DSL examples

extractPartitionOfQCow2 :: Int -> FilePath -> FilePath -> Program ()
extractPartitionOfQCow2 p srcFile dstFile = do
    inQCow <- fromFile srcFile SVmImage QCow2
    inRaw <- extract inQCow SVmImage (Left Raw)
    partedRawF <- extract inRaw SFreeFile ()
    partedRaw <- extract partedRawF SPartitionedVmImage ()
    outputFile partedRaw (MBRPartition p) dstFile

_copyEtcPasswdOntoSharedImage :: Program ()
_copyEtcPasswdOntoSharedImage = do
    root <- fromShared "prod-fc22-15.3.0"
    e <- lxc "juhu"
    void $ addFileFull e "test.mp3" (fileSpec "/test.mp3")
    outImgRaw <- mount e root "/"
    rwFs <- extract outImgRaw SFileSystemImage ()
    vmImg <- extract rwFs SVmImage ()
    vmQCow <- extract vmImg SVmImage (Left QCow2)
    vmQCow `sharedAs` "juhu-out"
    outputFile e "/etc/passwd" "/home/sven/fc-passwd"


#endif
