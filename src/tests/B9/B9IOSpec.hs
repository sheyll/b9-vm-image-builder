module B9.B9IOSpec (spec) where

import B9.B9IO
import B9.CommonTypes
import B9.Content
import B9.DiskImages
import B9.FileSystems
import B9.Logging
import B9.PartitionTable
import B9.Repository
import Data.Default
import System.FilePath
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec = do
    actionSpec
    getParentDirSpec
    getFileNameSpec
    traceEveryActionWrapsAllActionsSpec

actionSpec :: Spec
actionSpec =
    describe "dumpToStrings" $
    do it "returns an empty list for an empty program" $
           let p = return ()
           in dumpToStrings p `shouldBe` []
       it "handles LogMessage" $
           let p = logMsg LogTrace "test-trace"
           in dumpToStrings p `shouldBe` ["logMessage TRACE \"test-trace\""]
       it "handles GetBuildDir" $
           let p = getBuildDir
           in runPureDump p `shouldBe` ("/BUILD", ["getBuildDir"])
       it "handles GetBuildId" $
           let p = getBuildId
           in runPureDump p `shouldBe` ("build-id-1234", ["getBuildId"])
       it "handles GetBuildDate" $
           runPureDump B9.B9IO.getBuildDate `shouldBe`
           ("1970-01-01 00:00:00", ["getBuildDate"])
       it "handles MkTemp" $
           let p = mkTemp "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTemp test-prefix"]
       it "handles MkTempIn" $
           let p = mkTempIn "parent" "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTempIn parent test-prefix"]
       it "handles MkTempDir" $
           let p = mkTempDir "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTempDir test-prefix"]
       it "handles MkTempDir In" $
           let p = mkTempDirIn "parent" "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTempDirIn parent test-prefix"]
       it "handles Copy" $
           let p = copy "from" "to"
           in dumpToStrings p `shouldBe` ["copy from to"]
       it "handles CopyDir" $
           let p = copyDir "from" "to"
           in dumpToStrings p `shouldBe` ["copyDir from to"]
       it "handles MoveFile" $
           let p = moveFile "from" "to"
           in dumpToStrings p `shouldBe` ["moveFile from to"]
       it "handles MoveDir" $
           let p = moveDir "from" "to"
           in dumpToStrings p `shouldBe` ["moveDir from to"]
       it "handles ReadFileSize" $
           runPureDump (readFileSize "test") `shouldBe`
           (1234, ["readFileSize test"])
       it "handles MkDir" $
           let p = mkDir "test-dir"
           in dumpToStrings p `shouldBe` ["mkDir test-dir"]
       it "handles EnsureParentDir" $
           let p = ensureParentDir "test"
           in runPureDump p `shouldBe` ("/abs/path/test", ["ensureParentDir test"])
       it "handles GetRealPath of relative path" $
           let p = getRealPath "from"
           in runPureDump p `shouldBe` ("/abs/path/from", ["getRealPath from"])
       it "handles GetRealPath of '.'" $
           let p = getRealPath "."
           in runPureDump p `shouldBe` ("/cwd", ["getRealPath ."])
       it "handles GetParentDir" $
           let p = getParentDir "from"
           in runPureDump p `shouldBe` (".", ["getParentDir from"])
       it "handles WriteContentToFile" $
           let p = writeContentToFile testFile testContent
               testFile = "test-file"
               testContent = B.pack "hello world!\n"
           in dumpToStrings p `shouldBe`
              [ printf
                    "writeContentToFile %s %s %s"
                    testFile
                    (show testContent)]
       it "handles CreateFileSystem" $
           runPureDump
               (createFileSystem
                    "test"
                    (FileSystemSpec Ext4 "label" 10 MB)
                    "test.d"
                    [fileSpec "test"]) `shouldBe`
           ( ()
           , [ "createFileSystem test FileSystemSpec Ext4 \"label\" 10 MB " ++
               "test.d " ++ show [fileSpec "test"]])
       it "handles ResizeFileSystem" $
           dumpToStrings (resizeFileSystem "in" ShrinkFileSystem Ext4) `shouldBe`
           ["resizeFileSystem in ShrinkFileSystem Ext4"]
       it "handles ConvertVmImage" $
           dumpToStrings (convertVmImage "in" QCow2 "out" Vmdk) `shouldBe`
           ["convertVmImage in QCow2 out Vmdk"]
       it "handles ResizeVmImage" $
           dumpToStrings (resizeVmImage "test" 10 MB QCow2) `shouldBe`
           ["resizeVmImage test 10 MB QCow2"]
       it "handles ExtractPartition" $
           dumpToStrings (extractPartition (MBRPartition 1) "src" "dest") `shouldBe`
           ["extractPartition MBRPartition 1 src dest"]
       it "handles ImageRepoLookup" $
           runPureDump (imageRepoLookup (SharedImageName "src")) `shouldBe`
           ( (testSharedImage, "~/.b9/cache/xxx.qcow2")
           , ["imageRepoLookup SharedImageName \"src\""])
       it "handles ImageRepoPublish" $
           dumpToStrings
               (imageRepoPublish "test" QCow2 (SharedImageName "dst")) `shouldBe`
           ["imageRepoPublish test QCow2 SharedImageName \"dst\""]
       it "handles ExecuteInEnv" $
           dumpToStrings (executeInEnv def def def def) `shouldBe`
           [ "executeInEnv ExecEnvSpec {_execEnvTitle = \"exec-env\", _execEnvHypervisor = LibVirtLXC, _execEnvLimits = Resources {maxMemory = AutomaticRamSize, cpuCount = 2, cpuArch = X86_64}} NoOP [] []"]
       it "handles any program, really" $
           property $
           do prog <- arbitraryIoProgram
              return $ dumpToResult (prog >> return True)

testSharedImage :: SharedImage
testSharedImage =
    SharedImage
        (SharedImageName "src")
        (SharedImageDate "01-01-1970")
        (SharedImageBuildId "00000000")
        QCow2
        Ext4


getParentDirSpec :: Spec
getParentDirSpec =
    describe "getParentDir" $
    do it "returns '.' for 'test-file'" $
           let p = getParentDir "test-file"
           in dumpToResult p `shouldBe` "."
       it "returns './d1/d2' for './d1/d2/test-file'" $
           let p = getParentDir "./d1/d2/test-file"
           in dumpToResult p `shouldBe` "./d1/d2"
       it "returns '/d1/d2' for '/d1/d2/test-file'" $
           let p = getParentDir "/d1/d2/test-file"
           in dumpToResult p `shouldBe` "/d1/d2"
       it "has the same semantics as System.FilePath.takeDirectory" $
         property $ do
           filepath <- listOf $ elements $ ['a' .. 'k'] ++ ['/', '.', '-']
           let actual = dumpToResult $ getParentDir filepath
               expected = takeDirectory filepath
           return $ actual == expected

getFileNameSpec :: Spec
getFileNameSpec =
    describe "getFileName" $
    do it "has the same semantics as System.FilePath.takeFileName" $
           property $
           do filepath <- listOf $ elements $ ['a' .. 'k'] ++ ['/', '.', '-']
              let actual = dumpToResult $ getFileName filepath
                  expected = takeFileName filepath
              return $ actual == expected

traceEveryActionWrapsAllActionsSpec :: Spec
traceEveryActionWrapsAllActionsSpec =
    describe "traceEveryAction" $
    do it "ignores 'logMsg'" $
           (runPureDump (traceL "test")) `shouldBe`
           (runPureDump (traceEveryAction (traceL "test")))
       it "handles any program" $
           property $
           do prog <- arbitraryIoProgram
              return $ dumpToResult (traceEveryAction prog >> return True)
