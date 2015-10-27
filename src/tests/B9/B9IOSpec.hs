module B9.B9IOSpec (spec) where

import B9.DiskImages
import B9.B9IO
import B9.Content
import Data.List
import System.FilePath
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec = do
    actionSpec
    getParentDirSpec
    getFileNameSpec
    ensusreParentDirSpec
    traceEveryActionWrapsAllActionsSpec

actionSpec :: Spec
actionSpec =
    describe "dumpToStrings" $
    do it "returns an empty list for an empty program" $
           let p = return ()
           in dumpToStrings p `shouldBe` []
       it "handles LogTrace" $
           let p = logTrace "test-trace"
           in dumpToStrings p `shouldBe` ["logTrace test-trace"]
       it "handles MkTemp" $
           let p = mkTemp "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTemp test-prefix"]
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
       it "handles MkDir" $
           let p = mkDir "test-dir"
           in dumpToStrings p `shouldBe` ["mkDir test-dir"]
       it "handles GetRealPath" $
           let p = getRealPath "from"
           in runPureDump p `shouldBe` ("/abs/path/from", ["getRealPath from"])
       it "handles GetRealPath" $
           let p = getRealPath "."
           in runPureDump p `shouldBe` ("/cwd", ["getRealPath ."])
       it "handles GetParentDir" $
           let p = getParentDir "from"
           in runPureDump p `shouldBe` (".", ["getParentDir from"])
       it "handles GetBuildDir" $
           let p = getBuildDir
           in runPureDump p `shouldBe` ("/BUILD", ["getBuildDir"])
       it "handles GetBuildId" $
           let p = getBuildId
           in runPureDump p `shouldBe` ("build-id-1234", ["getBuildId"])
       it "handles RenderContentToFile" $
           let p = renderContentToFile testFile testContent testEnv
               testFile = "test-file"
               testContent =
                   (RenderYaml
                        (ASTObj [("test-field", ASTString "test-value")]))
               testEnv = Environment []
           in dumpToStrings p `shouldBe`
              [ printf
                    "renderContentToFile %s %s %s"
                    testFile
                    (show testContent)
                    (show testEnv)]
       it "handles CreateFileSystem" $
           runPureDump
               (createFileSystem
                    "test"
                    (FileSystemCreation Ext4 "label" 10 MB)
                    [("test", fileSpec "test")]) `shouldBe`
           ( ()
           , [ "createFileSystem test FileSystemCreation Ext4 \"label\" 10 MB " ++
               show [("test", fileSpec "test")]])
       it "handles any program, really" $
           property $
           do prog <- arbitraryIoProgram
              return $ dumpToResult (prog >> return True)

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

ensusreParentDirSpec :: Spec
ensusreParentDirSpec =
    describe "ensusreParentDir" $
    do it "creates the parent directory" $
           property $
           do filepath <- listOf $ elements $ ['a' .. 'k'] ++ ['/', '.', '-']
              let actual = dumpToStrings $ ensureParentDir filepath
                  expected = dumpToStrings $ getParentDir filepath >>= mkDir
              return $ expected `isInfixOf` actual
       it "returns the filepath with the parent directory canonicalized" $
           property $
           do filepath <- listOf $ elements $ ['a' .. 'k'] ++ ['/', '.', '-']
              let actual = dumpToResult $ ensureParentDir filepath
                  expected =
                      dumpToResult $
                      (</>) <$> (getParentDir filepath >>= getRealPath) <*>
                      getFileName filepath
              return $ expected == actual

traceEveryActionWrapsAllActionsSpec :: Spec
traceEveryActionWrapsAllActionsSpec =
    describe "traceEveryAction" $
    do it "ignores 'logTrace'" $
           (runPureDump (logTrace "test")) `shouldBe`
           (runPureDump (traceEveryAction (logTrace "test")))
       it "handles any program" $
           property $
           do prog <- arbitraryIoProgram
              return $ dumpToResult (traceEveryAction prog >> return True)
