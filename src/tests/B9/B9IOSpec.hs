module B9.B9IOSpec (spec) where

import B9.B9IO
import B9.Content
import System.FilePath
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec = actionSpec >> getParentDirSpec >> getFileNameSpec >> convertImageSpec

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
       it "handles Move" $
           let p = move "from" "to"
           in dumpToStrings p `shouldBe` ["move from to"]
       it "handles MkDir" $
           let p = mkDir "test-dir"
           in dumpToStrings p `shouldBe` ["mkDir test-dir"]
       it "handles GetRealPath" $
           let p = getRealPath "from"
           in runPureDump p `shouldBe` ("/abs/path/from", ["getRealPath from"])
       it "handles GetParentDir" $
           let p = getParentDir "from"
           in dumpToStrings p `shouldBe` ["getParentDir from"]
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
         property $ do
           filepath <- listOf $ elements $ ['a' .. 'k'] ++ ['/', '.', '-']
           let actual = dumpToResult $ getFileName filepath
               expected = takeFileName filepath
           return $ actual == expected

convertImageSpec :: Spec
convertImageSpec =
    describe "convertImageTo" $
    do it "handles ConvertImageTo" $
           property $
           \removeSource img dest ->
                let p = convertImageTo removeSource img dest
                in not $ null $ dumpToStrings p