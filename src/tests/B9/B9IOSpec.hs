module B9.B9IOSpec (spec) where

import B9.Content
import B9.B9IO
import Test.Hspec
import Text.Printf

spec :: Spec
spec =
    describe "dumpToStrings" $
    do it "returns an empty list for an empty program" $
           let p = return ()
           in dumpToStrings p `shouldBe` []
       it "handles programs containing LogTrace" $
           let p = logTrace "test-trace"
           in dumpToStrings p `shouldBe` ["logTrace test-trace"]
       it "handles programs containing GetBuildDir" $
           let p = getBuildDir >>= logTrace
           in dumpToStrings p `shouldBe` ["getBuildDir", "logTrace /BUILD"]
       it "handles programs containing GetBuildId" $
           let p = getBuildId
           in runPureDump p `shouldBe` ("build-id-1234", ["getBuildId"])
       it "handles programs containing MkTemp" $
           let p = mkTemp "test-prefix"
           in dumpToStrings p `shouldBe` ["mkTemp test-prefix"]
       it "handles programs containing MkDir" $
           let p = mkDir "test-dir"
           in dumpToStrings p `shouldBe` ["mkDir test-dir"]
       it "handles programs containing renderContentToFile" $
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
