module B9.B9IOSpec (spec) where
import Test.Hspec

import B9.B9IO

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
