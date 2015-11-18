module B9.LoggingSpec (spec) where

import B9.Logging
import Data.Word
import Test.Hspec

spec :: Spec
spec = do
    describe "traceL" $
        it "logs with LogTrace to a String, arguments in correct order" $
        (traceL "a" True False (4 :: Word8) :: String) `shouldBe`
        "TRACE a True False 4"
    describe "dbgL" $
        it "logs with LogDebug to a String" $
        (dbgL "a" :: String) `shouldBe` "DEBUG a"
    describe "infoL" $
        it "logs with LogInfo to a String" $
        (infoL "a" :: String) `shouldBe` "INFO  a"
    describe "errorL" $
        it "logs with LogError to a String" $
        (errorL "a" :: String) `shouldBe` "ERROR a"
