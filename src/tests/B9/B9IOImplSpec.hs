module B9.B9IOImplSpec (spec) where

import           B9.B9IO
import           B9.B9IOImpl
import qualified B9.B9Monad as B
import           B9.Content
import           Data.List
import           System.FilePath
import           Test.Hspec
import           Test.QuickCheck
import           Text.Printf

spec :: Spec
spec =
    describe "executeIoProg" $
    do it "returns an empty list for an empty program" $
           let p = return ()
           in dumpToStrings p `shouldBe` []
