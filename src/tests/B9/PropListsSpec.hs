{-# LANGUAGE OverloadedStrings #-}
module B9.PropListsSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.List
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import B9.PropLists
import Data.Semigroup
import qualified Data.ByteString.Char8 as B
import Text.Printf
import Data.Text
import Data.Yaml

spec :: Spec
spec = do
  describe "YamlPList's semi group instance" $ do

    it "combines two discting objects to an object containing all properties" $
      let plist1 = YamlPList (object ["k1" .= Number 1])
          plist2 = YamlPList (object ["k2" .= Number 2])
          combined = YamlPList (object ["k1" .= Number 1
                                       ,"k2" .= Number 2])
      in ((plist1 <> plist2) `shouldBe` combined)
