module B9.GeneratorSpec (spec) where

import Test.Hspec
import Control.Applicative
import Data.List
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Data.Monoid

import B9.Generator

spec :: Spec
spec =

  describe "mappend" $ do

    it "is associative" $
       (property $(monomorphic (\g1 g2 g3 -> g1 <> (g2 <> g3) == (g1 <> g2) <> g3)))
