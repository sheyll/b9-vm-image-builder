{-# LANGUAGE OverloadedStrings #-}
module B9.EnvironmentSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text
import B9.Environment
import Control.Eff

spec :: Spec
spec =
  describe "Environment" $
  describe "addLocalPositionalArguments" $ do
     let k = 13
         j = 7
         n = 13
         n :: Integer
         args = [pack (show i) | i <- [1 .. n]]

         k_key = pack ("arg_" ++ show k)
         j_key = pack ("arg_" ++ show j)

         k_value = pack (show k)
         j_value = pack (show j)

         testEnv = addPositionalArguments args mempty
         inEnv = run . runEnvironmentReader testEnv

     it "generates keys prefixed with arg_ followed by an ascending numeric index, starting with 1" $
         let res = inEnv $ do
                     ej_value <- lookupEither j_key
                     ek_value <- lookupEither k_key
                     e_arg_0 <- lookupEither "arg_0"
                     return (ej_value, ek_value, e_arg_0)
         in res `shouldBe` (Right j_value, Right k_value, Left (MkKeyNotFound "arg_0" testEnv))

--                      run . runEnvironmentReader testEnv $ do

