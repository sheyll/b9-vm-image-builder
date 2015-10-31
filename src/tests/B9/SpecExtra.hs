{- | Extra utilities for HSpec based unit tests -}
module B9.SpecExtra(should've) where

import Test.Hspec

-- | Expect that @actual@ contains all the elements of @expected@ and in that
-- order.
should've :: (Show a, Eq a) => [a] -> [a] -> Expectation
should've actual expected = actual' `shouldContain` expected'
  where
    (actual', expected') = red actual expected actual
    red (a:arest) (e:erest) xxx
      | a == e = red arest erest arest
      | otherwise = red arest (e : erest) xxx
    red _ erest xxx = (xxx, erest) -- if at least one of the parameters is empty


spec :: Spec
spec =
    describe "should've" $
    do it "returns an error for: [1] `should've` [2,1]" $
           ([1 :: Int] `should've` [2, 1]) `shouldThrow` anyException
       it "succeeds with: [] `should've` []" $
           ([] `should've` ([] :: [Int]))
       it "succeeds with: [1,2,3,4] `should've` []" $
           ([1::Int,2,3,4] `should've` [])
       it "succeeds with: [1,2,3,4] `should've` [2]" $
           ([1::Int,2,3,4] `should've` [2,4])
       it "succeeds with: [1,2,3,4] `should've` [2,4]" $
           ([1::Int,2,3,4] `should've` [2,4])
       it "succeeds with: [1,2,3,4] `should've` [1,2,3,4]" $
           ([1::Int,2,3,4] `should've` [1,2,3,4])
       it "returns an error for: [1,2,3,4] `should've` [3,2]" $
           ([1::Int,2,3,4] `should've` [3,2]) `shouldThrow` anyException
       it "returns an error for: [1,2,3,4] `should've` [1,2,3,4,5]" $
           ([1::Int,2,3,4] `should've` [1,2,3,4,5]) `shouldThrow` anyException
