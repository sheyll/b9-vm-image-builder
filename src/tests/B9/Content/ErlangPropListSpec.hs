{-# LANGUAGE OverloadedStrings #-}
module B9.Content.ErlangPropListSpec (spec) where

import Data.List
import Test.Hspec
import Test.QuickCheck
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text ()
import qualified Data.Binary as Binary

import B9.Artifact.Content.ErlTerms
import B9.Artifact.Content.ErlangPropList
import B9.Artifact.Content.AST

spec :: Spec
spec =
  describe "ErlangPropList" $ do

    it "decodeOrFail'" $
       let v = decodeOrFail' "" "ok."
       in v `shouldBe` Right (ErlangPropList (ErlAtom "ok"))

    it "Binary.encode" $
       let v = Binary.encode (ErlangPropList (ErlAtom "ok"))
       in v `shouldBe` "ok."

    it "combines primitives by putting them in a list" $
       let p1 = ErlangPropList (ErlList [ErlAtom "a"])
           p2 = ErlangPropList (ErlList [ErlNatural 123])
           combined = ErlangPropList
                        (ErlList [ErlAtom "a"
                                 ,ErlNatural 123])
       in (p1 <> p2) `shouldBe` combined

    it "combines a list and a primitve by extending the list" $
       let (Right l) = decodeOrFail' "" "[a,b,c]." :: Either String ErlangPropList
           (Right p) = decodeOrFail' "" "{ok,value}."
           (Right combined) = decodeOrFail' "" "[a,b,c,{ok,value}]."
       in l <> p `shouldBe` combined

    it "combines a primitve and a list by extending the list" $
       let (Right l) = decodeOrFail' "" "[a,b,c]." :: Either String ErlangPropList
           (Right p) = decodeOrFail' "" "{ok,value}."
           (Right combined) = decodeOrFail' "" "[{ok,value},a,b,c]."
       in p <> l `shouldBe` combined

    it "merges lists with distinct elements to lists containing the elements of both lists" $
       let p1 = ErlangPropList
                  (ErlList
                     [ErlTuple [ErlAtom "k_p1"
                               ,ErlList [ErlNatural 1]]])
           p2 = ErlangPropList
                  (ErlList
                     [ErlTuple [ErlAtom "k_p2"
                               ,ErlList [ErlNatural 1]]])
           expected = ErlangPropList
                        (ErlList
                           [ErlTuple [ErlAtom "k_p1"
                                     ,ErlList [ErlNatural 1]]
                           ,ErlTuple [ErlAtom "k_p2"
                                     ,ErlList [ErlNatural 1]]])
       in p1 <> p2 `shouldBe` expected

    it "merges two property lists into a prop list that has the lenght of the left + the right proplist - the number of entries sharing the same key" (property mergedPropListsHaveCorrectLength)

data ErlPropListTestData =
  ErlPropListTestData { plistLeft :: [SimpleErlangTerm]
                      , plistRight :: [SimpleErlangTerm]
                      , commonKeys :: [SimpleErlangTerm]
                      } deriving (Eq,Ord,Show)

mergedPropListsHaveCorrectLength :: ErlPropListTestData -> Bool
mergedPropListsHaveCorrectLength (ErlPropListTestData l r common) =
  let (ErlangPropList (ErlList merged)) =
       ErlangPropList (ErlList l) <> ErlangPropList (ErlList r)
      expectedLen = length l + length r - length common
  in length merged == expectedLen

instance Arbitrary ErlPropListTestData where
   arbitrary = do
     someKeys <- nub <$> listOf arbitraryPlistKey
     numLeftOnly <- choose (0, length someKeys - 1)
     let keysLeftOnly = take numLeftOnly someKeys
     numCommon <- choose (0, length someKeys - numLeftOnly - 1)
     let keysCommon = take numCommon (drop numLeftOnly someKeys)
     let keysRightOnly = drop (numLeftOnly + numCommon) someKeys
     let numRightOnly = length someKeys - numLeftOnly - numCommon
     valuesLeft <- vectorOf (numLeftOnly + numCommon) arbitraryPlistValue
     valuesRight <- vectorOf (numRightOnly + numCommon) arbitraryPlistValue
     return ErlPropListTestData {
       plistLeft = zipWith toPair (keysLeftOnly <> keysCommon) valuesLeft
     , plistRight = zipWith toPair (keysRightOnly <> keysCommon) valuesRight
     , commonKeys = keysCommon
     }
    where
        toPair a b = ErlTuple [a,b]
        arbitraryPlist = ErlList <$> listOf arbitraryPlistEntry
        arbitraryPlistEntry = toPair <$> arbitraryPlistKey
                                     <*> arbitraryPlistValue
        arbitraryPlistKey = arbitraryErlSimpleAtom
        arbitraryPlistValue = oneof [arbitraryLiteral
                                    ,arbitraryList
                                    ,arbitraryPlist
                                    ,arbitraryTuple]
        arbitraryTuple = ErlTuple <$> listOf arbitraryPlistValue
        arbitraryList = ErlList <$> listOf arbitraryPlistValue
        arbitraryLiteral =
          oneof [arbitraryPlistKey,arbitraryErlString,arbitraryErlNumber]
