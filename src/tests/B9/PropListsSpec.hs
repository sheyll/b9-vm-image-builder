{-# LANGUAGE OverloadedStrings #-}
module B9.PropListsSpec (spec) where

import Control.Applicative
import Data.List
import Test.Hspec
import Test.QuickCheck
import Data.Semigroup
import Data.Text ()
import Data.Yaml

import B9.ConcatableSyntax
import B9.PropLists
import B9.ErlTerms

spec :: Spec
spec = do
  describe "YamlUserData" $ do

    it "combines primitives by putting them in an array" $
       let v1 = YamlUserData (toJSON True)
           v2 = YamlUserData (toJSON (123::Int))
           combined = YamlUserData (array [toJSON True
                                          ,toJSON (123::Int)])
       in (v1 <> v2) `shouldBe` combined

    it "combines objects with disjunct keys to an object \
       \containing all properties" $
       let plist1 = YamlUserData (object ["k1" .= Number 1])
           plist2 = YamlUserData (object ["k2" .= Number 2])
           combined = YamlUserData (object ["k1" .= Number 1
                                           ,"k2" .= Number 2])
       in (plist1 <> plist2) `shouldBe` combined

    it "combines arrays by concatenating them" $
       let v1 = YamlUserData (array [toJSON ("x"::String)])
           v2 = YamlUserData (array [toJSON ("y"::String)])
           combined = YamlUserData (array [toJSON ("x"::String)
                                       ,toJSON ("y"::String)])
       in (v1 <> v2) `shouldBe` combined

    it "combines objects to a an object containing\
       \ all disjunct entries and combined entries\
       \ with the same keys" $
       let o1 = YamlUserData (object ["k1" .= Number 1
                                     ,"k" .= Number 2])
           o2 = YamlUserData (object ["k2" .= Number 3
                                     ,"k" .= Number 4])
           combined =
             YamlUserData (object ["k1" .= Number 1
                                  ,"k2" .= Number 3
                                  ,"k" .= array [Number 2
                                                ,Number 4]])
       in (o1 <> o2) `shouldBe` combined

    it "combines 'write_files' and 'runcmd' from typical 'user-data' files \
       \by merging each" $
     let (Right ud1) = decodeSyntax "#user-data\n\
                                  \\n\
                                  \write_files:\n\
                                  \  - contents: |\n\
                                  \      hello world!\n\
                                  \\n\
                                  \    path: /sdf/xyz/filename.cfg\n\
                                  \    owner: root:root\n\
                                  \\n\
                                  \runcmd:\n\
                                  \ - x y z\n"

         (Right ud2) = decodeSyntax "#user-data\n\
                                  \\n\
                                  \write_files:\n\
                                  \  - contents: |\n\
                                  \      hello world2!\n\
                                  \\n\
                                  \    path: /sdf/xyz/filename.cfg\n\
                                  \    owner: root:root\n\
                                  \\n\
                                  \runcmd:\n\
                                  \ - a b c\n"

         ud = YamlUserData
                (object
                   ["runcmd" .=
                    array [toJSON ("x y z"::String)
                          ,toJSON ("a b c"::String)]
                   ,"write_files" .=
                    array [object
                            ["contents" .=
                             toJSON ("hello world!\n"::String)
                            ,"path" .=
                             toJSON ("/sdf/xyz/filename.cfg"::String)
                            ,"owner" .=
                             toJSON ("root:root"::String)]
                          ,object
                            ["contents" .=
                             toJSON ("hello world2!\n"::String)
                            ,"path" .=
                             toJSON ("/sdf/xyz/filename.cfg"::String)
                            ,"owner" .=
                             toJSON ("root:root"::String)]]])
      in ud1 <> ud2 `shouldBe` ud

  describe "OTPSysConfig" $ do

    it "implements ConcatableSyntax method decodeSyntax" $
       let v = decodeSyntax "ok"
       in v `shouldBe` Right (OTPSysConfig (ErlAtom "ok"))

    it "implements ConcatableSyntax method encodeSyntax" $
       let v = encodeSyntax (OTPSysConfig (ErlAtom "ok"))
       in v `shouldBe` "ok"

    it "combines primitives by putting them in a list" $
       let p1 = OTPSysConfig (ErlList [ErlAtom "a"])
           p2 = OTPSysConfig (ErlList [ErlNatural 123])
           combined = OTPSysConfig
                        (ErlList [ErlAtom "a"
                                 ,ErlNatural 123])
       in (p1 <> p2) `shouldBe` combined

    it "combines a list and a primitve by extending the list" $
       let (Right l) = decodeSyntax "[a,b,c]" :: Either String OTPSysConfig
           (Right p) = decodeSyntax "{ok,value}"
           (Right combined) = decodeSyntax "[a,b,c,{ok,value}]"
       in l <> p `shouldBe` combined

    it "combines a primitve and a list by extending the list" $
       let (Right l) = decodeSyntax "[a,b,c]" :: Either String OTPSysConfig
           (Right p) = decodeSyntax "{ok,value}"
           (Right combined) = decodeSyntax "[{ok,value},a,b,c]"
       in p <> l `shouldBe` combined

    it "merges lists with distinct elements to lists containing the elements of both lists" $
       let p1 = OTPSysConfig
                  (ErlList
                     [ErlTuple [ErlAtom "k_p1"
                               ,ErlList [ErlNatural 1]]])
           p2 = OTPSysConfig
                  (ErlList
                     [ErlTuple [ErlAtom "k_p2"
                               ,ErlList [ErlNatural 1]]])
           expected = OTPSysConfig
                        (ErlList
                           [ErlTuple [ErlAtom "k_p1"
                                     ,ErlList [ErlNatural 1]]
                           ,ErlTuple [ErlAtom "k_p2"
                                     ,ErlList [ErlNatural 1]]])
       in p1 <> p2 `shouldBe` expected

    it "merges two property lists into a prop list that has the lenght\
       \ of the left + the right proplist - the number of entries sharing\
       \ the same key" (property mergedPropListsHaveCorrectLength)

data ErlPropListTestData =
  ErlPropListTestData { plistLeft :: [SimpleErlangTerm]
                      , plistRight :: [SimpleErlangTerm]
                      , commonKeys :: [SimpleErlangTerm]
                      } deriving (Eq,Ord,Show)

mergedPropListsHaveCorrectLength :: ErlPropListTestData -> Bool
mergedPropListsHaveCorrectLength (ErlPropListTestData l r common) =
  let (OTPSysConfig (ErlList merged)) =
       OTPSysConfig (ErlList l) <> OTPSysConfig (ErlList r)
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
        arbitraryPlistKey =
          ErlAtom <$> ((:) <$> letterLower <*> listOf nameChar)

        arbitraryPlistValue = oneof [arbitraryLiteral
                                    ,arbitraryList
                                    ,arbitraryPlist
                                    ,arbitraryTuple]
        arbitraryTuple = ErlTuple <$> listOf arbitraryPlistValue
        arbitraryList = ErlList <$> listOf arbitraryPlistValue
        arbitraryLiteral =
          oneof [arbitraryPlistKey,arbitraryString,arbitraryNumber]
        arbitraryString = ErlString <$> listOf (oneof [letter,digit])
        arbitraryNumber = oneof [arbitraryNatural, arbitraryFloat]
        arbitraryNatural = ErlNatural <$> arbitrary
        arbitraryFloat = ErlFloat <$> arbitrary
        letter = oneof [letterUpper,letterLower]
        letterUpper = elements ['A' .. 'Z']
        letterLower = elements ['a' .. 'z']
        digit = elements ['0' .. '9']
        nameChar = oneof [letter,digit,pure '_',pure '@']
