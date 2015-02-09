module B9.GeneratorSpec (spec) where

import Test.Hspec
import Control.Applicative
import Data.List
import Test.QuickCheck
import Data.Monoid
import Text.Printf
import B9.TestUtil

import B9.Generator

spec :: Spec
spec = do
  describe "addBindings" $ do
    let kv0 = ("k0","v0")

    it "appends an ':=' binding at the beginning of the list"
       (addBindings [kv0] ["k" := "v"] `shouldBe` [[("k","v"),kv0]])

    it "appends an ':<-' binding at the beginning of the list"
       (addBindings [kv0] [["k"] :<- [["v"]]] `shouldBe` [[("k","v"),kv0]])

    it "creates 3 environments when applied to '[k :<- [[v1,v2,v3]]'"
       (addBindings [kv0] [["k"] :<- [["v1"],["v2"],["v3"]]]
        `shouldBe`  [[("k","v1"),kv0],[("k","v2"),kv0],[("k","v3"),kv0]])

    it "creates 4 environments when applied to '[[k1] :<- [[v1],[v2]],\
       \ [k2] :<- [[v1],[v2]]'"
       (addBindings [] [["k1"] :<- [["v11"],["v12"]]
                       ,["k2"] :<- [["v21"],["v22"]]]
        `shouldBe`  [[("k2","v21"),("k1","v11")]
                    ,[("k2","v22"),("k1","v11")]
                    ,[("k2","v21"),("k1","v12")]
                    ,[("k2","v22"),("k1","v12")]])

    it "creates 4 environments when applied to '\
       \[k0 := v00,\
       \ [k1] :<- [[v1],[v2]],\
       \ [k2] :<- [[v1],[v2]]\
       \ k3 := v33]'"
       (addBindings [] ["k0" := "v0"
                       ,["k1"] :<- [["v11"],["v12"]]
                       ,["k2"] :<- [["v21"],["v22"]]
                       ,"k3" := "v3"]
        `shouldBe`  [[("k3","v3"),("k2","v21"),("k1","v11"),("k0","v0")]
                    ,[("k3","v3"),("k2","v22"),("k1","v11"),("k0","v0")]
                    ,[("k3","v3"),("k2","v21"),("k1","v12"),("k0","v0")]
                    ,[("k3","v3"),("k2","v22"),("k1","v12"),("k0","v0")]])

    it "binds every key in 'keys :<- valueSets'"
       (addBindings [] [["k1","k2"] :<- [["v11","v12"], ["v21","v22"]]]
        `shouldBe` [[("k1","v11"), ("k2","v12")]
                   ,[("k1","v21"), ("k2","v22")]])

    it "throws an error when in 'keys :<- valueSets' a valueSet has a \
       \different number of elements than 'keys'"
       (print (addBindings [] [["1","2"] :<- [["11","12"], ["21","22","XX"]]])
        `shouldThrow` anyErrorCall)

  describe "mappend" (it "is associative" (property associativity))

  describe "runGenerator" $ do
    let initialEnv = [("ie_k", "ie_v")]
    let let16 = Let [bindingsE14 "k1", bindingsE14 "k2"] [Build [()]]
        bindingsE14 key = [key] :<- [["1"],["2"],["3"],["4"]]

    it "creates a list of n items with the initial environment \
       \when applied to 'Build [x1 .. xn]'" $ property $ do
         items <- listOf (pure ())
         let expected = WithEnvironment initialEnv <$> items
         return (runGenerator (Build items) initialEnv == expected)

    it "creates nothing, i.e. '[]', when all 'Build [...]' elements are \
       \empty, i.e. 'Build []'"
       (property createNothingWithOnlyEmptyBuilds)

    it "creates 16 items when nesting two Lets with ':<-' bindings of 4 values"
       (length (runGenerator let16 []) `shouldBe` 16)


associativity :: (Generator (), Generator (), Generator ()) -> Bool
associativity (g1, g2, g3) =
   g1 <> (g2 <> g3) == (g1 <> g2) <> g3


createNothingWithOnlyEmptyBuilds :: EmptyBuildGenerator -> Bool
createNothingWithOnlyEmptyBuilds (EmptyBuildGenerator g) =
  null (runGenerator g [])

newtype EmptyBuildGenerator = EmptyBuildGenerator (Generator ())
  deriving (Show,Eq)

instance Arbitrary EmptyBuildGenerator where
  arbitrary = sized arbitraryNested
    where
      arbitraryNested 0 = return (EmptyBuildGenerator (Build []))
      arbitraryNested n = do
        nested <- resize (n `div` 2) arbitrary
        EmptyBuildGenerator <$>
          (Let <$> resize (n `div` 2) arbitrary
               <*> pure (map (\(EmptyBuildGenerator g) -> g) nested))

instance Arbitrary a => Arbitrary (Generator a) where
  arbitrary =
    oneof [Let <$> halfSized arbitrary <*> halfSized arbitrary
          ,Build <$> arbitrary]

instance Arbitrary Bindings where
  arbitrary = oneof [ do k <- halfSized arbitraryKey
                         v <- halfSized arbitraryValue
                         return (k := v)
                    , do r <- choose (0,5)
                         ks <- halfSized (vectorOf r arbitraryKey)
                         vs <- halfSized (listOf (vectorOf r arbitraryValue))
                         return (ks :<- vs)
                    ]
    where
      arbitraryKey = do k1 <- halfSized
                               (elements ["host", "bus", "user", "brand", "ip"])
                        k2 <- halfSized
                               (elements ["name", "network", "id", "server"])
                        return (intercalate "_" [k1,k2])
      arbitraryValue = do v1 <- halfSized
                                 (elements ["www", "db", "erts", "mrf", "sbc", "root"])
                          v2 <- halfSized
                                 (elements ["test", "prod", "lab", "failover"])
                          v3 <- halfSized
                                 (elements (printf "%.2i" <$> [0..4::Int]))
                          return (intercalate "_" [v1,v2,v3])
