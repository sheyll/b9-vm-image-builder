{-# LANGUAGE OverloadedStrings #-}
module B9.Content.YamlObjectSpec (spec) where

import Test.Hspec
import Data.Semigroup
import Data.Text ()
import Data.Yaml

import B9.Content.YamlObject
import B9.Content.AST

spec :: Spec
spec = do
  describe "YamlObject" $ do

    it "combines primitives by putting them in an array" $
       let v1 = YamlObject (toJSON True)
           v2 = YamlObject (toJSON (123::Int))
           combined = YamlObject (array [toJSON True
                                          ,toJSON (123::Int)])
       in (v1 <> v2) `shouldBe` combined

    it "combines objects with disjunct keys to an object \
       \containing all properties" $
       let plist1 = YamlObject (object ["k1" .= Number 1])
           plist2 = YamlObject (object ["k2" .= Number 2])
           combined = YamlObject (object ["k1" .= Number 1
                                           ,"k2" .= Number 2])
       in (plist1 <> plist2) `shouldBe` combined

    it "combines arrays by concatenating them" $
       let v1 = YamlObject (array [toJSON ("x"::String)])
           v2 = YamlObject (array [toJSON ("y"::String)])
           combined = YamlObject (array [toJSON ("x"::String)
                                        ,toJSON ("y"::String)])
       in (v1 <> v2) `shouldBe` combined

    it "combines objects to a an object containing\
       \ all disjunct entries and combined entries\
       \ with the same keys" $
       let o1 = YamlObject (object ["k1" .= Number 1
                                   ,"k" .= Number 2])
           o2 = YamlObject (object ["k2" .= Number 3
                                   ,"k" .= Number 4])
           combined =
             YamlObject (object ["k1" .= Number 1
                                ,"k2" .= Number 3
                                ,"k" .= array [Number 2
                                              ,Number 4]])
       in (o1 <> o2) `shouldBe` combined

    it "combines 'write_files' and 'runcmd' from typical 'user-data' files \
       \by merging each" $
     let (Right ud1) = decodeSyntax "" "#user-data\n\
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

         (Right ud2) = decodeSyntax "" "#user-data\n\
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

         ud = YamlObject
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


--    describe "fromAST YamlObject" $ do
--
--      it "returns x from (AST x)" $
--         let x = (object [])
--             lift :: a -> ReaderT Environment IO a
