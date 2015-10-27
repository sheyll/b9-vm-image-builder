module B9.BuilderSpec (spec) where

import B9
import B9.DSL
import Test.Hspec
import Test.QuickCheck
import Text.Printf

spec :: Spec
spec =
    describe "defaultMain" $
    do it "logs 'LogTrace'" $
           do res <- runProgramWithConfigAndCliArgs generateCloudInitDir
              res `shouldBe` True


generateCloudInitDir :: Program ()
generateCloudInitDir =
  do doc "test"
     doc "test-2"
     c <- newCloudInit "app1.lab.qa.lbaum.eu"
     writeCloudInitDir c "/tmp/app1.lab.qa.lbaum.eu"
     addFile
            c
            (fileSpec "/etc/passwd")
            (FromTextFile (Source NoConversion "/etc/passwd"))
