{-# LANGUAGE OverloadedStrings #-}
module B9.ErlTermsSpec (spec) where

import Control.Applicative
import Data.List
import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
import B9.ErlTerms
import qualified Data.ByteString.Char8 as B

spec :: Spec
spec = do
  describe "parseErlTerm" $ do
    it "parses a non-empty string"
       (parseErlTerm "test" "\"hello world\""
        `shouldBe` (Right (ErlString "hello world")))

    it "parses a string with escaped characters"
       (parseErlTerm "test" "\"\\b\\^A\""
        `shouldBe` (Right (ErlString "\b\^A")))

    it "parses a string with escaped octals: \\X"
       (parseErlTerm "test" "\"\\7\""
        `shouldBe` (Right (ErlString "\o7")))

    it "parses a string with escaped octals: \\XY"
       (parseErlTerm "test" "\"\\73\""
        `shouldBe` (Right (ErlString "\o73")))

    it "parses a string with escaped octals: \\XYZ"
       (parseErlTerm "test" "\"\\431\""
        `shouldBe` (Right (ErlString "\o431")))

    it "parses a string with escaped hex: \\xNN"
       (parseErlTerm "test" "\"\\xbE\""
        `shouldBe` (Right (ErlString "\xbe")))

    it "parses a string with escaped hex: \\x{N}"
       (parseErlTerm "test" "\"\\x{a}\""
        `shouldBe` (Right (ErlString "\xa")))

    it "parses a string with escaped hex: \\x{N}"
       (parseErlTerm "test" "\"\\x{2}\""
        `shouldBe` (Right (ErlString "\x2")))

    it "parses a string with escaped hex: \\x{NNNNNN...}"
       (parseErlTerm "test" "\"\\x{000000Fa}\""
        `shouldBe` (Right (ErlString "\xfa")))

    it "parses strings"
       (property
         (do str <- sized arbitraryErlString
             parsedTerm <-
              case parseErlTerm "test" (B.pack ("\"" ++ str ++ "\"")) of
                Left e -> fail e
                Right s -> return s
             return (ErlString str == parsedTerm)))

    it "parses decimal literals"
       (property
          (do decimal <- arbitrary `suchThat` (>= 0)
              let decimalStr = B.pack (show (decimal :: Integer))
              parsedTerm <- case parseErlTerm "test" decimalStr of
                                 (Left e) -> fail e
                                 (Right parsedTerm) -> return parsedTerm
              return (ErlNatural decimal == parsedTerm)))

    it "parses decimal literals with radix notation"
       (property
          (do radix <- choose (2, 36)
              dsi <- listOf1 (choose (0, radix - 1))
              let (Right parsedTerm) = parseErlTerm "test" decimalStr
                  decimalStr = B.pack ((show radix) ++ "#" ++ ds)
                  expected = convertStrToDecimal radix ds
                  ds = (naturals !!) <$> dsi
              return (ErlNatural expected == parsedTerm)))

    it "parses all rendered terms"
       (property (\term ->
                    let (Right parsedTerm) = parseErlTerm "test" renderedTerm
                        (Right renderedTerm) = renderErlTerm term
                    in parsedTerm == term))


naturals = ['0'..'9'] ++ ['a' .. 'z']

convertStrToDecimal :: Int -> [Char] -> Integer
convertStrToDecimal radix =
  foldl (\acc d ->
        let (Just ix) = elemIndex d naturals
        in acc * (toInteger radix) + (toInteger ix))
        0

arbitraryErlString :: Int -> Gen String
arbitraryErlString 0 = return ""
arbitraryErlString n = (++)
                       <$> arbitraryErlStringChunk
                       <*> arbitraryErlString (n-1)
  where
    arbitraryErlStringChunk = return <$> (choose ('a','z'))
