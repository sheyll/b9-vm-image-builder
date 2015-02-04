{-# LANGUAGE OverloadedStrings #-}
module B9.ErlTermsSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.List
import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import B9.ErlTerms
import qualified Data.ByteString.Char8 as B
import Text.Printf

spec :: Spec
spec = do
  describe "parseErlTerm" $ do
    it "parses a non-empty string"
       (parseErlTerm "test" "\"hello world\""
        `shouldBe` Right (ErlString "hello world"))

    it "parses a string with escaped characters"
       (parseErlTerm "test" "\"\\b\\^A\""
        `shouldBe` Right (ErlString "\b\^A"))

    it "parses a string with escaped octals: \\X"
       (parseErlTerm "test" "\"\\7\""
        `shouldBe` Right (ErlString "\o7"))

    it "parses a string with escaped octals: \\XY"
       (parseErlTerm "test" "\"\\73\""
        `shouldBe` Right (ErlString "\o73"))

    it "parses a string with escaped octals: \\XYZ"
       (parseErlTerm "test" "\"\\431\""
        `shouldBe` Right (ErlString "\o431"))

    it "parses a string with escaped hex: \\xNN"
       (parseErlTerm "test" "\"\\xbE\""
        `shouldBe` Right (ErlString "\xbe"))

    it "parses a string with escaped hex: \\x{N} (1)"
       (parseErlTerm "test" "\"\\x{a}\""
        `shouldBe` Right (ErlString "\xa"))

    it "parses a string with escaped hex: \\x{N} (2)"
       (parseErlTerm "test" "\"\\x{2}\""
        `shouldBe` Right (ErlString "\x2"))

    it "parses a two digit octal followed by a non-octal digit"
       (parseErlTerm "test" "\"\\779\"" `shouldBe` Right (ErlString "\o77\&9"))

    it "parses a string with escaped hex: \\x{NNNNNN...}"
       (parseErlTerm "test" "\"\\x{000000Fa}\""
        `shouldBe` Right (ErlString "\xfa"))

    it "parses decimal literals"
       (property
          (do decimal <- arbitrary `suchThat` (>= 0)
              let decimalStr = B.pack (show (decimal :: Integer))
              parsedTerm <- case parseErlTerm "test" decimalStr of
                                 (Left e) -> fail e
                                 (Right parsedTerm) -> return parsedTerm
              return (ErlNatural decimal == parsedTerm)))

    it "parses a negative signed decimal"
       (parseErlTerm "test" "-1" `shouldBe` Right (ErlNatural (-1)))

    it "parses a positive signed decimal"
       (parseErlTerm "test" "+1" `shouldBe` Right (ErlNatural 1))

    it "parses decimal literals with radix notation"
       (property
          (do radix <- choose (2, 36)
              digitsInRadix <- listOf1 (choose (0, radix - 1))
              let (Right parsedTerm) = parseErlTerm "test" erlNumber
                  erlNumber = B.pack (show radix ++ "#" ++ digitChars)
                  expected = convertStrToDecimal radix digitChars
                  digitChars = (naturals !!) <$> digitsInRadix
              return (ErlNatural expected == parsedTerm)))

    it "parses a floating point literal with exponent and sign"
       (parseErlTerm "test" "-10.40E02" `shouldBe` Right (ErlFloat (-10.4e2)))

    it "parses a simple erlang character literal"
       (parseErlTerm "test" "$ " `shouldBe` Right (ErlChar (toEnum 32)))

    it "parses an erlang character literal with escape sequence"
       (parseErlTerm "test" "$\\x{Fe}" `shouldBe` Right (ErlChar (toEnum 254)))

    it "parses an unquoted atom with @ and _"
       (parseErlTerm "test" "a@0_T" `shouldBe` Right (ErlAtom "a@0_T"))

    it "parses a quoted atom with letters, spaces and special characters"
       (parseErlTerm "test" "' $s<\\\\.0_=@\\e\\''" `shouldBe` Right (ErlAtom " $s<\\.0_=@\ESC'"))

    it "parses a binary literal containing a string"
       (parseErlTerm "test" "<<\"1 ok!\">>" `shouldBe` Right (ErlBinary "1 ok!"))

    it "parses an empty binary"
       (parseErlTerm "test" "<<>>" `shouldBe` Right (ErlBinary ""))

    it "parses all rendered terms"
       (property parsesRenderedTerms)

  describe "renderErlTerm" $ do
    it "renders an empty binary as \"<<>>\""
       (renderErlTerm (ErlBinary "") `shouldBe` "<<>>")

    it "renders an erlang character"
       (renderErlTerm (ErlChar 'a') `shouldBe` "$a")

    it "renders a quoted atom and escapes special characters"
       (renderErlTerm (ErlAtom " $s\"<\\.0_=@\ESC'")
       `shouldBe` "' $s\"<\\\\.0_=@\\x{1b}\\''")

    it "renders a string and escapes special characters"
       (renderErlTerm (ErlString "' $s\"<\\.0_=@\ESC''")
       `shouldBe` "\"' $s\\\"<\\\\.0_=@\\x{1b}''\"")

parsesRenderedTerms :: SimpleErlangTerm -> Bool
parsesRenderedTerms term =
  either error (term ==) (parseErlTerm "test" (renderErlTerm term))

naturals :: String
naturals = ['0'..'9'] ++ ['a' .. 'z']

convertStrToDecimal :: Int -> String -> Integer
convertStrToDecimal radix digitChars =
  let hornersMethod acc d = acc * radixHighPrecision + digitCharToInteger d
      digitCharToInteger d = toInteger $ fromJust $ elemIndex d naturals
      radixHighPrecision = toInteger radix
  in foldl hornersMethod 0 digitChars
