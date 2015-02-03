module B9.ErlTerms (parseErlTerm
                   ,renderErlTerm
                   ,SimpleErlangTerm(..)) where

import Data.Data
import Data.Function
import qualified Data.ByteString.Char8 as B
import Text.Parsec.ByteString
import Text.Parsec
import Text.Parsec.Char
import Test.QuickCheck
import Control.Applicative ((<$>), pure)
import Text.Show.Pretty
import Control.Monad

data SimpleErlangTerm = ErlString String
                      | ErlFloat Double
                      | ErlNatural Integer
                      | ErlAtom String
                      | ErlChar Char
                      | ErlBinary B.ByteString
                      | ErlTuple [SimpleErlangTerm]
                      | ErlList [SimpleErlangTerm]
                      deriving (Eq,Ord,Read,Show,Data,Typeable)

parseErlTerm :: String -> B.ByteString -> Either String SimpleErlangTerm
parseErlTerm src content =
  either (Left . ppShow) Right (parse erlTermParser src content)

renderErlTerm :: SimpleErlangTerm -> Either String B.ByteString
renderErlTerm str = Left "TODO"

instance Arbitrary SimpleErlangTerm where
  arbitrary = oneof [sized aErlString
                    -- ,sized aErlFloat
                    ]
    where
      aErlString n = ErlString <$> resize (n-1) (listOf arbitraryBoundedRandom)
      aErlFloat n = ErlFloat <$> resize (n-1) arbitrary



erlTermParser :: Parser SimpleErlangTerm
erlTermParser = erlStringParser <|> erlNaturalParser

erlNaturalParser :: Parser SimpleErlangTerm
erlNaturalParser = decimalLiteral
  where
    decimalLiteral =
      foldr (\radix acc ->
               (try (string (show radix ++ "#"))
                >> ErlNatural . calcBE (toInteger radix)
                    <$> many1 (erlDigits radix))
               <|> acc)
            ((ErlNatural . calcBE 10) <$> many1 (erlDigits 10))
            [2..36]

    calcBE a = foldl (\acc d -> a * acc + d) 0
    erlDigits k = choice (take k digitTable)
    digitTable =
      map (\(cs,v) -> choice (char <$> cs) >> return v)
          (zip
             ((pure <$> ['0' .. '9'])
              ++ (zipWith ((++) `on` pure) ['a' .. 'z'] ['A' .. 'Z']))
             [0..])

erlStringParser :: Parser SimpleErlangTerm
erlStringParser = do
  char '"'
  str <- many chars
  char '"'
  return (ErlString str)
  where
    chars = escaped <|> noneOf "\""
    escaped = do
      char '\\'
      >> (do char '^'
             choice (zipWith escapedChar ccodes creplacements)

          <|>
          do char 'x'
             (do ds <- between (char '{') (char '}') (fmap hexVal <$> many1 hexDigit)
                 let val = foldl (\acc v -> acc * 16 + v) 0 ds
                 when (val > (fromEnum (maxBound :: Char)))
                      (fail "value out of range!")
                 return (toEnum val)
              <|>
              do x1 <- hexVal <$> hexDigit
                 x2 <- hexVal <$> hexDigit;
                 return (toEnum ((x1*16)+x2)))

          <|>
          do o1 <- octVal <$> octDigit
             (do o2 <- octVal <$>  octDigit
                 (do o3 <- octVal <$>  octDigit
                     return (toEnum ((((o1*8)+o2)*8)+o3))
                  <|>
                  return (toEnum (((o1*8)+o2))))
              <|>
              return (toEnum o1))

          <|>
          choice (zipWith escapedChar codes replacements))
      where
        escapedChar code replacement = char code >> return replacement
        codes =
          ['0'   , 'b'  , 'd'  , 'e'  , 'f' , 'n' , 'r' , 's' , 't' , 'v' ,'\\','\"','/']
        replacements =
          ['\NUL', '\BS','\DEL','\ESC','\FF','\LF','\CR','\SP','\HT','\VT','\\','\"','/']
        ccodes =
          ['a' .. 'z'] ++ ['A' .. 'Z']
        creplacements =
          cycle ['\^A' .. '\^Z']
        hexVal v | v `elem` ['a' .. 'z'] = 0xA + (fromEnum v - fromEnum 'a')
                 | v `elem` ['A' .. 'Z'] = 0xA + (fromEnum v - fromEnum 'A')
                 | otherwise = fromEnum v - fromEnum '0'
        octVal = hexVal
