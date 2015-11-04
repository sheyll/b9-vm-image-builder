{- | Extra utilities for HSpec based unit tests -}
module B9.SpecExtra
       (should've, toList, toListIo, shouldDo, does,
        shouldDoIo)
       where

import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter
import Control.Monad
import Data.Function
import Data.List
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

-- | Compile and dump a 'Program' to a string list.
toList :: Program a -> [String]
toList = toListIo . compile

-- | Dump an 'IoProgram' to a string list.
toListIo :: IoProgram a -> [String]
toListIo = dumpToStrings . void

-- | Expect that two programs have the same effects when executed.
shouldDo :: Program a -> Program a -> IO ()
shouldDo = shouldContain `on` (dumpToStrings . compile)

-- | Like 'shouldDo' but as pure predicate.
does
    :: Eq a
    => Program a -> Program a -> Bool
does = isInfixOf `on` (dumpToStrings . compile)

-- | Expect that a 'Program' contains at least a given 'IoProgram'.
shouldDoIo :: Program a -> IoProgram b -> Expectation
shouldDoIo actual expected = (toList actual) `should've` (toListIo expected)
