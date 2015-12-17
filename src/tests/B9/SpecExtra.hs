{- | Extra utilities for HSpec based unit tests -}
module B9.SpecExtra
       (should've, toList, toListIo, shouldDo, does, shouldDoIo,
        shouldDoIoNoBS, noBS, shouldResultIn)
       where

import B9.B9IO
import B9.B9IO.DslCompiler
import Control.Monad
import Control.Monad.State
import Data.Function
import Data.List
import Test.Hspec
import Data.Default

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

-- | Assertion on the result of a 'IoCompiler'
shouldResultIn :: (Eq a, Show a) => IoCompiler a -> a -> Expectation
shouldResultIn prog =
    shouldBe (dumpToResult (evalStateT prog def))

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

-- | Expect that a 'Program' contains at least a given 'IoProgram' - minus the
-- noise
shouldDoIoNoBS :: Show a => Program a -> IoProgram b -> Expectation
shouldDoIoNoBS actual expected = do
    putStrLn $ dumpToResult $ inspect actual
    (noBS $ toList actual) `should've` (noBS $ toListIo expected)

-- | Remove noise from IoProgram listings.
noBS :: [String] -> [String]
noBS =
    filter (not . ("logTrace" `isPrefixOf`)) .
    filter (not . ("mkDir" `isPrefixOf`)) .
    filter (not . ("getRealPath" `isPrefixOf`)) .
    filter (not . ("getParentDir" `isPrefixOf`)) .
    filter (not . ("getFileName" `isPrefixOf`))
