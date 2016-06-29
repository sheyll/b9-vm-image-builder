module B9Spec.Prelude(module B9Spec.Prelude, module X) where

import Test.Hspec as X
import Test.QuickCheck as X
import B9.Core.Prelude as X hiding ((.&.))

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
