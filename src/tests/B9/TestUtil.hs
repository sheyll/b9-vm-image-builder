module B9.TestUtil (halfSized) where

import Test.QuickCheck

halfSized :: Gen a -> Gen a
halfSized g = sized (\s -> resize (s `div` 2) g)
