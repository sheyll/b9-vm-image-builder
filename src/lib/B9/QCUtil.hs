{-# LANGUAGE ScopedTypeVariables #-}
{-|
Some QuickCheck utility functions.
-}
module B9.QCUtil where

import Control.Monad
import Control.Monad.Free
import Test.QuickCheck

arbitraryEnv
    :: Arbitrary a
    => Gen [(String, a)]
arbitraryEnv = listOf ((,) <$> listOf1 (choose ('a', 'z')) <*> arbitrary)

halfSize :: Gen a -> Gen a
halfSize g = sized (flip resize g . flip div 2)

smaller :: Gen a -> Gen a
smaller g = sized (flip resize g . max 0 . flip (-) 1)

arbitraryFilePath :: Gen FilePath
arbitraryFilePath = do
    path <-
        join <$>
        listOf
            (elements
                 [ "/"
                 , "../"
                 , "./"
                 , "etc/"
                 , "opt/"
                 , "user/"
                 , "var/"
                 , "tmp/"
                 , "doc/"
                 , "share/"
                 , "conf.d/"])
    prefix <- elements ["foo_", "", "alt_", "ssh-", ""]
    body <- elements ["www", "passwd", "cert", "opnsfe", "runtime"]
    extension <- elements [".txt", ".png", ".ps", ".erl", ""]
    return (path ++ prefix ++ body ++ extension)

arbitraryLetter :: Gen Char
arbitraryLetter = oneof [arbitraryLetterUpper, arbitraryLetterLower]

arbitraryLetterUpper :: Gen Char
arbitraryLetterUpper = elements ['A' .. 'Z']

arbitraryLetterLower :: Gen Char
arbitraryLetterLower = elements ['a' .. 'z']

arbitraryDigit :: Gen Char
arbitraryDigit = elements ['0' .. '9']

arbitraryNiceString :: Gen String
arbitraryNiceString =
    listOf $ oneof [arbitraryDigit, arbitraryLetter, pure ' ']

arbitraryFree
    :: (Functor f, Arbitrary a, Arbitrary (f a), Arbitrary (f ()))
    => Gen (Free f a)
arbitraryFree = oneof [Pure <$> smaller arbitrary,
                       do
                         (x :: f ()) <- smaller arbitrary
                         rest <- smaller arbitraryFree
                         return (Free (fmap (const rest) x))]
