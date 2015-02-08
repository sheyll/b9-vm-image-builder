 module B9.Generator
  (Generator(..)
  ,Environment
  ,WithEnvironment (..)
  ) where

import Data.Data
import Data.Monoid

-- | In the B9-world, a 'Generator' describes how to build artifacts optionally
-- parameterized by parameters stored in an environment.
data Generator a = Let Environment [Generator a]
                 | LetX [(String, [String])] [Generator a]
                 | EachT [String] [[String]] [Generator a]
                 | Each [(String,[String])] [Generator a]
                 | Build [a]
                 | With [Bindings] [Generator a]
                 | Led [Bindings]
                 | Generator a :> Generator a
                 | Generator a :$ [a]
                 deriving (Read, Show, Typeable, Data, Eq)

infixl 4 :<-
infixl 4 :=
infixr 3 :$
infixr 2 :>


data Bindings = String := String
              | [String] :<- [[String]]
  deriving (Read, Show, Typeable, Data, Eq)

xxx' = With ["x" := "sdf"] [Led ["y" := "fd3"] :$ ["$x", "$y"]]

-- | A list of variable names to variable values. Both are 'String's.
type Environment = [(String, String)]

-- | Generator ouput type.
data WithEnvironment a = WithEnvironment Environment a
  deriving (Read, Show, Typeable, Data, Eq)

instance Monoid (Generator a) where
  mempty = Build []
  Build [] `mappend` x = x
  x `mappend` Build [] = x
  x `mappend` y = Let [] [x, y]


-- produce :: (MonadError String m) => Generator a ->
