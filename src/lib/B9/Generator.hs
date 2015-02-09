{-# LANGUAGE FlexibleContexts #-}
module B9.Generator
  (Generator(..)
  ,Environment
  ,WithEnvironment (..)
  ,Bindings (..)
  ,runGenerator
  ,runGeneratorM
  ,addBindings
  ) where

import Data.Data
import Control.Applicative
import Data.Monoid
import Control.Monad.Reader
import Text.Show.Pretty
import Text.Printf

-- | In the B9-world, a 'Generator' describes how to build artifacts optionally
-- parameterized by parameters stored in an environment.
data Generator a = Let [Bindings] [Generator a]
                 | Build [a]
                 deriving (Read, Show, Typeable, Data, Eq)


-- | Variable Bindings
data Bindings = String := String
              | [String] :<- [[String]]
  deriving (Read, Show, Typeable, Data, Eq)

infixl 4 :<-
infixl 4 :=

-- | Generate a list of 'WithEnvironment a' values, substitute all variables
-- (i.e. '$...') in values using the keys already defined.
runGenerator :: Generator a -> Environment -> [WithEnvironment a]
runGenerator = runReader . runGeneratorM

-- | Generate a list of 'WithEnvironment a' values, substitute all variables
-- (i.e. '$...') in values using the keys already defined.
runGeneratorM :: (Functor m, MonadReader Environment m)
              => Generator a -> m [WithEnvironment a]
runGeneratorM g = do
  case g of
    Build xs -> do
      env <- ask
      return (WithEnvironment env <$> xs)

    Let bs gs -> do
      env <- ask
      let envs = addBindings env bs
      join <$> mapM (\e -> local (const e)
                                 (join <$> mapM runGeneratorM gs))
                    envs



-- | Extend an environment by adding 'Bindings' to it. New entries are always
-- the beginning of the list.
addBindings :: Environment -> [Bindings] -> [Environment]
addBindings env [] = [env]
addBindings env (x:rest) = do
  bs <- case x of
          k := v ->
            return [(k,v)]
          ks :<- vss -> do
            addEach ks vss

  addBindings (bs ++ env) rest
  where
    addEach vars valueSets =
      if all ((== length vars) . length) valueSets
         then do vs <- valueSets
                 return (vars `zip` vs)
         else (error (printf "Error in ':<-' bindings in:\n '%s'.\
                             \\n\nThe variable list has %i entries, \
                             \but this binding set\n%s\n\n\
                             \has a different number of entries!\n"
                             (ppShow (vars :<- valueSets))
                             (length vars)
                             (ppShow (head (dropWhile ((== length vars) . length)
                                                      valueSets)))))

-- | A list of variable names to variable values. Both are 'String's.
type Environment = [(String, String)]

-- | Generator ouput type.
data WithEnvironment a = WithEnvironment Environment a
  deriving (Read, Show, Typeable, Data, Eq)

instance Monoid (Generator a) where
  mempty = Build []
  Build [] `mappend` x = x
  x `mappend` Build [] = x
  (Let [] ls) `mappend` (Let [] rs) = Let [] (ls ++ rs)
  x `mappend` (Let [] rs) = Let [] (x:rs)
  (Let [] ls) `mappend` y = Let [] (ls++ [y])
  x `mappend` y = Let [] [x,y]


-- produce :: (MonadError String m) => Generator a ->
