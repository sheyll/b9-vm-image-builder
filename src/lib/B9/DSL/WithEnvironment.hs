{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
module B9.DSL.WithEnvironment where

import Data.Data
import GHC.Generics (Generic)
import Control.Monad.RWS
import Data.Semigroup as Sem
import Control.Monad.Free

type BindTo e a = (e, a)

bindTo :: e -> a -> BindTo e a
bindTo e a = (e, a)

-- | A type alias to improve readability in the definition of 'WithEnvironment'.
type Bindings k v = [BindTo k v]

type WithEnvironment k v a r = Free (WithEnvironmentF k v a) r

generateBindings
    :: WithEnvironment k v [a] () -> Bindings k v -> Bindings (Bindings k v) a
generateBindings mx initialEnv =
    let (_res, _s, w) = runRWS (iterM go mx) initialEnv () in w
  where
    go (SetEnvironment e r) = do
        local (`mappend` e) r
    go (IterateEnvironments e r) = do
        let es = sequence $ sequence <$> e
        mapM_ (\e' -> local (`mappend` e') r) es
    go (InEnvironment x r) = do
        env <- ask
        tell (sequence (bindTo env x))
        r

inEnvironment :: a -> WithEnvironment k v a ()
inEnvironment x = liftF (InEnvironment x ())

($=) :: k -> v -> WithEnvironment k v a ()
k $= v = setEnviroment [k `bindTo` v]

setEnviroment :: Bindings k v -> WithEnvironment k v a ()
setEnviroment e = liftF (SetEnvironment e ())

($<<) :: k -> [v] -> WithEnvironment k v a ()
k $<< vs = iterateEnvironments [k `bindTo` vs]

iterateEnvironments :: Bindings k [v] -> WithEnvironment k v a ()
iterateEnvironments e = liftF (IterateEnvironments e ())

data WithEnvironmentF k v a r =
    InEnvironment a r
    -- ^ InEnvironment several outputs
    | SetEnvironment (Bindings k v) r
    -- ^ Extend the environment
    | IterateEnvironments (Bindings k [v]) r
    -- ^ A 'SetEnvironment' where each variable is assigned to each
    -- value; the nested generator is executed for each
    -- permutation.
    --
    -- @
    --     IterateEnvironments [("x", ["1","2","3"]), ("y", ["a","b"])] r
    -- @
    -- Is equal to:
    --
    -- @
    --     InEnvironment [
    --       SetEnvironment [("x", "1"), ("y", "a")] r
    --       SetEnvironment [("x", "1"), ("y", "b")] r
    --       SetEnvironment [("x", "2"), ("y", "a")] r
    --       SetEnvironment [("x", "2"), ("y", "b")] r
    --       SetEnvironment [("x", "3"), ("y", "a")] r
    --       SetEnvironment [("x", "3"), ("y", "b")] r
    --     ]
    -- @
      deriving (Data, Typeable, Generic, Eq, Ord, Show, Read, Functor,
                Foldable, Traversable)
