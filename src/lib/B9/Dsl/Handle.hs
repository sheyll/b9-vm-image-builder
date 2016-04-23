-- | A 'Proxy' like data type to references resources throughout the B9 DSL.

module B9.Dsl.Handle (module B9.Dsl.Handle, module X) where

import B9.Common as X
import B9.Logging as X

-- * Handle for artifacts.

-- | In order to indicate references and dependencies between artifacts, e.g.
--   disk images and lxc execution, inside the (pure and declaretive) 'ProgramT',
-- _handles_ are used, instead of actual artifact specific values (like
-- e.g. file handles, network sockets,...). Handles put an additional layer of
-- indirection and proctection around an actual artifact representation. A
-- handle is a proxy for an artifact singleton type paired with a string
-- identifieing the runtime value referenced to by the handle.
data Handle (a :: k) where
    -- | Create a 'Handle' that contains a string.
    Handle :: p a -> String -> Handle a

instance Eq (Handle (a :: k)) where
    (Handle _ t) == (Handle _ t') = t == t'
instance Ord (Handle (a :: k)) where
    compare (Handle _ t) (Handle _ t') = compare t t'

instance {-# OVERLAPPING #-} KnownSymbol a => Show (Handle a) where
    show (Handle p t) = symbolVal p ++ "//" ++ t

instance {-# OVERLAPPABLE #-} Show (Handle a) where
    show (Handle _ t) = "//" ++ t

instance LogArg (Handle a)

-- | Make a handle for some global/static pseudo artifact, like e.g. logging.
-- Specifically, create a handle for types of artifacts that are only ever
-- inhabited by a single value, by simply @show@ing the singleton.
globalHandle :: Show (p a) => p a -> Handle a
globalHandle p = Handle p (show p)

-- | A wrapper around 'Handle' with existential quantification over the
-- artifact type.
data SomeHandle where
        SomeHandle :: Handle a -> SomeHandle

instance Show SomeHandle where
    show (SomeHandle h) = "<" ++ show h ++ ">"

instance Eq SomeHandle where
    (==) = (==) `on` show

instance Ord SomeHandle where
    compare = compare `on` show
