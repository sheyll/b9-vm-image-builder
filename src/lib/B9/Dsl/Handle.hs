-- | A 'Proxy' like data type to references resources throughout the B9 DSL.

module B9.Dsl.Handle (Handle(handleTitle, handleType)
                     ,globalHandle,globalHandleP,globalHandleS
                     ,mkHandle,mkHandleP,mkHandleS
                     ,handleTitle
                     ,SomeHandle(..)
                     ,module X) where

import B9.Common as X
import B9.Logging as X
import Data.Singletons

-- * Handle for artifacts.

-- | In order to indicate references and dependencies between artifacts, e.g.
--   disk images and lxc execution, inside the (pure and declaretive) 'ProgramT',
-- _handles_ are used, instead of actual artifact specific values (like
-- e.g. file handles, network sockets,...). Handles put an additional layer of
-- indirection and proctection around an actual artifact representation. A
-- handle is a proxy for an artifact singleton type paired with a string
-- identifieing the runtime value referenced to by the handle.
data Handle (a :: k) =
  Handle {handleType :: String, handleTitle :: String}
  deriving (Eq, Ord)

instance Show (Handle a) where
  show (Handle tt it) = tt ++ "_AT_" ++ it

instance LogArg (Handle a)

-- | Make a handle for some global/static pseudo artifact, like e.g. logging.
-- Specifically, create a handle for types of artifacts that are only ever
-- inhabited by a single value, by simply @show@ing the singleton.
globalHandle :: String -> Handle a
globalHandle p = mkHandle p p

globalHandleP :: Show (p a) => p a -> Handle a
globalHandleP = globalHandle . show

globalHandleS :: (SingKind ('KProxy :: KProxy k), Show (Demote (a :: k)))
              => Sing a -> Handle a
globalHandleS = globalHandle . show . fromSing

-- | Generate a handle with formatted title
mkHandle :: String -> String -> Handle a
mkHandle hType hInst = Handle hType hInst

-- | Generate a handle with formatted title
mkHandleP :: Show (p a) => p a -> String -> Handle a
mkHandleP = mkHandle . show

-- | Generate a handle with formatted title from a 'Sing' instance
mkHandleS :: (SingKind ('KProxy :: KProxy k), Show (Demote (a :: k)))
             => Sing a -> String -> Handle a
mkHandleS = mkHandle . show . fromSing

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
