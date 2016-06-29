-- | A 'Proxy' like data type to references resources throughout the B9 DSL.
module B9.Core.Dsl.Types.Handle
       (Handle(handleTitle, handleType), globalHandle, mkHandle,
        SomeHandle(..))
       where

import B9.Core.Prelude
import B9.Core.Util.Logging

-- | In order to indicate references and dependencies between artifacts, e.g.
-- disk images and lxc execution, inside the (pure and declaretive) 'ProgramT',
-- _handles_ are used, instead of actual artifact specific values (like e.g.
-- file handles, network sockets,...). Handles put an additional layer of
-- indirection and proctection around an actual artifact representation. A
-- handle is a proxy for an artifact singleton type paired with a string
-- identifieing the runtime value referenced to by the handle.
data Handle (a :: k) where
  Handle :: Typeable a => {handleType :: TypeRep
                          ,handleTitle :: String} -> Handle a
  deriving (Typeable)

instance Show (Handle a) where
  show (Handle tt it) = it ++ ": " ++ show tt

instance LogArg (Handle a)

-- | Make a handle for some global/static pseudo artifact, like e.g. logging.
-- Specifically, create a handle for types of artifacts that are only ever
-- inhabited by a single value, by simply @show@ing the singleton.
globalHandle :: forall a . Typeable a => Handle a
globalHandle = mkHandle (show (typeRep (Proxy :: Proxy a)))

-- | Generate a handle with formatted title
mkHandle :: forall a . Typeable a => String -> Handle a
mkHandle title = Handle (typeRep (Proxy :: Proxy a)) title

-- | A wrapper around 'Handle' with existential quantification over the
-- artifact type.
data SomeHandle where
  SomeHandle :: Typeable a => Handle a -> SomeHandle

instance Show SomeHandle where
  show (SomeHandle h) = "<" ++ show h ++ ">"

instance Eq SomeHandle where
  (SomeHandle h@(Handle s t)) == (SomeHandle  h'@(Handle s' t')) =
    (typeRep h,s,t) == (typeRep h',s',t')

instance Ord SomeHandle where
  compare (SomeHandle h@(Handle s t))(SomeHandle  h'@(Handle s' t')) =
    compare (typeRep h,s,t) (typeRep h',s',t')
