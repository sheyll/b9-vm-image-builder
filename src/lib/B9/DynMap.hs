-- | A (pseudo-)typesafe dynamic map.
module B9.DynMap
    (DynMap
    ,Key(..)
    ,DynMapValue
    ,DynMapEntry
    ,empty
    ,singleton
    ,insert
    ,insertWith
    ,alter
    ,lookup
    ) where

import Prelude hiding (lookup)
import B9.Common hiding (lookup)
import Data.Dynamic
import qualified Data.Map as Map

-- | A dynamic map, that type checks on insertion and lookup using a type famliy
-- indexed by the key type. It is simply a 'Map' of 'SomeKey' to 'Dynamic'
-- behind a little facade to ensure type safe key->value access to the map.
-- The user must provide type family instances for 'DynMapValue'. This family
-- associates the key type to the value type for a certain map type.
data DynMap (map :: mapKind) =
  DynMap {unDynMap :: Map.Map SomeKey Dynamic} deriving Show

-- | Hide a 'Key'.
data SomeKey where
  SomeKey :: Key k -> SomeKey

instance Show SomeKey where
  show (SomeKey k) = show k

instance Eq SomeKey where
  (SomeKey k1) == (SomeKey k2) = show k1 == show k2

instance Ord SomeKey where
  compare (SomeKey k1) (SomeKey k2) = compare (show k1) (show k2)

-- | The key GADT for 'DynMap's
data Key (k :: t) where
   KeyFromTypeable :: Typeable k     => proxy k          -> Key k
   KeyFromProxy    :: Show (proxy k) => proxy k          -> Key k
   SubKey          ::                   String  -> Key k -> Key k

instance Show (Key k) where
  show (KeyFromTypeable px)   = "KeyFromTypeable '" ++ show (typeRep px) ++ "'"
  show (KeyFromProxy px)      = "KeyFromProxy '"    ++ show px           ++ "'"
  show (SubKey sub parentKey) = "SubKey '"          ++ sub               ++ "' '" ++ show parentKey ++ "'"

instance Eq (Key k) where
  (==) = (==) `on` show

instance Ord (Key k) where
  compare = compare `on` show

-- | Value type for a specific key. Every key must be assigned to a value
-- type. On insertion or lookup from the environment, type checks are made to
-- ensure that the value type matches the key.
type family DynMapValue (map :: mapKind) (key :: keyKind)

-- | A constraint type to reduce the amount of boilerplate.
type DynMapEntry m k v = (Typeable v, v ~ DynMapValue m k)

-- | Generate an empty generic environment.
empty :: DynMap m
empty = DynMap Map.empty

-- | Generate a singleton map.
singleton :: DynMapEntry m k v => Key k -> v -> DynMap m
singleton k v = insert k v empty

-- | Insert a value for the given key.
insert :: DynMapEntry m k v => Key k -> v -> DynMap m -> DynMap m
insert k v =
  DynMap . Map.insert (SomeKey k) (toDyn v) . unDynMap

-- | Insert a value for the given key, possibly combining it with a previous
-- value.
insertWith
  :: DynMapEntry m k v
  => (v -> v -> v) -> Key k -> v -> DynMap m -> DynMap m
insertWith f k v =
  DynMap . Map.insertWith f' (SomeKey k)  (toDyn v) . unDynMap
  where
    f' _newVal oldDyn =
      let (Just old) = fromDynamic oldDyn -- can never go wrong
      in toDyn (f v old)

-- | Alter a value for the given key.
alter :: DynMapEntry m k a
      => (Maybe a -> Maybe a) -> Key k -> DynMap m -> DynMap m
alter f k =
  DynMap . Map.alter f' (SomeKey k) . unDynMap
  where
    f' moldDyn = do
        new <- f $ do oldDyn <- moldDyn
                      fromDynamic oldDyn
        return $ toDyn new

-- | Lookup a value for the key.
lookup :: DynMapEntry m k v => Key k -> DynMap m -> Maybe v
lookup k m = do
  dynValue <- Map.lookup (SomeKey k) (unDynMap m)
  fromDynamic dynValue
