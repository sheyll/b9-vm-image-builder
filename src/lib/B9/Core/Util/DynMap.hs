-- | A (pseudo-)typesafe dynamic map.
module B9.Core.Util.DynMap
       (DynMap, SomeKey(..), Entry(..), empty, singleton, insert, delete,
        insertWith, alter, lookup, adjust)
       where

import Prelude hiding (lookup)
import B9.Core.Prelude hiding (lookup, delete)
import Data.Dynamic
import qualified Data.Map as Map

-- | A dynamic map, that type checks on insertion and lookup using a type famliy
-- indexed by the key type. It is simply a 'Map' of 'SomeKey' to 'Dynamic'
-- behind a little facade to ensure type safe key->value access to the map.
-- The user must provide type family instances for 'Value'. This family
-- associates the key type to the value type for a certain map type.
data DynMap (map :: mapKind) =
  DynMap {unDynMap :: Map.Map SomeKey Dynamic}
  deriving Show

-- | Hide a 'Key'.
newtype SomeKey where
        SomeKey :: String -> SomeKey
    deriving (Show, Eq, Ord)

class Entry (m :: mapKind) (k :: keyKind)  where
  data Key m k
  type Value m k
  toSomeKey :: Key m k -> SomeKey

-- | Generate an empty generic environment.
empty :: DynMap m
empty = DynMap Map.empty

-- | Generate a singleton map.
singleton
  :: (Typeable v, v ~ (Value m k), Entry m k) => Key m k -> v -> DynMap m
singleton k v = insert k v empty

-- | Insert a value for the given key.
insert
  :: (Typeable (Value m k), Entry m k)
  => Key m k
  -> Value m k
  -> DynMap m
  -> DynMap m
insert k v = DynMap . Map.insert (toSomeKey k) (toDyn v) . unDynMap

-- | Remove an entry for the given key. If the key is not a member of the map,
-- return the old map.
delete :: Entry m k => Key m k -> DynMap m -> DynMap m
delete k = DynMap . Map.delete (toSomeKey k) . unDynMap

-- | Insert a value for the given key, possibly combining it with a previous
-- value.
insertWith
  :: (Typeable (Value m k), Entry m k)
  => (Value m k -> Value m k -> Value m k)
  -> Key m k
  -> Value m k
  -> DynMap m
  -> DynMap m
insertWith f k v =
  DynMap . Map.insertWith f' (toSomeKey k) (toDyn v) . unDynMap
 where
  f' _newVal oldDyn = let (Just old) = fromDynamic oldDyn in toDyn (f v old) -- can never go wrong

-- | Alter a value for the given key.
alter
  :: (Typeable (Value m k), Entry m k)
  => (Maybe (Value m k) -> Maybe (Value m k))
  -> Key m k
  -> DynMap m
  -> DynMap m
alter f k = DynMap . Map.alter f' (toSomeKey k) . unDynMap
 where
  f' moldDyn = do
    new <- f $ do
      oldDyn <- moldDyn
      fromDynamic oldDyn
    return $ toDyn new

-- | Lookup a value for the key.
lookup
  :: (Typeable (Value m k), Entry m k)
  => Key m k
  -> DynMap m
  -> Maybe (Value m k)
lookup k m = do
  dynValue <- Map.lookup (toSomeKey k) (unDynMap m)
  fromDynamic dynValue

-- | Update a value at a specific key with the result of the provided function.
-- When the key is not a member of the map, the original map is returned.
adjust
  :: (Typeable (Value m k), Entry m k)
  => (Value m k -> Value m k)
  -> Key m k
  -> DynMap m
  -> DynMap m
adjust f = alter (fmap f)



