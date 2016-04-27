{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | (Type-) Configurable PropList
--
-- A PropList Contains key-value pairs. See these examples: (TODO Documentation)
--
-- >
-- > testRender = unlines [renderProperties test1
-- >                      ,""
-- >                      ,renderProperties test2
-- >                      ,""
-- >                      ,renderProperties test2]
-- >
-- > test1 =
-- >          (Value "this, is the test service"      :: Property "Documentation")
-- >      ++> (Value [("X","sdfsdf"),("HOME","sdfs")] :: Property "Environment")
-- >      ++> (Value "test service"                   :: Property "Description")
-- >      ++> (Value 123                              :: Property "XYZ")
-- >      ++> (Begin                                  :: EmptyProperties "Unit")
-- >
-- > test2 =
-- >      (   (Begin        :: EmptyProperties "Test")
-- >      <++ (Value 123    :: Property "XYZ")
-- >      <++ (Value "blah" :: Property "Description")
-- >      )
-- >
-- > test3 =
-- >      (   (Begin        :: EmptyProperties "Test2")
-- >      <++ (Value 123    :: Property "XYZ")
-- >      )
-- >
-- > getDescriptions :: (IsProperty "Description", IsMember n "Description" ks)
-- >                 => Properties s r ks
-- >                 -> [Property "Description"]
-- > getDescriptions = getMembers (Proxy :: Proxy "Description")
-- >
-- > type instance RequiredKeys "Test" = '["XYZ"]
-- > type instance RequiredKeys "Test2" = '[]
-- > type instance RequiredKeys "Unit" = '["Description", "XYZ"]
-- >
-- > instance IsProperty "Documentation"
-- > instance IsProperty "Description"
-- > instance IsProperty "Environment" where
-- >  type ValueType   "Environment"        = [(String,String)]
-- >  type Cardinality "Environment" "Unit" = 'ZeroOrMore
-- >  showPropertyValue (Value es) =
-- >    unwords ((\(k,v) -> show ((printf "%s=%s" k v) :: String)) <$> es)
-- > instance IsProperty "XYZ" where
-- >  type ValueType "XYZ" = Int
-- >
-- 
module B9.Content.PropList where

import B9.Common

-- | Convert 'Properties' which contain at least all required entries to a
-- 'String'.
renderProperties :: forall section ps.
                 ShowUninhabited section
              => SufficientProperties (section :: sym) (ps :: [sym])
              -> String
renderProperties props =
  unlines $ printf "[%s]" (showProxy (Proxy :: Proxy section)) : go props
  where go
          :: forall s r p.
             Properties (s :: sym) (r :: [sym]) (p :: [sym]) -> [String]
        go Begin = []
        go (AddProperty s rest) = showProperty s : go rest

-- | Add an entry to 'Properties'. This is the right associative cons operator
-- for the construction of a proplist from an element and a proplist.
(++>)
  :: (IsProperty key
     ,CanAddProperty (Cardinality key section) key keys ~ 'True)
  => Property key
  -> Properties section required keys
  -> Properties section (Remove key required) (key ': keys)
(++>) = AddProperty
infixr 1 ++>

-- | Add an entry to 'Properties'. This is the left associative and analog to
-- the @(flip (++>))@ operator
(<++)
  :: (IsProperty key
     ,CanAddProperty (Cardinality key section) key keys ~ 'True)
  => Properties section required keys
  -> Property key
  -> Properties section (Remove key required) (key ': keys)
(<++) = flip AddProperty
infixl 1 <++

-- | Lookup all entries for a 'Property' in a 'Properties'
getMembers
  :: (IsProperty k
     ,IsMember n k ks)
  => proxy k
  -> Properties s r ks
  -> [Property k]
getMembers keyPx props =
  let memberPositions = getMemberPositions Proxy keyPx props
  in getMembersAtPositions keyPx memberPositions props

-- | Return how many occurences of a 'Property' with a given 'k' are in a type
-- level list 'ks'.
getMemberCount
  :: KnownNat (MemberCount k ks)
  => proxy1 (k :: t)
  -> proxy2 (ks :: [t])
  -> Integer
getMemberCount pe pes = natVal (getMemberCountProxy  pe pes)

-- | Return a 'Nat' 'Proxy' matching the number of occurences of a 'Property'
-- with a given 'k' are in a type level list 'ks'.
getMemberCountProxy
  :: KnownNat (MemberCount k ks)
  => proxy1 (k :: t)
  -> proxy2 (ks :: [t])
  -> Proxy (MemberCount k ks)
getMemberCountProxy _k _ks = Proxy

-- | Return a list of 'Property' entries for a given key 'k' from a
-- 'MemberPositions', usually derived from an instance of 'IsMember'.
getMembersAtPositions
  :: (IsProperty k)
  => proxy k
  -> MemberPositions n k ks
  -> Properties s r ks
  -> [Property k]
getMembersAtPositions px pos props =
  case (pos, props) of
    (_, Begin) ->
      []
    (NotHere, _) ->
      []
    (AndNotHere nextPos, AddProperty _ nextProp) ->
      getMembersAtPositions px nextPos nextProp
    (Here nextPos, AddProperty r nextProp) ->
      r : getMembersAtPositions px nextPos nextProp

-- | A witness for count and the positions at which a type 'a' occurs in a type
-- level list 'as'.
data MemberPositions (count :: Nat) (a :: k) (as :: [k]) where
  NotHere    ::                           MemberPositions 0       e       '[]
  AndNotHere :: MemberPositions n e es -> MemberPositions n       e (f ': es)
  Here       :: MemberPositions n e es -> MemberPositions (n + 1) e (e ': es)

instance Show (MemberPositions (count :: Nat) (a :: k) (as :: [k])) where
  show NotHere        = "NotHere"
  show (AndNotHere m) = "AndNotHere " ++ show m
  show (Here       m) = "Here "       ++ show m

-- | A type-class providing 'MemberPositions'.
class (KnownNat n, n ~ MemberCount a as)
      => IsMember (n :: Nat) (a :: k) (as :: [k]) where
  getMemberPositions
    :: proxy0 n
    -> proxy1 a
    -> proxy2 as
    -> MemberPositions n a as

instance IsMember 0 a '[] where
  getMemberPositions _ _ _ = NotHere

instance
     (IsMember n a as
     ,KnownNat n
     ,n ~ MemberCount a as
     ,n ~ MemberCount a (b ': as))
     => IsMember n a (b ': as) where
  getMemberPositions pn pa _ =
    AndNotHere (getMemberPositions pn pa (Proxy :: Proxy as))

instance
    (IsMember n a as
    ,KnownNat n
    ,n ~ MemberCount a as
    ,KnownNat m
    ,m ~ MemberCount a (a ': as)
    ,m ~ (n + 1))
    => IsMember m a (a ': as) where
  getMemberPositions _pn pa _pas =
    Here (getMemberPositions (Proxy :: Proxy n) pa (Proxy :: Proxy as))

type family MemberCount (a :: k) (as :: [k]) :: Nat where
  MemberCount a '[] = 0
  MemberCount a (a ': rest) = 1 + MemberCount a rest
  MemberCount a (b ': rest) = MemberCount a rest

-- | The type of empty systemD sections.
type EmptyProperties (section :: sym) =
  Properties section (RequiredKeys section) '[]

type SufficientProperties (section :: sym) (ps :: [sym]) =
  Properties section ('[] :: [sym]) ps

-- | Type family to indicate what @section@ identified by a type of any kind may
-- contain what entries.
type family RequiredKeys (section :: sym) :: [k]

data Properties (section :: sym) (required :: [sym])
     (contents :: [sym]) where
        Begin :: Properties section required '[]
        AddProperty ::
            (IsProperty key,
             CanAddProperty (Cardinality key section) key keys ~ 'True) =>
              Property key ->
              Properties section required keys ->
              Properties section (Remove key required) (key ': keys)

type family CanAddProperty (r :: MaxCardinality) (p :: k)
     (ps :: [k]) where
        CanAddProperty 'Never      p ps = 'False
        CanAddProperty 'AtMostOnce p ps = (NotElem p ps)
        CanAddProperty 'ZeroOrMore p ps = 'True

type family Elem (p :: k) (ps :: [k]) :: Bool where
        Elem p '[]       = 'False
        Elem p (p ': ps) = 'True
        Elem p (x ': ps) = Elem p ps

type family NotElem (p :: k) (ps :: [k]) :: Bool where
        NotElem p '[]       = 'True
        NotElem p (p ': ps) = 'False
        NotElem p (x ': ps) = NotElem p ps

type family Remove (p :: k) (ps :: [k]) :: [k] where
        Remove p '[]       =              '[]
        Remove p (p ': ps) =      Remove p ps
        Remove p (x ': ps) = x ': Remove p ps

class IsProperty (k :: sym)  where
  -- | Max occurences for the property in a unit
  type Cardinality k (u :: h) :: MaxCardinality
  type Cardinality k (u :: h) = 'ZeroOrMore
  -- | Values accepted for the property
  type ValueType k :: *
  type ValueType k = String
  -- | Render the value
  showPropertyValue :: Property k -> String
  default showPropertyValue :: (Show (ValueType k)) => (Property k) -> String
  -- | Render the value using the 'Show' for the value type by default.
  showPropertyValue (Value v) = show v
  -- | Render the key of the property
  showPropertyKey :: proxy k -> String
  default showPropertyKey :: (ShowUninhabited k) => proxy k -> String
  -- | Render the key of the property using 'showProxy' by default.
  showPropertyKey = showProxy
  -- | Render the complete key-value assignment
  showProperty :: Property k -> String
  default showProperty :: (ShowUninhabited k) => Property k -> String
  -- | Render the complete key-value assignment as @key=value@ by default
  showProperty zv = showPropertyKey zv ++ "=" ++ showPropertyValue zv

-- | A class similar to 'Show' especially for uninhabited types.
class ShowUninhabited (k :: t) where
  showProxy :: p k -> String

instance KnownSymbol k => ShowUninhabited k where
  showProxy = symbolVal

data Property (k :: sym) where
  Value :: IsProperty k => ValueType k -> Property k

data MaxCardinality
  = Never
  | AtMostOnce
  | ZeroOrMore
