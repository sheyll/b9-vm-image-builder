-- Instances of 'B9.ConcatableSyntax' for some commonly used
-- syntax types.
module B9.PropLists (YamlObject (..)
                    ,ErlangPropList (..)
                    ) where

import Data.Data
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Yaml
import Data.Function
import Data.HashMap.Strict
import Data.Vector ((++))
import Prelude hiding ((++))
import Data.List (partition,sortBy)
import Data.Monoid
import Text.Printf

import B9.ErlTerms
import B9.ConcatableSyntax

-- | A wrapper type around erlang terms with a Semigroup instance useful for
-- combining sys.config files with OTP-application configurations in a list of
-- the form of a proplist.
data ErlangPropList = ErlangPropList SimpleErlangTerm | EmptyErlangPropList
  deriving (Read,Eq,Show,Data,Typeable)

instance Monoid ErlangPropList where
  mempty = EmptyErlangPropList

  EmptyErlangPropList `mappend` x = x
  x `mappend` EmptyErlangPropList = x

  (ErlangPropList v1) `mappend` (ErlangPropList v2) = ErlangPropList (combine v1 v2)
    where
      combine (ErlList l1) (ErlList l2) =
        ErlList (l1Only `mappend` merged `mappend` l2Only)
        where
          l1Only = l1NonPairs `mappend` l1NotL2
          l2Only = l2NonPairs `mappend` l2NotL1
          (l1Pairs,l1NonPairs) = partition isPair l1
          (l2Pairs,l2NonPairs) = partition isPair l2
          merged = zipWith merge il1 il2
            where
              merge (ErlTuple [_k,pv1]) (ErlTuple [k,pv2]) =
                ErlTuple [k, pv1 `combine` pv2]
          (l1NotL2, il1, il2, l2NotL1) =
            partitionByKey l1Sorted l2Sorted ([],[],[],[])
            where
              partitionByKey [] ys  (exs,cxs,cys,eys) =
                (reverse exs,reverse cxs,reverse cys,reverse eys `mappend` ys)

              partitionByKey xs []  (exs,cxs,cys,eys) =
                (reverse exs `mappend` xs,reverse cxs,reverse cys,reverse eys)

              partitionByKey (x:xs) (y:ys) (exs,cxs,cys,eys)
                | equalKey x y = partitionByKey xs ys (exs,x:cxs,y:cys,eys)
                | x `keyLessThan` y = partitionByKey xs (y:ys) (x:exs,cxs,cys,eys)
                | otherwise = partitionByKey (x:xs) ys (exs,cxs,cys,y:eys)

              l1Sorted = sortByKey l1Pairs
              l2Sorted = sortByKey l2Pairs

          sortByKey = sortBy (compare `on` getKey)
          keyLessThan = (<) `on` getKey
          equalKey = (==) `on` getKey
          getKey (ErlTuple (x:_)) = x
          getKey x = x
          isPair (ErlTuple [_,_]) = True
          isPair _ = False

      combine (ErlList pl1) t2 = ErlList (pl1 `mappend` [t2])
      combine t1 (ErlList pl2) = ErlList ([t1] `mappend` pl2)
      combine t1 t2 = ErlList [t1,t2]

instance ConcatableSyntax ErlangPropList where
  decodeSyntax src str = do
    t <- parseErlTerm src str
    return (ErlangPropList t)

  encodeSyntax (ErlangPropList t) =
    renderErlTerm t

-- | A wrapper type around yaml values with a Semigroup instance useful for
-- combining yaml documents describing system configuration like e.g. user-data.
data YamlObject = YamlObject Data.Yaml.Value | EmptyYamlObject
  deriving (Eq,Show)

instance Monoid YamlObject where
  mempty = EmptyYamlObject

  EmptyYamlObject `mappend` x = x
  x `mappend` EmptyYamlObject = x

  (YamlObject v1) `mappend` (YamlObject v2) = YamlObject (combine v1 v2)
    where
      combine :: Data.Yaml.Value
              -> Data.Yaml.Value
              -> Data.Yaml.Value
      combine (Object o1) (Object o2) =
        Object (unionWith combine o1 o2)
      combine (Array a1) (Array a2) =
        Array (a1 ++ a2)
      combine t1 t2 =
        array [t1,t2]

instance ConcatableSyntax YamlObject where
  decodeSyntax src str = do
    case decodeEither str of
      Left e ->
        Left (printf "YamlObject parse error in file '%s':\n%s\n"
                      src
                      e)
      Right o ->
        return (YamlObject o)

  encodeSyntax (YamlObject o) =
    E.encodeUtf8 (T.pack "#cloud-config\n") `mappend` encode o
