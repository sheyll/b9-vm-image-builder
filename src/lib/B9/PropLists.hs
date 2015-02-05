-- Instances of 'B9.ConcatableSyntax' for some commonly used
-- syntax types.
module B9.PropLists (YamlUserData (..)
                    ,OTPSysConfig (..)) where

import Data.Yaml
import Data.Function
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict
import Control.Applicative
import Text.Show.Pretty
import Data.Vector ((++))
import Prelude hiding ((++))
import Data.List (partition,sortBy)
import Data.Semigroup

import B9.ErlTerms
import B9.ConcatableSyntax

-- | A wrapper type around erlang terms with a Semigroup instance useful for
-- combining sys.config files with OTP-application configurations in a list of
-- the form of a proplist.
newtype OTPSysConfig = OTPSysConfig SimpleErlangTerm
  deriving (Eq,Show)

instance Semigroup OTPSysConfig where
  (OTPSysConfig v1) <> (OTPSysConfig v2) = OTPSysConfig (combine v1 v2)
    where
      combine (ErlList l1) (ErlList l2) =
        ErlList (l1Only <> merged <> l2Only)
        where
          l1Only = l1NonPairs <> l1NotL2
          l2Only = l2NonPairs <> l2NotL1
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
                (reverse exs,reverse cxs,reverse cys,reverse eys <> ys)

              partitionByKey xs []  (exs,cxs,cys,eys) =
                (reverse exs <> xs,reverse cxs,reverse cys,reverse eys)

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

      combine (ErlList pl1) t2 = ErlList (pl1 <> [t2])
      combine t1 (ErlList pl2) = ErlList ([t1] <> pl2)
      combine t1 t2 = ErlList [t1,t2]

instance ConcatableSyntax OTPSysConfig where
  decodeSyntax str = do
    t <- parseErlTerm "" str
    return (OTPSysConfig t)

  encodeSyntax (OTPSysConfig t) =
    renderErlTerm t

-- | A wrapper type around yaml values with a Semigroup instance useful for
-- combining yaml documents describing system configuration like e.g. user-data.
newtype YamlUserData = YamlUserData Data.Yaml.Value
  deriving (Eq,Show)

instance Semigroup YamlUserData where
  (YamlUserData v1) <> (YamlUserData v2) = YamlUserData (combine v1 v2)
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

instance ConcatableSyntax YamlUserData where
  decodeSyntax str = do
    o <- decodeEither str
    return (YamlUserData o)

  encodeSyntax (YamlUserData o) = encode o
