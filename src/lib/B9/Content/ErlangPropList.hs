{-| Allow reading, merging and writing Erlang terms. -}
module B9.Content.ErlangPropList
  ( ErlangPropList(..)
  ) where

import Control.Parallel.Strategies
import Data.Binary as Binary
import Data.Binary.Get as Binary
import Data.Data
import Data.Function
import Data.Hashable
import Data.List (partition, sortBy)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import GHC.Generics (Generic)
import Text.Printf

import B9.Content.AST
import B9.Content.ErlTerms
import B9.Content.Generator
import B9.Content.StringTemplate

import Data.Binary.Put (putLazyByteString)
import Test.QuickCheck

-- | A wrapper type around erlang terms with a Semigroup instance useful for
-- combining sys.config files with OTP-application configurations in a list of
-- the form of a proplist.
newtype ErlangPropList =
  ErlangPropList SimpleErlangTerm
  deriving (Read, Eq, Show, Data, Typeable, Generic)

instance Hashable ErlangPropList

instance NFData ErlangPropList

instance Arbitrary ErlangPropList where
  arbitrary = ErlangPropList <$> arbitrary

instance Semigroup ErlangPropList where
  (ErlangPropList v1) <> (ErlangPropList v2) = ErlangPropList (combine v1 v2)
    where
      combine (ErlList l1) (ErlList l2) = ErlList (l1Only <> merged <> l2Only)
        where
          l1Only = l1NonPairs <> l1NotL2
          l2Only = l2NonPairs <> l2NotL1
          (l1Pairs, l1NonPairs) = partition isPair l1
          (l2Pairs, l2NonPairs) = partition isPair l2
          merged = zipWith merge il1 il2
            where
              merge (ErlTuple [_k, pv1]) (ErlTuple [k, pv2]) = ErlTuple [k, pv1 `combine` pv2]
              merge _ _ = error "unreachable"
          (l1NotL2, il1, il2, l2NotL1) = partitionByKey l1Sorted l2Sorted ([], [], [], [])
            where
              partitionByKey [] ys (exs, cxs, cys, eys) = (reverse exs, reverse cxs, reverse cys, reverse eys <> ys)
              partitionByKey xs [] (exs, cxs, cys, eys) = (reverse exs <> xs, reverse cxs, reverse cys, reverse eys)
              partitionByKey (x:xs) (y:ys) (exs, cxs, cys, eys)
                | equalKey x y = partitionByKey xs ys (exs, x : cxs, y : cys, eys)
                | x `keyLessThan` y = partitionByKey xs (y : ys) (x : exs, cxs, cys, eys)
                | otherwise = partitionByKey (x : xs) ys (exs, cxs, cys, y : eys)
              l1Sorted = sortByKey l1Pairs
              l2Sorted = sortByKey l2Pairs
          sortByKey = sortBy (compare `on` getKey)
          keyLessThan = (<) `on` getKey
          equalKey = (==) `on` getKey
          getKey (ErlTuple (x:_)) = x
          getKey x = x
          isPair (ErlTuple [_, _]) = True
          isPair _ = False
      combine (ErlList pl1) t2 = ErlList (pl1 <> [t2])
      combine t1 (ErlList pl2) = ErlList ([t1] <> pl2)
      combine t1 t2 = ErlList [t1, t2]

instance Binary ErlangPropList where
  get = do
    str <- Binary.getRemainingLazyByteString
    case parseErlTerm "" str of
      Right t -> return (ErlangPropList t)
      Left e -> fail e
  put (ErlangPropList t) = putLazyByteString (renderErlTerm t)

instance FromAST ErlangPropList where
  fromAST (AST a) = pure a
  fromAST (ASTObj pairs) = ErlangPropList . ErlList <$> mapM makePair pairs
    where
      makePair (k, ast) = do
        (ErlangPropList second) <- fromAST ast
        return $ ErlTuple [ErlAtom k, second]
  fromAST (ASTArr xs) =
    ErlangPropList . ErlList <$>
    mapM
      (\x -> do
         (ErlangPropList x') <- fromAST x
         return x')
      xs
  fromAST (ASTString s) = pure $ ErlangPropList $ ErlString s
  fromAST (ASTInt i) = pure $ ErlangPropList $ ErlString (show i)
  fromAST (ASTEmbed c) = ErlangPropList . ErlString . T.unpack . E.decodeUtf8 <$> toContentGenerator c
  fromAST (ASTMerge []) = error "ASTMerge MUST NOT be used with an empty list!"
  fromAST (ASTMerge asts) = foldl1 (<>) <$> mapM fromAST asts
  fromAST (ASTParse src@(Source _ srcPath)) = do
    c <- readTemplateFile src
    case decodeOrFail' srcPath c of
      Right s -> return s
      Left e -> error (printf "could not parse erlang source file: '%s'\n%s\n" srcPath e)