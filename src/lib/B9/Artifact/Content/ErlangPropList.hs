-- | Allow reading, merging and writing Erlang terms.
module B9.Artifact.Content.ErlangPropList
  ( ErlangPropList (..),
    textToErlangAst,
    stringToErlangAst,
  )
where

import B9.Artifact.Content
import B9.Artifact.Content.AST
import B9.Artifact.Content.ErlTerms
import B9.Artifact.Content.StringTemplate
import B9.Text
import Control.Parallel.Strategies
import Data.Data
import Data.Function
import Data.Hashable
import Data.List (partition, sortBy)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck
import Text.Printf

-- | A wrapper type around erlang terms with a Semigroup instance useful for
-- combining sys.config files with OTP-application configurations in a list of
-- the form of a proplist.
newtype ErlangPropList
  = ErlangPropList SimpleErlangTerm
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
              partitionByKey (x : xs) (y : ys) (exs, cxs, cys, eys)
                | equalKey x y = partitionByKey xs ys (exs, x : cxs, y : cys, eys)
                | x `keyLessThan` y = partitionByKey xs (y : ys) (x : exs, cxs, cys, eys)
                | otherwise = partitionByKey (x : xs) ys (exs, cxs, cys, y : eys)
              l1Sorted = sortByKey l1Pairs
              l2Sorted = sortByKey l2Pairs
          sortByKey = sortBy (compare `on` getKey)
          keyLessThan = (<) `on` getKey
          equalKey = (==) `on` getKey
          getKey (ErlTuple (x : _)) = x
          getKey x = x
          isPair (ErlTuple [_, _]) = True
          isPair _ = False
      combine (ErlList pl1) t2 = ErlList (pl1 <> [t2])
      combine t1 (ErlList pl2) = ErlList ([t1] <> pl2)
      combine t1 t2 = ErlList [t1, t2]

instance Textual ErlangPropList where
  parseFromText txt = do
    str <- parseFromText txt
    t <- parseErlTerm "" str
    return (ErlangPropList t)
  renderToText (ErlangPropList t) = renderToText (renderErlTerm t)

instance FromAST ErlangPropList where
  fromAST (AST a) = pure a
  fromAST (ASTObj pairs) = ErlangPropList . ErlList <$> mapM makePair pairs
    where
      makePair (k, ast) = do
        (ErlangPropList second) <- fromAST ast
        return $ ErlTuple [ErlAtom k, second]
  fromAST (ASTArr xs) =
    ErlangPropList . ErlList
      <$> mapM
        ( \x -> do
            (ErlangPropList x') <- fromAST x
            return x'
        )
        xs
  fromAST (ASTString s) = pure $ ErlangPropList $ ErlString s
  fromAST (ASTInt i) = pure $ ErlangPropList $ ErlString (show i)
  fromAST (ASTEmbed c) = ErlangPropList . ErlString . T.unpack <$> toContentGenerator c
  fromAST (ASTMerge []) = error "ASTMerge MUST NOT be used with an empty list!"
  fromAST (ASTMerge asts) = foldl1 (<>) <$> mapM fromAST asts
  fromAST (ASTParse src@(Source _ srcPath)) = do
    c <- readTemplateFile src
    case parseFromTextWithErrorMessage srcPath c of
      Right s -> return s
      Left e -> error (printf "could not parse erlang source file: '%s'\n%s\n" srcPath e)

-- * Misc. utilities

-- | Parse a text containing an @Erlang@ expression ending with a @.@ and Return
-- an 'AST'.
--
-- @since 0.5.67
textToErlangAst :: Text -> AST c ErlangPropList
textToErlangAst txt =
  either
    (error . ((unsafeParseFromText txt ++ "\n:  ") ++))
    AST
    (parseFromTextWithErrorMessage "textToErlangAst" txt)

-- | Parse a string containing an @Erlang@ expression ending with a @.@ and Return
-- an 'AST'.
--
-- @since 0.5.67
stringToErlangAst :: String -> AST c ErlangPropList
stringToErlangAst = textToErlangAst . unsafeRenderToText
