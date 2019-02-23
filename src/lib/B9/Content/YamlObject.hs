-- | A wrapper around Yaml with 'Semigroup' and 'Monoid' instances for merging, reading and
-- writing yaml files within B9.
module B9.Content.YamlObject
  ( YamlObject(..)
  ) where

import           Control.Applicative
import           Control.Parallel.Strategies
import           Data.Binary                 (Binary (..))
import qualified Data.Binary.Get             as Binary
import qualified Data.ByteString.Lazy.Char8  as Lazy
import           Data.Data
import           Data.Function
import           Data.Hashable
import           Data.HashMap.Strict         hiding (singleton)
import           Data.Semigroup
import qualified Data.Text                   as StrictT
import qualified Data.Text.Encoding          as StrictE
import qualified Data.Text.Lazy              as LazyT
import qualified Data.Text.Lazy.Encoding     as LazyE
import           Data.Vector                 as Vector (singleton, (++))
import           Data.Yaml                   as Yaml
import           GHC.Generics                (Generic)
import           Prelude                     hiding ((++))
import           Text.Printf

import           B9.Content.AST
import           B9.Content.Generator
import           B9.Content.StringTemplate

import           Test.QuickCheck

-- | A wrapper type around yaml values with a Semigroup instance useful for
-- combining yaml documents describing system configuration like e.g. user-data.
newtype YamlObject = YamlObject
  { _fromYamlObject :: Yaml.Value
  } deriving (Hashable, NFData, Eq, Data, Typeable, Generic)

instance Binary YamlObject where
  put = put . encode . _fromYamlObject
  get = YamlObject . either (error . show) id . Yaml.decodeThrow . Lazy.toStrict <$> Binary.getRemainingLazyByteString

instance Read YamlObject where
  readsPrec _ = readsYamlObject
    where
      readsYamlObject :: ReadS YamlObject
      readsYamlObject s = [(yamlFromString y, r2) | ("YamlObject", r1) <- lex s, (y, r2) <- reads r1]
        where
          yamlFromString :: String -> YamlObject
          yamlFromString = either error id . decodeOrFail' "HERE-DOC" . LazyE.encodeUtf8 . LazyT.pack

instance Show YamlObject where
  show (YamlObject o) = "YamlObject " <> show (StrictT.unpack $ StrictE.decodeUtf8 $ encode o)

instance Semigroup YamlObject where
  (YamlObject v1) <> (YamlObject v2) = YamlObject (combine v1 v2)
    where
      combine :: Yaml.Value -> Yaml.Value -> Yaml.Value
      combine (Object o1) (Object o2) = Object (unionWith combine o1 o2)
      combine (Array a1) (Array a2)   = Array (a1 ++ a2)
      combine (Array a1) t2           = Array (a1 ++ Vector.singleton t2)
      combine t1 (Array a2)           = Array (Vector.singleton t1 ++ a2)
      combine (String s1) (String s2) = String (s1 <> s2)
      combine t1 t2                   = array [t1, t2]

instance FromAST YamlObject where
  fromAST ast =
    case ast of
      ASTObj pairs -> do
        ys <- mapM fromASTPair pairs
        return (YamlObject (object ys))
      ASTArr asts -> do
        ys <- mapM fromAST asts
        let ys' = (\(YamlObject o) -> o) <$> ys
        return (YamlObject (array ys'))
      ASTMerge [] -> error "ASTMerge MUST NOT be used with an empty list!"
      ASTMerge asts -> do
        ys <- mapM fromAST asts
        return (foldl1 (<>) ys)
      ASTEmbed c -> YamlObject . toJSON . StrictT.unpack . StrictE.decodeUtf8 . Lazy.toStrict <$> toContentGenerator c
      ASTString str -> return (YamlObject (toJSON str))
      ASTInt int -> return (YamlObject (toJSON int))
      ASTParse src@(Source _ srcPath) -> do
        c <- readTemplateFile src
        case decodeOrFail' srcPath c of
          Right s -> return s
          Left e -> error (printf "could not parse yaml source file: '%s'\n%s\n" srcPath e)
      AST a -> pure a
    where
      fromASTPair (key, value) = do
        (YamlObject o) <- fromAST value
        let key' = StrictT.pack key
        return $ key' .= o

instance Arbitrary YamlObject where
  arbitrary = pure (YamlObject Null)
