{-| A wrapper around erlang and yaml syntax with a proplist-like behaviour in
    the ConcatableSyntax instances -}
module B9.Content.YamlObject ( YamlObject (..)
                             ) where

import           Control.Applicative
import           Control.Parallel.Strategies
import           Data.Binary                 (Binary (..))
import           Data.Data
import           Data.Function
import           Data.Hashable
import           Data.HashMap.Strict         hiding (singleton)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import           Data.Vector                 (singleton, (++))
import           Data.Yaml
import           GHC.Generics                (Generic)
import           Prelude                     hiding ((++))
import           Text.Printf

import           B9.Content.AST
import           B9.Content.StringTemplate

import           Test.QuickCheck

-- | A wrapper type around yaml values with a Semigroup instance useful for
-- combining yaml documents describing system configuration like e.g. user-data.
data YamlObject =
    YamlObject Data.Yaml.Value
    deriving (Eq,Data,Typeable,Generic)

instance Hashable YamlObject
instance Binary YamlObject
instance NFData YamlObject

instance Binary Data.Yaml.Value where
    put = put . encode
    get = do
        v <- get
        return $ fromJust $ decode v

instance Read YamlObject where
    readsPrec _ = readsYamlObject
      where
        readsYamlObject :: ReadS YamlObject
        readsYamlObject s =
            [(yamlFromString y, r2) | ("YamlObject",r1) <- lex s
                                    , (y,r2) <- reads r1]
          where
            yamlFromString :: String -> YamlObject
            yamlFromString =
                either error id .
                decodeSyntax "HERE-DOC" . E.encodeUtf8 . T.pack

instance Show YamlObject where
  show (YamlObject o) =
    "YamlObject " <> show (T.unpack $ E.decodeUtf8 $ encode o)

instance Semigroup YamlObject where
  (YamlObject v1) <> (YamlObject v2) = YamlObject (combine v1 v2)
    where
      combine :: Data.Yaml.Value
              -> Data.Yaml.Value
              -> Data.Yaml.Value
      combine (Object o1) (Object o2) =
        Object (unionWith combine o1 o2)
      combine (Array a1) (Array a2) =
        Array (a1 ++ a2)
      combine (Array a1) t2 =
        Array (a1 ++ singleton t2)
      combine t1 (Array a2) =
        Array (singleton t1 ++ a2)
      combine (String s1) (String s2) = String (s1 <> s2)
      combine t1 t2 =
        array [t1,t2]

instance ConcatableSyntax YamlObject where
    decodeSyntax src str =
        case decodeEither str of
            Left e ->
                Left
                    (printf "YamlObject parse error in file '%s':\n%s\n" src e)
            Right o -> return (YamlObject o)
    encodeSyntax (YamlObject o) = encode o

instance ASTish YamlObject where
    fromAST ast =
        case ast of
            ASTObj pairs -> do
                ys <- mapM fromASTPair pairs
                return (YamlObject (object ys))
            ASTArr asts -> do
                ys <- mapM fromAST asts
                let ys' =
                        (\(YamlObject o) ->
                              o) <$>
                        ys
                return (YamlObject (array ys'))
            ASTMerge [] ->
                error "ASTMerge MUST NOT be used with an empty list!"
            ASTMerge asts -> do
                ys <- mapM fromAST asts
                return (foldl1 (<>) ys)
            ASTEmbed c ->
                YamlObject . toJSON . T.unpack . E.decodeUtf8 <$> render c
            ASTString str -> return (YamlObject (toJSON str))
            ASTParse src@(Source _ srcPath) -> do
                c <- readTemplateFile src
                case decodeSyntax srcPath c of
                    Right s -> return s
                    Left e ->
                        error
                            (printf
                                 "could not parse yaml source file: '%s'\n%s\n"
                                 srcPath
                                 e)
            AST a -> pure a
      where
        fromASTPair (key,value) = do
            (YamlObject o) <- fromAST value
            let key' = T.pack key
            return $ key' .= o


instance Arbitrary YamlObject
