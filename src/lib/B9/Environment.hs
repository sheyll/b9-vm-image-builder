{-| An 'Environment' contains textual key value pairs, relavant for string template
substitution.

The variables are passed to the B9 build either via command line, OS environment
variables or configuration file.

@since 0.5.62
 -}
module B9.Environment
  ( Environment()
  , fromStringPairs
  , addBinding
  , addStringBinding
  , addLocalStringBinding
  , addPositionalArguments
  , addLocalPositionalArguments
  , EnvironmentReader
  , hasKey
  , runEnvironmentReader
  , askEnvironment
  , localEnvironment
  , lookupOrThrow
  , lookupEither
  , KeyNotFound(..)
  , DuplicateKey(..)
  )
where

import           B9.B9Error
import           B9.Text
import           Control.Arrow                  ( (***) )
import           Control.Exception              ( Exception )
import           Control.Eff                   as Eff
import           Control.Eff.Reader.Lazy       as Eff
import           Control.Parallel.Strategies
import           Data.Data
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( maybe
                                                , isJust
                                                )
import           GHC.Generics                   ( Generic )

-- | A map of textual keys to textual values.
--
-- @since 0.5.62
data Environment = MkEnvironment
  { nextPosition    :: Int
  , fromEnvironment :: HashMap Text Text
  } deriving (Show, Typeable, Data, Eq, Generic)

instance NFData Environment

instance Semigroup Environment where
  e1 <> e2 = MkEnvironment
    { nextPosition    = case (nextPosition e1, nextPosition e2) of
                          (0 , 0 ) -> 0
                          (0 , p2) -> p2
                          (p1, 0 ) -> p1
                          _        -> error
                            (  "Overlapping positional arguments (<>): ("
                            ++ show e1
                            ++ ") <> ("
                            ++ show e2
                            ++ ")"
                            )
    , fromEnvironment = let i  = HashMap.intersection h1 h2
                            h1 = fromEnvironment e1
                            h2 = fromEnvironment e2
                        in  if HashMap.null i || all
                               (\k -> HashMap.lookup k h1 == HashMap.lookup k h2
                               )
                               (HashMap.keys i)
                            then
                              h1 <> h2
                            else
                              error
                                (  "Overlapping entries (<>): ("
                                ++ show e1
                                ++ ") <> ("
                                ++ show e2
                                ++ "): ("
                                ++ show i
                                ++ ")"
                                )
    }

instance Monoid Environment where
  mempty = MkEnvironment 0 HashMap.empty

-- | If environment variables @arg_1 .. arg_n@ are bound
-- and a list of @k@ additional values are passed to this function,
-- store them with keys @arg_(n+1) .. arg_(n+k)@.
--
-- Note that the Environment contains an index of the next position.
--
-- @since 0.5.62
addPositionalArguments :: [Text] -> Environment -> Environment
addPositionalArguments = flip
  (foldr
    (\arg (MkEnvironment i e) -> MkEnvironment
      (i + 1)
      (HashMap.insert (unsafeRenderToText ("arg_" ++ show i)) arg e)
    )
  )


-- | Convenient wrapper around 'addPositionalArguments' and 'localEnvironment'.
--
-- @since 0.5.65
addLocalPositionalArguments
  :: Member EnvironmentReader e => [String] -> Eff e a -> Eff e a
addLocalPositionalArguments extraPositional = localEnvironment appendVars
 where
  appendVars = addPositionalArguments (unsafeRenderToText <$> extraPositional)

-- | Create an 'Environment' from a list of pairs ('String's).
-- Duplicated entries are ignored.
--
-- @since 0.5.62
fromStringPairs :: [(String, String)] -> Environment
fromStringPairs = MkEnvironment 0 . HashMap.fromList . fmap
  (unsafeRenderToText *** unsafeRenderToText)

-- | Insert a key value binding to the 'Environment'.
--
-- Throw 'DuplicateKey' if the key already exists, but
-- the value is not equal to the given value.
--
-- @since 0.5.67
addBinding :: Member ExcB9 e => (Text, Text) -> Environment -> Eff e Environment
addBinding (k, vNew) env =
  let h = fromEnvironment env
  in  case HashMap.lookup k h of
        Just vOld | vOld /= vNew ->
          throwSomeException (MkDuplicateKey k vOld vNew)
        _ -> pure (MkEnvironment (nextPosition env) (HashMap.insert k vNew h))

-- | Insert 'String's into the 'Environment', see 'addBinding'.
--
-- @since 0.5.62
addStringBinding
  :: Member ExcB9 e => (String, String) -> Environment -> Eff e Environment
addStringBinding = addBinding . (unsafeRenderToText *** unsafeRenderToText)

-- | Insert a value into an 'Environment' like 'addStringBinding',
-- but add it to the environment of the given effect, as in 'localEnvironment'.
--
-- @since 0.5.65
addLocalStringBinding
  :: (Member EnvironmentReader e, Member ExcB9 e)
  => (String, String)
  -> Eff e a
  -> Eff e a
addLocalStringBinding binding action = do
  e  <- askEnvironment
  e' <- addStringBinding binding e
  localEnvironment (const e') action

-- | A monad transformer providing a 'MonadReader' instance for 'Environment'
--
-- @since 0.5.62
type EnvironmentReader = Reader Environment

-- | Run a 'ReaderT' of 'Environment'.
--
-- @since 0.5.62
runEnvironmentReader :: Environment -> Eff (EnvironmentReader ': e) a -> Eff e a
runEnvironmentReader = runReader

-- | Get the current 'Environment'
--
-- @since 0.5.62
askEnvironment :: Member EnvironmentReader e => Eff e Environment
askEnvironment = ask

-- | Run a computation with a modified 'Environment'
--
-- @since 0.5.62
localEnvironment
  :: Member EnvironmentReader e
  => (Environment -> Environment)
  -> Eff e a
  -> Eff e a
localEnvironment = local

-- | Lookup a key for a value.
--
-- 'throwM' a 'KeyNotFound' 'Exception' if no value with the given key exists
-- in the 'Environment'.
--
-- @Since 0.5.62
lookupOrThrow :: ('[ExcB9, EnvironmentReader] <:: e) => Text -> Eff e Text
lookupOrThrow key = do
  env <- askEnvironment
  maybe (throwSomeException (MkKeyNotFound key env))
        return
        (HashMap.lookup key (fromEnvironment env))

-- | Lookup a key for a value.
--
-- Return 'Either' 'Left' 'KeyNotFound', if no value with the given key exists
-- in the 'Environment', or 'Right' the value.
--
-- @Since 0.5.62
lookupEither
  :: Member EnvironmentReader e => Text -> Eff e (Either KeyNotFound Text)
lookupEither key = do
  env <- askEnvironment
  (return . maybe (Left (MkKeyNotFound key env)) Right)
    (HashMap.lookup key (fromEnvironment env))

-- | An 'Exception' thrown by 'addBinding' indicating that a key already exists.
--
-- @Since 0.5.62
data DuplicateKey = MkDuplicateKey
  { duplicateKey         :: Text
  , duplicateKeyOldValue :: Text
  , duplicateKeyNewValue :: Text
  } deriving (Typeable, Show)

instance Exception DuplicateKey

-- | An 'Exception' thrown by 'lookupOrThrow' indicating that a key does not exist.
--
-- @Since 0.5.62
data KeyNotFound =
  MkKeyNotFound Text
                Environment
  deriving (Typeable)

instance Exception KeyNotFound

instance Show KeyNotFound where
  showsPrec _ (MkKeyNotFound key env) =
    let keys =
            unlines (unsafeParseFromText <$> HashMap.keys (fromEnvironment env))
    in  showString "Invalid template parameter: \""
          . showString (unsafeParseFromText key)
          . showString "\".\nValid variables:\n"
          . showString keys

-- | A predicate that is satisfied when a key exists in the environment.
--
-- @since 0.5.64
hasKey :: Member EnvironmentReader e => Text -> Eff e Bool
hasKey k = isJust . HashMap.lookup k . fromEnvironment <$> askEnvironment
