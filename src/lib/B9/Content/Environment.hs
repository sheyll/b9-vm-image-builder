{-| An 'Environment' contains textual key value pairs, relavant for string template
substitution.

The variables are passed to the B9 build either via command line, OS environment
variables or configuration file.

@since 0.5.62
 -}
module B9.Content.Environment
  ( Environment()
  , fromStringPairs
  , addStringBinding
  , EnvironmentReaderT
  , MonadEnvironment
  , runEnvironmentReaderT
  , askEnvironment
  , localEnvironment
  , lookupOrThrow
  , lookupEither
  , KeyNotFound(..)
  , DuplicateKey(..)
  ) where

import           Control.Arrow               ((***))
import           Control.Exception           (Exception)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.Reader
import           Control.Parallel.Strategies
import           Data.Data
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

-- | A map of textual keys to textual values.
--
-- @since 0.5.62
newtype Environment = MkEnvironment
  { fromEnvironment :: HashMap Text Text
  } deriving (Show, Typeable, Data, Eq, NFData)

-- | Create an 'Environment' from a list of pairs ('String's)
--
-- @since 0.5.62
fromStringPairs :: [(String, String)] -> Environment
fromStringPairs = MkEnvironment . HashMap.fromList . fmap (Text.pack *** Text.pack)

-- | Insert a value into an 'Environment'.
--
-- @since 0.5.62
addStringBinding :: MonadThrow m => (String, String) -> Environment -> m Environment
addStringBinding (k, vNew) env =
  case HashMap.lookup (Text.pack k) (fromEnvironment env) of
    Just vOld -> throwM (MkDuplicateKey (Text.pack k) vOld (Text.pack vNew))
    Nothing -> pure (MkEnvironment (HashMap.insert (Text.pack k) (Text.pack vNew) (fromEnvironment env)))

-- | A monad transformer providing a 'MonadReader' instance for 'Environment'
--
-- @since 0.5.62
type EnvironmentReaderT m a = ReaderT Environment m a

-- | A constraint on a monad @m@ ensuring (read-only) access to an 'Environment'
--
-- @since 0.5.62
type MonadEnvironment m = MonadReader Environment m

-- | Run a 'ReaderT' of 'Environment'.
--
-- @since 0.5.62
runEnvironmentReaderT :: Environment -> EnvironmentReaderT m a -> m a
runEnvironmentReaderT = flip runReaderT

-- | Get the current 'Environment'
--
-- @since 0.5.62
askEnvironment :: MonadEnvironment m => m Environment
askEnvironment = ask

-- | Run a computation with a modified 'Environment'
--
-- @since 0.5.62
localEnvironment :: MonadEnvironment m => (Environment -> Environment) -> m a -> m a
localEnvironment = local

-- | Lookup a key for a value.
--
-- 'throwM' a 'KeyNotFound' 'Exception' if no value with the given key exists
-- in the 'Environment'.
--
-- @Since 0.5.62
lookupOrThrow :: (MonadThrow m, MonadEnvironment m) => Text -> m Text
lookupOrThrow key = do
  env <- askEnvironment
  maybe (throwM (MkKeyNotFound key env)) return (HashMap.lookup key (fromEnvironment env))

-- | Lookup a key for a value.
--
-- Return 'Either' 'Left' 'KeyNotFound', if no value with the given key exists
-- in the 'Environment', or 'Right' the value.
--
-- @Since 0.5.62
lookupEither :: MonadEnvironment m => Text -> m (Either KeyNotFound Text)
lookupEither key = do
  env <- askEnvironment
  maybe (return (Left (MkKeyNotFound key env))) (return . Right) (HashMap.lookup key (fromEnvironment env))

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
    let keys = unlines (Text.unpack <$> HashMap.keys (fromEnvironment env))
     in showString "Invalid template parameter: \"" .
        showString (Text.unpack key) . showString "\".\nValid variables:\n" . showString keys
