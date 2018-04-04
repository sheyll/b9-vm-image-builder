module B9.Invokation ( B9Invokation()
                     , invokeB9
                     , invokeB9_
                     , overrideWorkingDirectory
                     , doAfterConfiguration
                     , overrideB9ConfigPath
                     , modifyInvokationConfig
                     , modifyPermanentConfig) where

import B9.B9Config
import Data.ConfigFile.B9Extras
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Control.Exception (bracket)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Data.Semigroup as Sem
import Data.Maybe (fromMaybe)
import Text.Printf ( printf )

-- | A monad encapsulating all configuration adaptions happening before any actual
-- build actions are performed. Use 'invokeB9' to execute these things.
-- Inside the 'B9Invokation' monad you can influence and inspect the b9
-- configuration, the permanent, as well as the transient.
newtype B9Invokation a = B9Inv {runB9Invokation :: StateT InternalState IO a}
  deriving (MonadState InternalState, Monad, Applicative, Functor, MonadIO)

-- | Internal state of the 'B9Invokation'
data InternalState = IS { _initialConfigOverride   :: B9ConfigOverride
                        , _permanentB9ConfigUpdate :: Maybe (ConfigParser -> Either CPError ConfigParser)
                        , _changeWorkingDirectory  :: Maybe FilePath
                        , _buildAction             :: ReaderT B9Config IO Bool
                        }

makeLenses ''InternalState

initialState :: InternalState
initialState =
    IS (B9ConfigOverride Nothing mempty) Nothing Nothing (return True)

-- | Run a 'B9Invokation'.
-- If the user has no B9 config file yet, a default config file will be created.
invokeB9 :: B9Invokation a -> IO (a, Bool)
invokeB9 act = do
    (a, st) <- runStateT (runB9Invokation act) initialState
    let cfgPath = st ^. initialConfigOverride . customB9ConfigPath
    cp0 <- openOrCreateB9Config cfgPath
    let cpExtErr = fmap ($ cp0) (st ^. permanentB9ConfigUpdate)
    cpExt <- maybe
        (return Nothing)
        ( either
            ( fail
            . printf "Internal configuration error! Please report this: %s\n"
            . show
            )
            (return . Just)
        )
        cpExtErr
    let cp = fromMaybe cp0 cpExt
    mapM_ (writeB9ConfigParser cfgPath) cpExt
    case parseB9Config cp of
        Left e -> fail (printf "Configuration error: %s\n" (show e))
        Right permanentConfig -> do
            let runtimeCfg =
                    permanentConfig
                        Sem.<> st
                        ^.     initialConfigOverride
                        .      customB9Config
                completeBuildAction = bracket
                    getCurrentDirectory
                    setCurrentDirectory
                    ( const
                        ( do
                            mapM_ setCurrentDirectory
                                  (st ^. changeWorkingDirectory)
                            runReaderT (st ^. buildAction) runtimeCfg
                        )
                    )
            res <- completeBuildAction
            return (a, res)

-- | Run a 'B9Invokation' and ignore the results of the invokation monad and
-- instead return the results of the '_buildAction'.
-- See: 'invokeB9'
invokeB9_ :: B9Invokation a -> IO Bool
invokeB9_ act = snd <$> invokeB9 act

-- | Add an action to be executed once the configuration has been done.
doAfterConfiguration :: ReaderT B9Config IO Bool -> B9Invokation ()
doAfterConfiguration action = buildAction %= (>> action)

-- | Override the B9 configuration path
overrideB9ConfigPath :: SystemPath -> B9Invokation ()
overrideB9ConfigPath p = initialConfigOverride . customB9ConfigPath .= Just p

-- | Override the current working directory during execution of the actions
-- added with 'doAfterConfiguration'.
overrideWorkingDirectory :: FilePath -> B9Invokation ()
overrideWorkingDirectory p = changeWorkingDirectory .= Just p

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyInvokationConfig :: (B9Config -> B9Config) -> B9Invokation ()
modifyInvokationConfig f = initialConfigOverride . customB9Config %= f

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyPermanentConfig :: (B9Config -> B9Config) -> B9Invokation ()
modifyPermanentConfig g = permanentB9ConfigUpdate %= go
  where
    go Nothing  = go (Just return)
    go (Just f) = Just (\cp -> f cp >>= modifyConfigParser g)











