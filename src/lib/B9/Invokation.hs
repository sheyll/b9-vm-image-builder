module B9.Invokation ( B9Invokation()
                     , invokeB9
                     , overrideWorkingDirectory
                     , mergeAfterConfigurationActionResults
                     , ignoreActionResults
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
newtype B9Invokation res a = B9Inv {runB9Invokation :: StateT (InternalState res) IO a}
  deriving (MonadState (InternalState res), Monad, Applicative, Functor, MonadIO)

-- | Internal state of the 'B9Invokation'
data InternalState a = IS { _initialConfigOverride   :: B9ConfigOverride
                          , _permanentB9ConfigUpdate :: Maybe (ConfigParser -> Either CPError ConfigParser)
                          , _changeWorkingDirectory  :: Maybe FilePath
                          , _buildAction             :: Maybe (ReaderT B9Config IO a)
                          }

makeLenses ''InternalState

initialState :: InternalState a
initialState = IS (B9ConfigOverride Nothing mempty) Nothing Nothing Nothing

-- | Run a 'B9Invokation'.
-- If the user has no B9 config file yet, a default config file will be created.
invokeB9 :: B9Invokation res () -> IO (Maybe res)
invokeB9 act = do
    st <- execStateT (runB9Invokation act) initialState
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
            let
                runtimeCfg =
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
                            runReaderT (sequence (st ^. buildAction)) runtimeCfg
                        )
                    )
            completeBuildAction

-- | Add an action to be executed once the configuration has been done.
-- The action will be called with the result of the previous action defined by
-- this function. In case this is the first action added 'Nothing' is passed.
doAfterConfiguration :: (Maybe a -> ReaderT B9Config IO a) -> B9Invokation a ()
doAfterConfiguration action = buildAction %= appendOrSet
  where
    appendOrSet Nothing  = Just (action Nothing)
    appendOrSet (Just x) = Just (x >>= action . Just)

-- | Combine the result of a given 'B9Invokation' and the results of the action
-- defined by 'doAfterConfiguration' in the current state.
mergeAfterConfigurationActionResults
    :: (Maybe a -> Maybe b -> ReaderT B9Config IO b)
    -> B9Invokation a ()
    -> B9Invokation b ()
mergeAfterConfigurationActionResults f ba = do
    st <- liftIO $ execStateT (runB9Invokation ba) initialState
    let fb mbAction = Just $ do
            ma <- sequence (st ^. buildAction)
            mb <- sequence mbAction
            f ma mb
    buildAction %= fb

-- | Ignore the results of the actions defined by 'doAfterConfiguration'.
-- All actions will be executed, but the result will be replaced with '()'.
ignoreActionResults :: B9Invokation a () -> B9Invokation () ()
ignoreActionResults =
    mergeAfterConfigurationActionResults (const (const (return ())))

-- | Override the B9 configuration path
overrideB9ConfigPath :: SystemPath -> B9Invokation a ()
overrideB9ConfigPath p = initialConfigOverride . customB9ConfigPath .= Just p

-- | Override the current working directory during execution of the actions
-- added with 'doAfterConfiguration'.
overrideWorkingDirectory :: FilePath -> B9Invokation a ()
overrideWorkingDirectory p = changeWorkingDirectory .= Just p

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyInvokationConfig :: (B9Config -> B9Config) -> B9Invokation a ()
modifyInvokationConfig f = initialConfigOverride . customB9Config %= f

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyPermanentConfig :: (B9Config -> B9Config) -> B9Invokation a ()
modifyPermanentConfig g = permanentB9ConfigUpdate %= go
  where
    go Nothing  = go (Just return)
    go (Just f) = Just (\cp -> f cp >>= modifyConfigParser g)





























