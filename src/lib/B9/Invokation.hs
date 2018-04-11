-- |
module B9.Invokation ( B9Invokation()
                     , invokeB9
                     , defaultB9Invokation
                     , overrideWorkingDirectory
                     , overrideB9ConfigPath
                     , runtimeInvokationConfig
                     , modifyPermanentConfig
                     ) where

import B9.B9Config
import Data.ConfigFile.B9Extras
import Control.Monad.Reader
import Control.Lens
import Data.Semigroup as Sem
import Data.Maybe ( fromMaybe )
import Text.Printf ( printf )

-- | A type encapsulating all configuration adaptions happening before any
-- actual 'B9' build actions are performed by 'invokeB9'.
-- This type can be used to change the B9 configuration, the permanent, as well
-- as the transient.
data B9Invokation =
    B9Invokation { _initialConfigOverride   :: B9ConfigOverride
                 , _permanentB9ConfigUpdate :: Maybe (ConfigParser -> Either CPError ConfigParser)
                 }

makeLenses ''B9Invokation

defaultB9Invokation :: B9Invokation
defaultB9Invokation = B9Invokation (B9ConfigOverride Nothing mempty) Nothing

-- | Run a 'B9Invokation'.
-- If the user has no B9 config file yet, a default config file will be created.
invokeB9 :: B9Invokation -> ReaderT B9Config IO a -> IO a
invokeB9 st buildAction = do
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
            runReaderT buildAction runtimeCfg

-- | Override the B9 configuration path
overrideB9ConfigPath :: SystemPath -> B9Invokation -> B9Invokation
overrideB9ConfigPath p = initialConfigOverride . customB9ConfigPath .~ Just p

-- | Override the current working directory during execution of the actions
-- added with 'doAfterConfiguration'.
overrideWorkingDirectory :: FilePath -> B9Invokation -> B9Invokation
overrideWorkingDirectory p = runtimeInvokationConfig . buildDirRoot .~ Just p

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
runtimeInvokationConfig :: Lens' B9Invokation B9Config
runtimeInvokationConfig = initialConfigOverride . customB9Config

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyPermanentConfig :: (B9Config -> B9Config) -> B9Invokation -> B9Invokation
modifyPermanentConfig g = permanentB9ConfigUpdate %~ go
  where
    go Nothing  = go (Just return)
    go (Just f) = Just (\cp -> f cp >>= modifyConfigParser g)