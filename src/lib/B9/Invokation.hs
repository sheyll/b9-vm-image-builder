module B9.Invokation ( B9CustomConfig(..)
                     , B9Invokation()
                     , invokeB9
                     , getInvokationConfig
                     , modifyInvokationConfig
                     , askInvokationConfigParser
                     , askInvokationConfigPath) where

import B9.B9Config
import B9.ConfigUtils
import qualified B9.LibVirtLXC as LibVirtLXC
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Semigroup as Sem
import Text.Printf ( printf )

-- | Parameters controlling the execution of every build command.
data B9CustomConfig = B9CustomConfig
  { customB9ConfigPath :: Maybe SystemPath
  , customB9Config     :: B9Config
  }

newtype B9Invokation a = B9Inv {runB9Invokation :: RWST (Maybe SystemPath, ConfigParser) () B9Config IO a}
  deriving (MonadReader ConfigParser, MonadState B9Config
            , MonadIO, Monad, Applicative, Functor)

-- | Merge 'B9CustomConfig' with the configuration from the main b9 config
-- file. If the file does not exists, a new config file with the given
-- configuration will be written. The return value is a parser for the config
-- file. Returning the raw config file parser allows modules unkown to
-- 'B9.B9Config' to add their own values to the shared config file.
initConfigFile :: MonadIO m => B9CustomConfig -> m ConfigParser
initConfigFile customConfig = do
    writeInitialB9Config b9ConfigPath
                         (customB9Config customConfig)
                         LibVirtLXC.setDefaultConfig
    readB9Config (customB9ConfigPath customConfig)

invokeB9 :: B9Invokation a -> B9CustomConfig -> IO a
invokeB9 act customConfig = do
    cp <- initConfigFile customConfig
    let parsedCfg' = parseB9Config cp
    case parsedCfg' of
        Left e -> fail (printf "B9 Failed to start: %s" e)
        Right parsedCfg ->
            let
                cfg =
                    defaultB9Config
                        Sem.<> parsedCfg
                        Sem.<> customB9Config customConfig
            in
                do
                    (res, _, _) <- runRWST
                        (runB9Invokation act)
                        (customB9ConfigPath customConfig, cp)
                        cfg
                    return res

-- | Return the current 'B9Config'.
getInvokationConfig :: B9Invokation B9Config
getInvokationConfig = Control.Monad.RWS.get

-- | Modify the current 'B9Config'. The modifications are not reflected in the
-- config file or the 'ConfigParser' returned by 'askInvokationConfigParser'.
modifyInvokationConfig :: (B9Config -> B9Config) -> B9Invokation ()
modifyInvokationConfig = Control.Monad.RWS.modify

-- | Return the 'ConfigParser' that contains represents the initial
-- configuration file contents as well as the custom configuration changes
-- from 'B9CustomConfig'.
askInvokationConfigParser :: B9Invokation ConfigParser
askInvokationConfigParser = Control.Monad.RWS.asks snd

-- | Return the path to the 'B9Config' file, ''
askInvokationConfigPath :: B9Invokation (Maybe SystemPath)
askInvokationConfigPath = Control.Monad.RWS.asks fst