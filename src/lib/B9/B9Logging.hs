-- | This modules contains support for logging.
--
-- @since 0.5.65
module B9.B9Logging
  ( Logger (..),
    CommandIO,
    LoggerReader,
    withLogger,
    b9Log,
    traceL,
    dbgL,
    infoL,
    errorL,
    errorExitL,
    printHash,
  )
where

import B9.B9Config
import B9.B9Error
import B9.BuildInfo
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
  ( MonadBaseControl,
    liftBaseWith,
    restoreM,
  )
import Data.Hashable
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import qualified System.IO as SysIO
import Text.Printf

-- | The logger to write log messages to.
--
-- @since 0.5.65
newtype Logger
  = MkLogger
      { logFileHandle :: Maybe SysIO.Handle
      }

-- | Effect that reads a 'Logger'.
--
-- @since 0.5.65
type LoggerReader = Reader Logger

-- | Lookup the selected 'getLogVerbosity' and '_logFile' from the 'B9Config'
-- and open it.
--
-- Then run the given action; if the action crashes, the log file will be closed.
--
-- @since 0.5.65
withLogger ::
  (MonadBaseControl IO (Eff e), MonadIO (Eff e), Member B9ConfigReader e) =>
  Eff (LoggerReader ': e) a ->
  Eff e a
withLogger action = do
  lf <- _logFile <$> getB9Config
  effState <- liftBaseWith $ \runInIO ->
    let fInIO = runInIO . flip runReader action . MkLogger
     in maybe
          (fInIO Nothing)
          (\logf -> SysIO.withFile logf SysIO.AppendMode (fInIO . Just))
          lf
  restoreM effState

-- | Convenience type alias for 'Eff'ects that have a 'B9Config', a 'Logger', 'MonadIO' and 'MonadBaseControl'.
--
-- @since 0.5.65
type CommandIO e =
  ( MonadBaseControl IO (Eff e),
    MonadIO (Eff e),
    Member LoggerReader e,
    Member B9ConfigReader e
  )

type CommandLogIO e =
  ( CommandIO e, Member BuildInfoReader e
  )

traceL :: CommandIO e => String -> Eff e ()
traceL = b9Log LogTrace

dbgL :: CommandIO e => String -> Eff e ()
dbgL = b9Log LogDebug

infoL :: CommandIO e => String -> Eff e ()
infoL = b9Log LogInfo

errorL :: CommandIO e => String -> Eff e ()
errorL = b9Log LogError

errorExitL :: (CommandIO e, Member ExcB9 e) => String -> Eff e a
errorExitL e = b9Log LogError e >> throwB9Error e

b9Log :: CommandIO e => LogLevel -> String -> Eff e ()
b9Log level msg = do
  lv <- getLogVerbosity
  lfh <- logFileHandle <$> ask
  liftIO $ logImpl lv lfh level msg

logImpl :: Maybe LogLevel -> Maybe SysIO.Handle -> LogLevel -> String -> IO ()
logImpl minLevel mh level msg = do
  lm <- formatLogMsg level msg
  when (isJust minLevel && level >= fromJust minLevel) $ do
    putStr lm
    SysIO.hFlush SysIO.stdout
  when (isJust mh) $ do
    SysIO.hPutStr (fromJust mh) lm
    SysIO.hFlush (fromJust mh)

formatLogMsg :: LogLevel -> String -> IO String
formatLogMsg l msg = do
  u <- getCurrentTime
  let time = formatTime defaultTimeLocale "%H:%M:%S" u
  return $ unlines $ printf "[%s] %s - %s" (printLevel l) time <$> lines msg

printLevel :: LogLevel -> String
printLevel l = case l of
  LogNothing -> "NOTHING"
  LogError -> " ERROR "
  LogInfo -> " INFO  "
  LogDebug -> " DEBUG "
  LogTrace -> " TRACE "

printHash :: Hashable a => a -> String
printHash = printf "%x" . hash
