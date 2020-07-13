-- | This modules contains support for external command execution.
--
-- @since 0.5.65
module B9.B9Exec
  ( cmd,
    hostCmdEither,
    hostCmd,
    hostCmdStdIn,
    Timeout (..),
    HostCommandStdin (..),
  )
where

import B9.B9Config
import B9.B9Error
import B9.B9Logging
-- import qualified Data.ByteString.Lazy as Lazy

import qualified Conduit as CL
import Control.Concurrent
import Control.Concurrent.Async (Concurrently (..), race)
import Control.Eff
import qualified Control.Exception as ExcIO
import Control.Lens (view)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (control, embed_, restoreM)
import qualified Data.ByteString as Strict
import Data.Conduit
  ( (.|),
    ConduitT,
    Void,
    runConduit,
  )
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Functor ()
import qualified Data.Text as Text
import GHC.Stack
import System.Exit

-- | Execute the given shell command.
--
-- If 'isInteractive' is true, the standard-in will be passed to the external command,
-- and all output of the program will be directed to standard-out.
--
-- The command and the output is either logged to the logfile with 'traceL' or 'errorL' or
-- written to stdout.
--
-- If the command exists with non-zero exit code, the current process exists with the same
-- exit code.
--
-- @since 0.5.65
cmd ::
  (HasCallStack, Member ExcB9 e, CommandIO e) =>
  String ->
  Eff e ()
cmd str = do
  t <- view defaultTimeout <$> getB9Config
  inheritStdIn <- isInteractive
  ok <-
    if inheritStdIn
      then hostCmdEither HostCommandInheritStdin str t
      else hostCmdEither HostCommandNoStdin str t
  case ok of
    Right _ ->
      return ()
    Left e ->
      errorExitL ("SYSTEM COMMAND FAILED: " ++ show e)

-- | Run a shell command defined by a string and optionally interrupt the command
-- after a given time has elapsed.
-- If the shell command did not exit with 'ExitSuccess', or the timer elapsed,
-- a 'B9Error' is thrown.
--
-- This is only useful for non-interactive commands.
--
-- @since 1.0.0
hostCmd ::
  (CommandIO e, Member ExcB9 e) =>
  -- | The shell command to execute.
  String ->
  -- | An optional 'Timeout'
  Maybe Timeout ->
  -- | An action that performs the shell command and returns 'True' on success
  Eff e Bool
hostCmd cmdStr timeout = do
  res <- hostCmdEither HostCommandNoStdin cmdStr timeout
  case res of
    Left e ->
      throwB9Error ("Command timed out: " ++ show cmdStr ++ " " ++ show e)
    Right (ExitFailure ec) -> do
      errorL ("Command exited with error code: " ++ show cmdStr ++ " " ++ show ec)
      return False
    Right ExitSuccess ->
      return True

-- | Like 'hostCmd' but with std-input attached.
--
-- @since 1.0.0
hostCmdStdIn ::
  (CommandIO e, Member ExcB9 e) =>
  -- | A 'HostCommandStdin' to define standard input.
  -- If the value is 'HostCommandInheritStdin' then
  -- **also stdout and stderr** will be redirected to
  -- the 'Inherited' file descriptors.
  HostCommandStdin ->
  -- | The shell command to execute.
  String ->
  -- | An optional 'Timeout'
  Maybe Timeout ->
  -- | An action that performs the shell command and returns 'True' on success
  Eff e Bool
hostCmdStdIn hostStdIn cmdStr timeout = do
  res <- hostCmdEither hostStdIn cmdStr timeout
  case res of
    Left e ->
      throwB9Error ("Command timed out: " ++ show cmdStr ++ " " ++ show e)
    Right (ExitFailure ec) -> do
      errorL ("Command exited with error code: " ++ show cmdStr ++ " " ++ show ec)
      return False
    Right ExitSuccess ->
      return True

-- | Ways to process std-input.
--
-- @since 1.0.0
data HostCommandStdin
  = -- | Disbale std-in
    HostCommandNoStdin
  | -- | Inherit std-in
    HostCommandInheritStdin
  | -- | Produce std-in
    HostCommandStdInConduit (ConduitT () Strict.ByteString IO ())

-- | Run a shell command defined by a string and optionally interrupt the command
-- after a given time has elapsed.
-- This is only useful for non-interactive commands.
--
-- @since 1.0.0
hostCmdEither ::
  forall e.
  (CommandIO e) =>
  -- | A 'HostCommandStdin' to define standard input.
  -- If the value is 'HostCommandInheritStdin' then
  -- **also stdout and stderr** will be redirected to
  -- the 'Inherited' file descriptors.
  HostCommandStdin ->
  -- | The shell command to execute.
  String ->
  -- | An optional 'Timeout'
  Maybe Timeout ->
  Eff e (Either Timeout ExitCode)
hostCmdEither inputSource cmdStr timeout = do
  let tag = "[" ++ printHash cmdStr ++ "]"
  traceL $ "COMMAND " ++ tag ++ ": " ++ cmdStr
  control $ \runInIO ->
    do
      ExcIO.catch
        (runInIO (go tag))
        ( \(e :: ExcIO.SomeException) -> do
            runInIO (errorL ("COMMAND " ++ tag ++ " interrupted: " ++ show e))
            runInIO (return (Right (ExitFailure 126) :: Either Timeout ExitCode))
        )
      >>= restoreM
  where
    go :: String -> Eff e (Either Timeout ExitCode)
    go tag = do
      traceLC <- traceMsgProcessLogger tag
      errorLC <- errorMsgProcessLogger tag
      let timer t@(TimeoutMicros micros) = do
            threadDelay micros
            return t
      (cph, runCmd) <- case inputSource of
        HostCommandNoStdin -> do
          (ClosedStream, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
          let runCmd =
                runConcurrently
                  ( Concurrently (runConduit (cpOut .| runProcessLogger traceLC))
                      *> Concurrently (runConduit (cpErr .| runProcessLogger errorLC))
                      *> Concurrently (waitForStreamingProcess cph)
                  )
          return (cph, runCmd)
        HostCommandInheritStdin -> do
          (Inherited, Inherited, Inherited, cph) <- streamingProcess (shell cmdStr)
          let runCmd = waitForStreamingProcess cph
          return (cph, runCmd)
        HostCommandStdInConduit inputC -> do
          (stdIn, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
          let runCmd =
                runConcurrently
                  ( Concurrently (runConduit (cpOut .| runProcessLogger traceLC))
                      *> Concurrently (runConduit (cpErr .| runProcessLogger errorLC))
                      *> Concurrently (runConduit (inputC .| stdIn))
                      *> Concurrently (waitForStreamingProcess cph)
                  )
          return (cph, runCmd)
      e <- liftIO (maybe (fmap Right) (race . timer) timeout runCmd)
      closeStreamingProcessHandle cph
      case e of
        Left _ ->
          errorL $ "COMMAND TIMED OUT " ++ tag
        Right ExitSuccess ->
          traceL $ "COMMAND FINISHED " ++ tag
        Right (ExitFailure ec) ->
          errorL $ "COMMAND FAILED EXIT CODE: " ++ show ec ++ " " ++ tag
      return e

newtype ProcessLogger
  = MkProcessLogger
      {runProcessLogger :: ConduitT Strict.ByteString Void IO ()}

traceMsgProcessLogger :: (CommandIO e) => String -> Eff e ProcessLogger
traceMsgProcessLogger = mkMsgProcessLogger traceL

errorMsgProcessLogger :: (CommandIO e) => String -> Eff e ProcessLogger
errorMsgProcessLogger = mkMsgProcessLogger errorL

mkMsgProcessLogger :: (CommandIO e) => (String -> Eff e ()) -> String -> Eff e ProcessLogger
mkMsgProcessLogger logFun prefix = do
  logIO <-
    embed_
      ( \logBytes ->
          logFun (prefix ++ ": " ++ Text.unpack logBytes)
      )
  return
    ( MkProcessLogger
        ( CB.lines
            .| CL.decodeUtf8LenientC
            .| CL.mapM_ (liftIO . logIO)
        )
    )
