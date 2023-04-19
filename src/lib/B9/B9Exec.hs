-- | This modules contains support for external command execution.
--
-- @since 0.5.65
module B9.B9Exec
  ( cmd,
    cmdStdout,
    cmdInteractive,
    hostCmdEither,
    hostCmdStdoutEither,
    hostCmd,
    hostCmdStdIn,
    Timeout (..),
    ptyCmdInteractive,
    HostCommandStdin (..),
    HostCommandStdout (..),
  )
where

import B9.B9Config
import B9.B9Error
import B9.B9Logging
import B9.BuildInfo (BuildInfoReader, isInteractive)
import qualified Conduit as CL
import Control.Concurrent (readMVar, newMVar, modifyMVar_, threadDelay, MVar)
import Control.Concurrent.Async (Concurrently (..), race)
import Control.Eff
import qualified Control.Exception as ExcIO
import Control.Lens (view)
import Control.Monad.Trans.Control (control, embed_, restoreM)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Strict
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Functor ()
import Data.Maybe
import qualified Data.Text as Text
import GHC.Stack
import System.Exit
import System.Posix.Terminal
import System.Posix.Types
import System.Posix.Pty
import           Control.Applicative      ((*>))
import           Control.Exception (try, IOException())
import           Data.Conduit             (ConduitT, yield, (.|), runConduit, Void)
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           waitForStreamingProcess)
import           Control.Monad.IO.Class   (liftIO)


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
-- @since 2.0.0
cmdInteractive ::
  (HasCallStack, Member ExcB9 e, Member BuildInfoReader e, CommandIO e) =>
  String ->
  Eff e ()
cmdInteractive str = do
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

-- | Execute the given shell command.
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
  ok <- hostCmdEither HostCommandNoStdin str t
  case ok of
    Right _ ->
      return ()
    Left e ->
      errorExitL ("SYSTEM COMMAND FAILED: " ++ show e)

-- | Execute the given shell command and collect its standard output.
--
-- The command and the output is additionally either logged to the logfile with 'traceL' or 'errorL' or
-- written to stdout.
--
-- If the command exists with non-zero exit code, the current process exists with the same
-- exit code.
--
-- @since 3.1.0
cmdStdout ::
  (HasCallStack, Member ExcB9 e, CommandIO e) =>
  String ->
  Eff e Strict.ByteString
cmdStdout str = do
  t <- view defaultTimeout <$> getB9Config
  ok <- hostCmdStdoutEither HostCommandNoStdin HostCommandStdoutLogAndCapture str t
  case ok of
    Right (out, ExitSuccess) ->
      return out
    Right (_, e@(ExitFailure _)) ->
      errorExitL ("SYSTEM COMMAND FAILED: " ++ show e)
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
hostCmdEither inputSource cmdStr timeoutArg = do
  hostCmdStdoutEither inputSource HostCommandStdoutLog cmdStr timeoutArg

-- | Ways to process std-output.
--
-- @since 3.1.0
data HostCommandStdout a where
  -- | Write std-out to the log sink.
  HostCommandStdoutLog :: HostCommandStdout ExitCode
  -- | Write std-out to the log sink, additionally collect and return it.
  HostCommandStdoutLogAndCapture :: HostCommandStdout (Strict.ByteString, ExitCode)

data HostCommandStdoutState a where
  HostCommandStdoutStateLog :: HostCommandStdoutState ExitCode
  HostCommandStdoutStateLogAndCapture :: MVar Strict.Builder -> HostCommandStdoutState (Strict.ByteString, ExitCode)

emptyState :: (CommandIO e) => HostCommandStdout a -> Eff e (HostCommandStdoutState a)
emptyState HostCommandStdoutLog = return HostCommandStdoutStateLog
emptyState HostCommandStdoutLogAndCapture = liftIO (newMVar mempty) >>= return . HostCommandStdoutStateLogAndCapture

-- | Run a shell command defined by a string and optionally interrupt the command
-- after a given time has elapsed.
-- This is only useful for non-interactive commands.
--
-- Also provide the possibility to receive the stdout of the command. It of course be collected
-- in ram, so be sure the command doesn't have to much output.
--
-- @since 3.1.0
hostCmdStdoutEither ::
  forall e a.
  (CommandIO e) =>
  -- | A 'HostCommandStdin' to define standard input.
  -- If the value is 'HostCommandInheritStdin' then
  -- **also stdout and stderr** will be redirected to
  -- the 'Inherited' file descriptors.
  HostCommandStdin ->
  -- | A 'HostCommandStdout' to define standard output.
  -- No output will be returned in case of timeout or
  -- 'HostComandStdin' being 'HostComandInheritStdin'.
  HostCommandStdout a ->
  -- | The shell command to execute.
  String ->
  -- | An optional 'Timeout'
  Maybe Timeout ->
  Eff e (Either Timeout a)
hostCmdStdoutEither inputSource outputSinkType cmdStr timeoutArg = do
  let tag = "[" ++ printHash cmdStr ++ "]"
  traceL $ "COMMAND " ++ tag ++ ": " ++ cmdStr
  tf <- fromMaybe 1 . view timeoutFactor <$> getB9Config
  timeout <-
    fmap (TimeoutMicros . \(TimeoutMicros t) -> tf * t)
      <$> maybe
        (view defaultTimeout <$> getB9Config)
        (return . Just)
        timeoutArg
  control $ \runInIO ->
    do
      ExcIO.catch
        (runInIO (go timeout tag))
        ( \(e :: ExcIO.SomeException) -> do
            runInIO (errorL ("COMMAND " ++ tag ++ " interrupted: " ++ show e))
            runInIO (return (wrapEmptyOutputResult outputSinkType (ExitFailure 126)))
        )
      >>= restoreM
  where
    wrapEmptyOutputResult :: HostCommandStdout a -> ExitCode -> Either Timeout a
    wrapEmptyOutputResult HostCommandStdoutLog ec = Right ec
    wrapEmptyOutputResult HostCommandStdoutLogAndCapture ec = Right (mempty, ec)

    wrapOutputResult :: HostCommandStdoutState a -> ExitCode -> Eff e a
    wrapOutputResult HostCommandStdoutStateLog ec = return ec
    wrapOutputResult (HostCommandStdoutStateLogAndCapture mvar) ec = do
      value <- liftIO (readMVar mvar)
      return (Lazy.toStrict (Strict.toLazyByteString value), ec)

    go :: Maybe Timeout -> String -> Eff e (Either Timeout a)
    go timeout tag = do
      errorLC <- errorMsgProcessLogger tag
      let timer t@(TimeoutMicros micros) = do
            threadDelay micros
            return t
      stdoutState <- emptyState outputSinkType
      (cph, runCmd) <- case inputSource of
        HostCommandNoStdin -> do
          outSink <- createStdoutSink stdoutState tag
          (ClosedStream, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
          let runCmd =
                runConcurrently
                  ( Concurrently (runConduit (cpOut .| runStdoutSink outSink))
                      *> Concurrently (runConduit (cpErr .| runProcessLogger errorLC))
                      *> Concurrently (waitForStreamingProcess cph)
                  )
          return (cph, runCmd)
        HostCommandInheritStdin -> do
          (Inherited, Inherited, Inherited, cph) <- streamingProcess (shell cmdStr)
          let runCmd = waitForStreamingProcess cph
          return (cph, runCmd)
        HostCommandStdInConduit inputC -> do
          outSink <- createStdoutSink stdoutState tag
          (stdIn, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
          let runCmd =
                runConcurrently
                  ( Concurrently (runConduit (cpOut .| runStdoutSink outSink))
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
      traverse (wrapOutputResult stdoutState) e

-- | Execute the given shell command in a newly created pseudo terminal, if necessary.
--
-- @since 2.1.1
ptyCmdInteractive ::
  (HasCallStack, Member ExcB9 e, Member BuildInfoReader e, CommandIO e) =>
  Maybe Timeout ->
  String ->
  [String] ->
  Eff e ()
ptyCmdInteractive timeoutArg progName progArgs  = do
--   isInATerm <- liftIO (queryTerminal (Fd 0))
    let cmdStr = unwords (progName: progArgs)
--   if isInATerm then cmdInteractive cmdStr
--   else do
    let tag = "[" ++ printHash cmdStr ++ "]"
    traceL $ "PTY-COMMAND " ++ tag ++ ": " ++ cmdStr
    tf <- fromMaybe 1 . view timeoutFactor <$> getB9Config
    timeout <-
      fmap (TimeoutMicros . \(TimeoutMicros t) -> tf * t)
        <$> maybe
          (view defaultTimeout <$> getB9Config)
          (return . Just)
          timeoutArg
    traceLC <- traceMsgProcessLogger tag
    let timer t@(TimeoutMicros micros) = do
          threadDelay micros
          return t

        runCmd = liftIO $ do
          (pty, procH) <- spawnWithPty Nothing True progName progArgs (80, 25)
          let close = liftIO (do
                cleanupProcess (Nothing, Nothing, Nothing, procH)
                closePty pty)
              output = runConduit $ fromProcess .| runProcessLogger traceLC
              fromProcess = do
                res <- liftIO (try (readPty pty))
                case res of
                  Left (_ :: IOException) -> do
                    close
                  Right d -> do
                    yield d
                    fromProcess

          runConcurrently $
              Concurrently output *>
              Concurrently (waitForProcess procH)

    e <- liftIO (maybe (fmap Right) (race . timer) timeout runCmd)
    case e of
      Left _ ->
        errorL $ "PTY-COMMAND TIMED OUT " ++ tag
      Right ExitSuccess ->
        traceL $ "PTY-COMMAND FINISHED " ++ tag
      Right (ExitFailure ec) ->
        errorL $ "PTY-COMMAND FAILED EXIT CODE: " ++ show ec ++ " " ++ tag

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

newtype StdoutSink
  = MkStdoutSink
      {runStdoutSink :: ConduitT Strict.ByteString Void IO ()}

createStdoutSink :: (CommandIO e) => HostCommandStdoutState a -> String -> Eff e StdoutSink
createStdoutSink HostCommandStdoutStateLog tag = traceMsgProcessLogger tag >>= return . MkStdoutSink . runProcessLogger
createStdoutSink (HostCommandStdoutStateLogAndCapture _stdoutCollector) _tag = do
  logger <- runProcessLogger <$> traceMsgProcessLogger _tag
  return
    ( MkStdoutSink
       ( CL.getZipSink
           ( CL.ZipSink logger
             *> CL.ZipSink (writeToMVar _stdoutCollector)
           )
       )
    )
  where
    writeToMVar mvar = do
      chunk <- CL.await
      case chunk of
        Nothing -> return ()
        (Just val) -> liftIO $ modifyMVar_ mvar $ \old -> return (old <> Strict.byteString val)
