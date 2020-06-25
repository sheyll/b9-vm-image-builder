{-# LANGUAGE Strict #-}

-- | This modules contains support for external command execution.
--
-- @since 0.5.65
module B9.B9Exec
  ( cmd,
    hostCmdEither,
    hostCmd,
    CommandTimeout (..),
  )
where

import System.IO (hSetBuffering, BufferMode(LineBuffering))
import B9.B9Config
import B9.B9Error
import B9.B9Logging
import Control.Concurrent
import Control.Concurrent.Async (Concurrently (..), race)
import Control.Eff
import qualified Control.Exception as ExcIO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (embed_)
import qualified Data.ByteString as Strict
-- import qualified Data.ByteString.Lazy as Lazy
import Data.Conduit
  ( (.|),
    ConduitT,
    Void,
    runConduit,
  )
import qualified Data.Conduit.Binary      as CB
import qualified Conduit as CL
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Functor ()
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import System.Exit
import Text.Printf

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
cmd :: CommandIO e => String -> Eff e ()
cmd str = do
  inheritStdIn <- isInteractive
  if inheritStdIn then interactiveCmd str else nonInteractiveCmd str

interactiveCmd :: forall e. CommandIO e => String -> Eff e ()
interactiveCmd str = void (cmdWithStdIn True str :: Eff e Inherited)

nonInteractiveCmd :: forall e. CommandIO e => String -> Eff e ()
-- TODO if we use 'ClosedStream' we get an error from 'virsh console'
-- complaining about a missing controlling tty. Original source line:
-- nonInteractiveCmd str = void (cmdWithStdIn False str :: B9 ClosedStream)
nonInteractiveCmd str = void (cmdWithStdIn False str :: Eff e Inherited)

cmdWithStdIn ::
  (CommandIO e, InputSource stdin) => Bool -> String -> Eff e stdin
cmdWithStdIn toStdOut cmdStr = do
  traceL $ "COMMAND: " ++ cmdStr
  traceLIO <-
    embed_
      (traceL . Text.unpack . Text.decodeUtf8With Text.lenientDecode)
  errorLIO <-
    embed_
      (errorL . Text.unpack . Text.decodeUtf8With Text.lenientDecode)
  let errorLC = CL.mapM_ (liftIO . errorLIO)
  let traceLC =
        if toStdOut
          then CL.mapM_ Strict.putStr
          else CL.mapM_ (liftIO . traceLIO)
  (cpIn, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
  e <-
    liftIO
      $ runConcurrently
      $ Concurrently (runConduit (cpOut .| traceLC))
        *> Concurrently (runConduit (cpErr .| errorLC))
        *> Concurrently (waitForStreamingProcess cph)
  closeStreamingProcessHandle cph
  checkExitCode e
  return cpIn
  where
    checkExitCode ExitSuccess =
      traceL $ printf "COMMAND '%s' exited with exit code: 0" cmdStr
    checkExitCode ec@(ExitFailure e) = do
      errorL $ printf "COMMAND '%s' exited with exit code: %i" cmdStr e
      liftIO $ exitWith ec

-- | Run a shell command defined by a string and optionally interrupt the command
-- after a given time has elapsed.
-- If the shell command did not exit with 'ExitSuccess', or the timer elapsed,
-- a 'B9Error' is thrown.
--
-- This is only useful for non-interactive commands.
--
-- @since 1.0.0
hostCmd :: (CommandIO e, Member ExcB9 e) => String -> Maybe CommandTimeout -> Eff e Bool
hostCmd cmdStr timeout = do
  res <- hostCmdEither cmdStr timeout
  case res of
    Left e ->
      throwB9Error ("Command timed out: " ++ show cmdStr ++ " " ++ show e)
    Right (ExitFailure ec) -> do
      errorL ("Command exited with error code: " ++ show cmdStr ++ " " ++ show ec)
      return False
    Right ExitSuccess ->
      return True

-- | Run a shell command defined by a string and optionally interrupt the command
-- after a given time has elapsed.
-- This is only useful for non-interactive commands.
--
-- @since 1.0.0
hostCmdEither ::
  (CommandIO e) => String -> Maybe CommandTimeout -> Eff e (Either CommandTimeout ExitCode)
hostCmdEither cmdStr timeout = do
  let tag = "[" ++ printHash cmdStr ++ "]"
  traceL $ "COMMAND " ++ tag ++ ": " ++ cmdStr
  (ClosedStream, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
  liftIO $ do 
    -- hSetBuffering cpOut LineBuffering
    hSetBuffering cpErr LineBuffering
  traceLC <- traceMsgProcessLogger tag
  errorLC <- errorMsgProcessLogger tag
  let timer t@(CommandTimeout micros) = do
        threadDelay micros
        return t
      runCmd =
        ExcIO.catch -- TODO
         (runConcurrently 
           (Concurrently (putStrLn "XXXXXXXXXXXXXXXXXXXXXXXX" >> runConduit (cpOut .| (CL.mapM_ (\b -> putStrLn $ tag ++ ": " ++ show b))))
            *> Concurrently (runConduit (CL.sourceHandle cpErr .| runProcessLogger errorLC))
            *> Concurrently (waitForStreamingProcess cph)))
        (\(e :: ExcIO.SomeException) -> do 
            putStrLn ("COMMAND " ++ tag++ " interrupted: " ++ show e)
            return (ExitFailure 666))
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

data CommandTimeout = CommandTimeout Int
  deriving (Show)

newtype ProcessLogger
  = MkProcessLogger
      {runProcessLogger :: ConduitT Strict.ByteString Void IO ()}

traceMsgProcessLogger :: (CommandIO e) => String -> Eff e ProcessLogger
traceMsgProcessLogger = mkMsgProcessLogger (liftIO . putStrLn) 

errorMsgProcessLogger :: (CommandIO e) => String -> Eff e ProcessLogger
errorMsgProcessLogger = mkMsgProcessLogger errorL

mkMsgProcessLogger :: (CommandIO e) => (String -> Eff e ()) -> String -> Eff e ProcessLogger 
mkMsgProcessLogger logFun prefix = do
  logIO <-
    embed_
      ( \logBytes ->
          logFun (prefix ++ ": " ++ Text.unpack logBytes)
      )
  return (MkProcessLogger (      
                            -- CL.chunksOfCE 64  .| 
                            CL.decodeUtf8LenientC      
                               --   .| CL.linesUnboundedC 
                                  .| CL.mapM_ (liftIO . logIO)))
