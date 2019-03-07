-- | This modules contains support for external command execution.
--
-- @since 0.5.65
module B9.B9Exec
  ( cmd
  ) where

import B9.B9Config
import B9.B9Logging
import Control.Concurrent.Async (Concurrently(..))
import Control.Eff
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (embed_)
import qualified Data.ByteString.Char8 as Strict
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.List as CL
import Data.Conduit.Process
import Data.Functor ()
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
  if inheritStdIn
    then interactiveCmd str
    else nonInteractiveCmd str

interactiveCmd ::
     forall e. CommandIO e
  => String
  -> Eff e ()
interactiveCmd str = void (cmdWithStdIn True str :: Eff e Inherited)

nonInteractiveCmd ::
     forall e. CommandIO e
  => String
  -> Eff e ()
-- TODO if we use 'ClosedStream' we get an error from 'virsh console'
-- complaining about a missing controlling tty. Original source line:
-- nonInteractiveCmd str = void (cmdWithStdIn False str :: B9 ClosedStream)
nonInteractiveCmd str = void (cmdWithStdIn False str :: Eff e Inherited)

cmdWithStdIn :: (CommandIO e, InputSource stdin) => Bool -> String -> Eff e stdin
cmdWithStdIn toStdOut cmdStr = do
  traceL $ "COMMAND: " ++ cmdStr
  traceLIO <- embed_ (traceL . Strict.unpack)
  errorLIO <- embed_ (errorL . Strict.unpack)
  let errorLC = CL.mapM_ (liftIO . errorLIO)
  let traceLC =
        if toStdOut
          then CL.mapM_ Strict.putStr
          else CL.mapM_ (liftIO . traceLIO)
  (cpIn, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
  e <-
    liftIO $
    runConcurrently $
    Concurrently (runConduit (cpOut .| traceLC)) *> Concurrently (runConduit (cpErr .| errorLC)) *>
    Concurrently (waitForStreamingProcess cph)
  checkExitCode e
  return cpIn
  where
    checkExitCode ExitSuccess = traceL $ printf "COMMAND '%s' exited with exit code: 0" cmdStr
    checkExitCode ec@(ExitFailure e) = do
      errorL $ printf "COMMAND '%s' exited with exit code: %i" cmdStr e
      liftIO $ exitWith ec