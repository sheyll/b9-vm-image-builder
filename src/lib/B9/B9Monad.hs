{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| Definition of the B9 monad. It encapsulates logging, very basic command
execution profiling, a reader for the "B9.B9Config" and access to the
current build id, the current build directory and the artifact to build.

This module is used by the _effectful_ functions in this library.
-}
module B9.B9Monad ( B9 , runB9Monad , b9Log, traceL , dbgL , infoL , errorL , getConfigParser
, getConfig , getBuildId , getBuildDate , getBuildDir , getExecEnvType ,
getSelectedRemoteRepo , getRemoteRepos , getRepoCache , cmd, cmdRaw ) where

import           B9.B9Config
import           B9.ExecEnv
import           B9.Repository
import           Control.Applicative
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception ( bracket )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.ConfigFile as CF
import           Data.Functor ()
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Word ( Word32 )
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified System.IO as SysIO
import           System.Random ( randomIO )
import           Text.Printf

data BuildState = BuildState { bsBuildId :: String
                             , bsBuildDate :: String
                             , bsCfgParser :: ConfigParser
                             , bsCfg :: B9Config
                             , bsBuildDir :: FilePath
                             , bsLogFileHandle :: Maybe SysIO.Handle
                             , bsSelectedRemoteRepo :: Maybe RemoteRepo
                             , bsRemoteRepos :: [RemoteRepo]
                             , bsRepoCache :: RepoCache
                             , bsProf :: [ProfilingEntry]
                             , bsStartTime :: UTCTime
                             , bsInheritStdIn :: Bool
                             }

data ProfilingEntry = IoActionDuration NominalDiffTime
                    | LogEvent LogLevel String
                      deriving (Eq, Show)

runB9Monad :: ConfigParser -> B9Config -> B9 a -> IO a
runB9Monad cfgParser cfg action = do
  buildId <- generateBuildId
  now <- getCurrentTime
  withBuildDir buildId (withLogFile . run' buildId now)

  where
    withLogFile f =
      maybe (f Nothing)
            (\logf -> SysIO.withFile logf SysIO.AppendMode (f . Just))
            (logFile cfg)

    withBuildDir buildId = bracket (createBuildDir buildId) removeBuildDir

    run' buildId now buildDir logFileHandle = do
      maybe (return ()) setCurrentDirectory (buildDirRoot cfg)
      -- Check repositories
      repoCache <- initRepoCache (fromMaybe defaultRepositoryCache (repositoryCache cfg))
      let remoteRepos = getConfiguredRemoteRepos cfgParser
          buildDate = formatTime undefined "%F-%T" now
      remoteRepos' <- mapM (initRemoteRepo repoCache) remoteRepos
      let ctx = BuildState
                  buildId
                  buildDate
                  cfgParser
                  cfg
                  buildDir
                  logFileHandle
                  selectedRemoteRepo
                  remoteRepos'
                  repoCache
                  []
                  now
                  (interactive cfg)
          selectedRemoteRepo = do
            sel <- repository cfg
            lookupRemoteRepo remoteRepos sel
             <|> error
                   (printf
                      "selected remote repo '%s' not configured, valid remote repos are: '%s'"
                      sel
                      (show remoteRepos))
      (r, ctxOut) <- runStateT (runB9 wrappedAction) ctx
      -- Write a profiling report
      when (isJust (profileFile cfg)) $
        writeFile (fromJust (profileFile cfg))
                  (unlines $ show <$> reverse (bsProf ctxOut))
      return r

    createBuildDir buildId =
      if uniqueBuildDirs cfg
        then do
          let subDir = "BUILD-" ++ buildId
          buildDir <- resolveBuildDir subDir
          createDirectory buildDir
          canonicalizePath buildDir
        else do
          let subDir = "BUILD-" ++ buildId
          buildDir <- resolveBuildDir subDir
          createDirectoryIfMissing True buildDir
          canonicalizePath buildDir

      where
        resolveBuildDir f =
          case buildDirRoot cfg of
            Nothing ->
              return f
            Just root' -> do
              createDirectoryIfMissing True root'
              root <- canonicalizePath root'
              return $ root </> f

    removeBuildDir buildDir =
      when (uniqueBuildDirs cfg && not (keepTempDirs cfg)) $ removeDirectoryRecursive buildDir

    generateBuildId = printf "%08X" <$> (randomIO :: IO Word32)

    -- Run the action build action
    wrappedAction = do
      startTime <- gets bsStartTime
      r <- action
      now <- liftIO getCurrentTime
      let duration = show (now `diffUTCTime` startTime)
      infoL (printf "DURATION: %s" duration)
      return r


getBuildId :: B9 FilePath
getBuildId = gets bsBuildId

getBuildDate :: B9 String
getBuildDate = gets bsBuildDate

getBuildDir :: B9 FilePath
getBuildDir = gets bsBuildDir

getConfigParser :: B9 ConfigParser
getConfigParser = gets bsCfgParser

getConfig :: B9 B9Config
getConfig = gets bsCfg

getExecEnvType :: B9 ExecEnvType
getExecEnvType = gets (execEnvType . bsCfg)

getSelectedRemoteRepo :: B9 (Maybe RemoteRepo)
getSelectedRemoteRepo = gets bsSelectedRemoteRepo

getRemoteRepos :: B9 [RemoteRepo]
getRemoteRepos = gets bsRemoteRepos

getRepoCache :: B9 RepoCache
getRepoCache = gets bsRepoCache

cmd :: String -> B9 ()
cmd str = do
  traceL $ "COMMAND: " ++ str
  execCmd $ shell str

cmdRaw :: FilePath -> [String] -> B9 ()
cmdRaw p args = do
  traceL $ printf "RAW COMMAND: %s %s" p (show args)
  execCmd $ proc p args

execCmd :: CreateProcess -> B9 ()
execCmd c = do
  inheritStdIn <- gets bsInheritStdIn
  if inheritStdIn
     then interactiveCmd c
     else nonInteractiveCmd c

interactiveCmd :: CreateProcess -> B9 ()
interactiveCmd p = void (cmdWithStdIn True p :: B9 Inherited)

nonInteractiveCmd :: CreateProcess -> B9 ()
-- TODO if we use 'ClosedStream' we get an error from 'virsh console'
-- complaining about a missing controlling tty. Original source line:
-- nonInteractiveCmd str = void (cmdWithStdIn False str :: B9 ClosedStream)
nonInteractiveCmd p = void (cmdWithStdIn False p :: B9 Inherited)

cmdWithStdIn :: (InputSource stdin) => Bool -> CreateProcess -> B9 stdin
cmdWithStdIn toStdOut p = do
  cmdLogger <- getCmdLogger
  let outPipe = if toStdOut then CL.mapM_ B.putStr
                else cmdLogger LogTrace
  (cpIn, cpOut, cpErr, cph) <- streamingProcess p
  e <- liftIO $ runConcurrently $
         Concurrently (cpOut $$ outPipe) *>
         Concurrently (cpErr $$ cmdLogger LogInfo) *>
         Concurrently (waitForStreamingProcess cph)
  checkExitCode e
  return cpIn

  where

    getCmdLogger = do
      lv <- gets $ verbosity . bsCfg
      lfh <- gets bsLogFileHandle
      return $ \level -> CL.mapM_ (logImpl lv lfh level . B.unpack)

    checkExitCode ExitSuccess =
      traceL "COMMAND SUCCESS"
    checkExitCode ec@(ExitFailure e) = do
      errorL $ printf "COMMAND FAILED: %i!" e
      liftIO $ exitWith ec

traceL :: String -> B9 ()
traceL = b9Log LogTrace

dbgL :: String -> B9 ()
dbgL = b9Log LogDebug

infoL :: String -> B9 ()
infoL = b9Log LogInfo

errorL :: String -> B9 ()
errorL = b9Log LogError

b9Log :: LogLevel -> String -> B9 ()
b9Log level msg = do
  lv <- gets $ verbosity . bsCfg
  lfh <- gets bsLogFileHandle
  modify $ \ ctx -> ctx { bsProf = LogEvent level msg : bsProf ctx }
  B9 $ liftIO $ logImpl lv lfh level msg

logImpl :: Maybe LogLevel -> Maybe SysIO.Handle -> LogLevel -> String -> IO ()
logImpl minLevel mh level msg = do
  lm <- formatLogMsg level msg
  when (isJust minLevel && level >= fromJust minLevel) (putStr lm)
  when (isJust mh) $ do
    SysIO.hPutStr (fromJust mh) lm
    SysIO.hFlush (fromJust mh)

formatLogMsg :: LogLevel -> String -> IO String
formatLogMsg l msg = do
  utct <- getCurrentTime
  let time = formatTime defaultTimeLocale "%H:%M:%S" utct
  return $ unlines $ printf "[%s] %s - %s" (printLevel l) time <$> lines msg

printLevel :: LogLevel -> String
printLevel l =
  case l of
    LogNothing -> "NOTHING"
    LogError   -> " ERROR "
    LogInfo    -> " INFO  "
    LogDebug   -> " DEBUG "
    LogTrace   -> " TRACE "

newtype B9 a = B9 { runB9 :: StateT BuildState IO a }
  deriving (Functor, Applicative, Monad, MonadState BuildState)

instance MonadIO B9 where
  liftIO m = do
    start <- B9 $ liftIO getCurrentTime
    res <- B9 $ liftIO m
    stop <- B9 $ liftIO getCurrentTime
    let durMS = IoActionDuration (stop `diffUTCTime` start)
    modify $
      \ctx -> ctx { bsProf = durMS : bsProf ctx }
    return res
