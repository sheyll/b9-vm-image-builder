{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B9.B9Monad ( B9
                  , run
                  , traceL
                  , dbgL
                  , infoL
                  , errorL
                  , getConfig
                  , getBuildId
                  , getBuildDir
                  , getExecEnvType
                  , cmd
                  ) where

import           B9.B9Config
import           Control.Applicative
import           Control.Exception ( bracket )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.Functor ()
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Word ( Word32 )
import           System.CPUTime
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Locale ( defaultTimeLocale )
import           System.Random ( randomIO )
import           Text.Printf
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (($$))
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process

data BuildState = BuildState { bsBuildId :: String
                             , bsCfg :: B9Config
                             , bsBuildDir :: FilePath
                             , bsProf :: [ProfilingEntry]
                             , bsCliArgs :: [String]
                             , bsEnvVars :: [(String, String)]
                             }

data ProfilingEntry = IoActionDuration NominalDiffTime
                    | LogEvent LogLevel String
                      deriving (Eq, Show)

run :: String -> B9Config -> [String] -> B9 a -> IO a
run name cfg args action = do
  buildId <- generateBuildId name
  bracket (createBuildDir buildId) removeBuildDir (run' buildId)
  where
    run' buildId buildDir = do
      let ctx = BuildState buildId cfg buildDir []
                          args (envVars cfg)
      (r, ctxOut) <- runStateT (runB9 action) ctx
      when (isJust (profileFile cfg)) $
        writeFile (fromJust (profileFile cfg))
                  (unlines $ show <$> (reverse $ bsProf ctxOut))
      return r

    createBuildDir buildId = do
      createDirectoryIfMissing True $ buildDirRoot cfg
      root <- canonicalizePath $ buildDirRoot cfg
      if uniqueBuildDirs cfg then do
        let buildDir = root </> "BUILD-" ++ buildId
        createDirectory buildDir
        return buildDir
       else do
        let buildDir = root </> "BUILD-" ++ name
        createDirectoryIfMissing True buildDir
        return buildDir

    removeBuildDir buildDir =
      when (uniqueBuildDirs cfg) $ removeDirectoryRecursive buildDir

    generateBuildId name =
      printf "%s-%08X" name <$> (randomIO :: IO Word32)


getBuildId :: B9 FilePath
getBuildId = gets bsBuildId

getBuildDir :: B9 FilePath
getBuildDir = gets bsBuildDir

getConfig :: B9 B9Config
getConfig = gets bsCfg

getExecEnvType :: B9 ExecEnvType
getExecEnvType = gets $ execEnvType . bsCfg

cmd :: String -> B9 ()
cmd cmdStr = do
  traceL $ "COMMAND: " ++ cmdStr
  (Inherited, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
  cmdLogger <- getCmdLogger
  e <- liftIO $ runConcurrently $
       Concurrently (cpOut $$ cmdLogger) *>
       Concurrently (cpErr $$ cmdLogger) *>
       Concurrently (waitForStreamingProcess cph)
  checkExitCode e
  where
    getCmdLogger = do
      lc <- gets $ logConfig . bsCfg
      return $ (CL.mapM_ (logImpl lc LogTrace . B.unpack))

    checkExitCode ExitSuccess =
      traceL $ "COMMAND SUCCESS"
    checkExitCode ec@(ExitFailure e) = do
      errorL $ printf "COMMAND '%s' FAILED: %i!" cmdStr e
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
  lc <- gets $ logConfig . bsCfg
  modify $ \ ctx -> ctx { bsProf = LogEvent level msg : bsProf ctx }
  B9 $ liftIO $ logImpl lc level msg

logImpl :: LogConfig -> LogLevel -> String -> IO ()
logImpl (ToStdOut minLevel) level msg
  | level >= minLevel = formatLogMsg level msg >>= putStr
  | otherwise = return ()
logImpl (ToStdErr minLevel) level msg
  | level >= minLevel =
      formatLogMsg level msg >>= hPutStr stderr
  | otherwise = return ()
logImpl (ToFile f minLevel) level msg
  | level >= minLevel =
      formatLogMsg level msg >>= appendFile f
  | otherwise = return ()
logImpl (LogTo cfgs) level msg =
  mapM_ (\ cfg -> logImpl cfg level msg) cfgs

formatLogMsg :: LogLevel -> String -> IO String
formatLogMsg l msg = do
  utct <- getCurrentTime
  let time = formatTime defaultTimeLocale "%H:%M:%S" utct
  return $ unlines $ printf "[%s] %s - %s" (printLevel l) time <$> lines msg

printLevel :: LogLevel -> String
printLevel l = case l of
  LogNothing -> "NOTHING"
  LogError   -> " ERROR "
  LogInfo    -> " INFO  "
  LogDebug   -> " DEBUG "
  LogTrace   -> " TRACE "

newtype B9 a = B9 { runB9 :: StateT BuildState IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState BuildState )

instance MonadIO B9 where
  liftIO m = do
    start <- B9 $ liftIO getCurrentTime
    res <- B9 $ liftIO m
    stop <- B9 $ liftIO getCurrentTime
    let durMS = IoActionDuration $ (stop `diffUTCTime` start)
    modify $
      \ ctx ->
       ctx { bsProf = durMS : bsProf ctx }
    return res
