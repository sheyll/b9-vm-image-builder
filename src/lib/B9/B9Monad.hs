{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module B9.B9Monad ( B9
                  , run
                  , traceL
                  , dbgL
                  , infoL
                  , errorL
                  , getConfigParser
                  , getConfig
                  , getBuildId
                  , getBuildDir
                  , getExecEnvType
                  , getRepository
                  , getRepositoryCache
                  , cmd
                  ) where

import           B9.B9Config
import           B9.ConfigUtils
import           B9.Repository
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
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Locale ( defaultTimeLocale )
import           System.Random ( randomIO )
import           Text.Printf
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (($$))
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process

data BuildState = BuildState { bsBuildId :: String
                             , bsCfgParser :: ConfigParser
                             , bsCfg :: B9Config
                             , bsBuildDir :: FilePath
                             , bsRepository :: Maybe Repository
                             , bsRepositoryCache :: Maybe Repository
                             , bsProf :: [ProfilingEntry]
                             }

data ProfilingEntry = IoActionDuration NominalDiffTime
                    | LogEvent LogLevel String
                      deriving (Eq, Show)

run :: String -> ConfigParser -> B9Config -> B9 a -> IO a
run name cfgParser cfg action = do
  buildId <- if uniqueBuildDirs cfg then generateBuildId name
             else return name
  bracket (createBuildDir buildId) removeBuildDir (run' buildId)
  where
    run' buildId buildDir = do
      let ctx = BuildState buildId cfgParser cfg buildDir repo repoCache []
          repo = repository cfg >>= return . lookupRepository cfgParser
          repoCache = repositoryCache cfg >>= return . lookupRepository cfgParser
      (r, ctxOut) <- runStateT (runB9 action) ctx
      when (isJust (profileFile cfg)) $
        writeFile (fromJust (profileFile cfg))
                  (unlines $ show <$> (reverse $ bsProf ctxOut))
      return r

    createBuildDir buildId = do
      if uniqueBuildDirs cfg then do
        let subDir = "BUILD-" ++ buildId
        buildDir <- resolveBuildDir subDir
        createDirectory buildDir
        canonicalizePath buildDir
       else do
        let subDir = "BUILD-" ++ name
        buildDir <- resolveBuildDir subDir
        createDirectoryIfMissing True buildDir
        canonicalizePath buildDir
      where
        resolveBuildDir f = do
          case buildDirRoot cfg of
           Nothing ->
             return f
           Just root' -> do
             createDirectoryIfMissing True root'
             root <- canonicalizePath root'
             return $ root </> f

    removeBuildDir buildDir =
      when (uniqueBuildDirs cfg && not (keepTempDirs cfg))
      $ removeDirectoryRecursive buildDir

    generateBuildId name' =
      printf "%s-%08X" name' <$> (randomIO :: IO Word32)


getBuildId :: B9 FilePath
getBuildId = gets bsBuildId

getBuildDir :: B9 FilePath
getBuildDir = gets bsBuildDir

getConfigParser :: B9 ConfigParser
getConfigParser = gets bsCfgParser

getConfig :: B9 B9Config
getConfig = gets bsCfg

getExecEnvType :: B9 ExecEnvType
getExecEnvType = gets (execEnvType . bsCfg)

getRepository :: B9 (Maybe Repository)
getRepository = gets bsRepository

getRepositoryCache :: B9 (Maybe Repository)
getRepositoryCache = gets bsRepositoryCache

cmd :: String -> B9 ()
cmd cmdStr = do
  traceL $ "COMMAND: " ++ cmdStr
  (Inherited, cpOut, cpErr, cph) <- streamingProcess (shell cmdStr)
  cmdLogger <- getCmdLogger
  e <- liftIO $ runConcurrently $
       Concurrently (cpOut $$ cmdLogger LogTrace) *>
       Concurrently (cpErr $$ cmdLogger LogInfo) *>
       Concurrently (waitForStreamingProcess cph)
  checkExitCode e
  where
    getCmdLogger = do
      lv <- gets $ verbosity . bsCfg
      lf <- gets $ logFile . bsCfg
      return $ \ level -> (CL.mapM_ (logImpl lv lf level . B.unpack))

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
  lv <- gets $ verbosity . bsCfg
  lf <- gets $ logFile . bsCfg
  modify $ \ ctx -> ctx { bsProf = LogEvent level msg : bsProf ctx }
  B9 $ liftIO $ logImpl lv lf level msg

logImpl :: Maybe LogLevel -> Maybe FilePath -> LogLevel -> String -> IO ()
logImpl minLevel mf level msg = do
  lm <- formatLogMsg level msg
  when (isJust minLevel && level >= fromJust minLevel) (putStr lm)
  when (isJust mf) (appendFile (fromJust mf) lm)

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
    let durMS = IoActionDuration (stop `diffUTCTime` start)
    modify $
      \ ctx ->
       ctx { bsProf = durMS : bsProf ctx }
    return res
