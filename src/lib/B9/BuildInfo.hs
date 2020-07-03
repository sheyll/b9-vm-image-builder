-- | Provide information about the current build.
--
-- This module provides build meta information like
-- build directory, build-id and build-time.
--
-- @since 0.5.65
module B9.BuildInfo
  ( getBuildId,
    getBuildDate,
    getBuildDir,
    withBuildInfo,
    BuildInfoReader,
  )
where

import B9.B9Config
import B9.B9Error
import B9.B9Logging
import B9.Environment
import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Exception (bracket)
import Control.Lens ((?~))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
  ( MonadBaseControl,
    control,
  )
import Data.Functor ()
import Data.Hashable
import Data.Time.Clock
import Data.Time.Format
import GHC.Stack
import System.Directory
import System.FilePath
import Text.Printf

-- | Build meta information.
--
-- @since 0.5.65
data BuildInfo
  = BuildInfo
      { bsBuildId :: String,
        bsBuildDate :: String,
        bsBuildDir :: FilePath,
        bsStartTime :: UTCTime
      }
  deriving (Eq, Show)

-- | Type alias for a 'BuildInfo' 'Reader'
--
-- @since 0.5.65
type BuildInfoReader = Reader BuildInfo

-- | Create the build directories, generate (hash) the build-id and execute the given action.
--
-- Bindings added to the text template parameter environment:
--
-- * @projectRoot@ the directory that contains the sources of the project to build
-- * @buildDir@ the temporary directory used store the build artifacts passed into- or outof the build
--
-- Unless '_keepTempDirs' is @True@ clean up the build directories after the actions
-- returns - even if the action throws a runtime exception.
--
-- @since 0.5.65
withBuildInfo ::
  ( Lifted IO e,
    MonadBaseControl IO (Eff e),
    Member B9ConfigReader e,
    Member ExcB9 e,
    Member EnvironmentReader e,
    Member LoggerReader e,
    HasCallStack
  ) =>
  Eff (BuildInfoReader ': e) a ->
  Eff e a
withBuildInfo action = withRootDir $ do
  now <- lift getCurrentTime -- TODO reproducability: make configurable how the build-date is generated, e.g. by using always 1970/01/01-00:01
  let buildDate = formatTime undefined "%F-%T" now -- TODO make configurable how the build date is formatted
  buildId <- generateBuildId buildDate
  withBuildDir buildId (runImpl buildId buildDate now)
  where
    withRootDir f = do
      mRoot <- _projectRoot <$> getB9Config
      root <- lift $ case mRoot of
        Nothing -> getCurrentDirectory >>= canonicalizePath
        Just rootIn -> do
          createDirectoryIfMissing True rootIn
          canonicalizePath rootIn
      localB9Config
        (projectRoot ?~ root)
        (addLocalStringBinding ("projectRoot", root) f)
    generateBuildId buildDate = do
      -- TODO generate a proper, reproducable build id!
      unqiueBuildDir <- _uniqueBuildDirs <$> getB9Config
      cfgHash <- hash . show <$> getB9Config
      if unqiueBuildDir
        then return (printf "%08X-%08X" cfgHash (hash buildDate))
        else return (printf "%08X" cfgHash)
    withBuildDir buildId f = do
      root <- _projectRoot <$> getB9Config
      cfg <- getB9Config
      control $ \runInIO ->
        bracket (createBuildDir root) (removeBuildDir cfg) (runInIO . f)
      where
        createBuildDir root = do
          -- TODO allow config option to enable build dirs outside of the projectRoot
          let buildDir = case root of
                Just r -> r </> "BUILD-" ++ buildId
                Nothing -> "BUILD-" ++ buildId
          createDirectoryIfMissing True buildDir
          canonicalizePath buildDir
        removeBuildDir cfg buildDir =
          when (_uniqueBuildDirs cfg && not (_keepTempDirs cfg)) $
            removeDirectoryRecursive buildDir
    runImpl buildId buildDate startTime buildDir =
      let ctx = BuildInfo buildId buildDate buildDir startTime
       in runReader ctx wrappedAction
      where
        wrappedAction = do
          rootD <- getProjectRoot
          traceL (printf "Project Root Directory: %s" rootD)
          buildD <- getBuildDir
          traceL (printf "Build Directory:        %s" buildD)
          r <- addLocalStringBinding ("buildDir", buildD) action
          tsAfter <- liftIO getCurrentTime
          let duration = show (tsAfter `diffUTCTime` startTime)
          infoL (printf "DURATION: %s" duration)
          return r

-- Run the action build action
getBuildId :: Member BuildInfoReader e => Eff e String
getBuildId = bsBuildId <$> ask

getBuildDate :: Member BuildInfoReader e => Eff e String
getBuildDate = bsBuildDate <$> ask

getBuildDir :: Member BuildInfoReader e => Eff e FilePath
getBuildDir = bsBuildDir <$> ask
