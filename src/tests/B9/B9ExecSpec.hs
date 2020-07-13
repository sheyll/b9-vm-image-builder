{-# LANGUAGE NumericUnderscores #-}

module B9.B9ExecSpec
  ( spec,
  )
where

import B9 (ppShow)
import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Error
import B9.B9Exec
import B9.B9Logging
import B9.B9Monad
import B9.BuildInfo
import B9.DiskImages
import B9.Repository
import B9.RepositoryIO
import B9.Vm
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
import System.Environment
import System.FilePath
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec =
  context "varying_default_timeout_config" $ do
    describe "Nothing" $ do
      describe
        "cmd"
        ( it
            "does not crash if a command is stuck for more than one second"
            ( cmdWrapper Nothing "sleep 1" `shouldReturn` ()
            )
        )
    describe "Just one_second" $ do
      let timeout = 1
      describe
        ""
        ( it
            "crashes if a command is stuck for more than one second"
            ( ( cmdWrapper (Just timeout) (printf "sleep %d" (1 + timeout))
                  `shouldThrow` (const True :: Selector B9Error)
              )
            )
        )

cmdWrapper :: HasCallStack => Maybe Int -> String -> IO ()
cmdWrapper timeoutSeconds cmdStr =
  withTempBuildDirs $ \cfgOverride -> do
    let t = TimeoutMicros . (* 1_000_000) <$> timeoutSeconds
        effect = cmd cmdStr
        cfg = overrideDefaultTimeout t cfgOverride
    runB9ConfigActionWithOverrides
      (runB9 effect)
      cfg

withTempBuildDirs :: HasCallStack => (B9ConfigOverride -> IO a) -> IO a
withTempBuildDirs k =
  bracket acquire release use
  where
    acquire = do
      nixOutDirEnv <- lookupEnv "NIX_BUILD_TOP"
      let rootDir = maybe InTempDir (((.) . (.)) Path (</>)) nixOutDirEnv
      repoRelPath <- printf "testsRepositoryIOSpec-test-repo-%U" <$> randomUUID
      buildRelPath <- printf "RepositoryIOSpec-root-%U" <$> randomUUID
      cfgRelPath <- printf "RepositoryIOSpec-b9cfg-%U" <$> randomUUID
      let tmpRepoPath = rootDir ("tests" </> repoRelPath)
          tmpBuildPath = rootDir ("tests" </> buildRelPath)
          tmpCfgPath = rootDir ("tests" </> cfgRelPath)
      ensureSystemPath tmpRepoPath
      ensureSystemPath tmpBuildPath
      tmpBuildPathFileName <- resolve tmpBuildPath
      return (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath)
    release (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath) = do
      let cleanupTmpPath = removePathForcibly <=< resolve
      cleanupTmpPath tmpRepoPath
      cleanupTmpPath tmpCfgPath
      removePathForcibly tmpBuildPathFileName
    use (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath) =
      let mkCfg cfgIn =
            cfgIn
              { _repositoryCache = Just tmpRepoPath,
                _projectRoot = Just tmpBuildPathFileName
              }
          oCfg =
            overrideB9Config
              mkCfg
              ( overrideWorkingDirectory
                  tmpBuildPathFileName
                  ( overrideDefaultB9ConfigPath
                      tmpCfgPath
                      noB9ConfigOverride
                  )
              )
       in k oCfg
