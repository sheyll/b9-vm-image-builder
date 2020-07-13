{-# LANGUAGE NumericUnderscores #-}

module B9.B9ExecSpec
  ( spec,
  )
where

import B9.B9Config
import B9.B9Error
import B9.B9Exec
import B9.B9Monad
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec = do
  context
    "timeout_factor is Nothing"
    ( do
        let tf = Nothing
        context
          "default_timeout_seconds is Nothing "
          ( do
              let t = Nothing
              context
                "commands finish in time"
                ( do
                    context
                      "cmd"
                      (it "returns" (cmdWrapper t tf "sleep 1" `shouldReturn` ()))
                    context
                      "hostCmd without timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" Nothing)
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                    context
                      "hostCmd with timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" (Just (TimeoutMicros 2_000_000)))
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                )
              context
                "commands take too long"
                ( context
                    "hostCmd with timeout"
                    ( it
                        "it returns an error"
                        ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" (Just (TimeoutMicros 100_000)))
                            `shouldReturn` Left (TimeoutMicros 100_000)
                        )
                    )
                )
          )
        context
          "default_timeout_seconds is Just 1.5 second "
          ( do
              let t = Just (TimeoutMicros 1_500_000)
              context
                "commands finish in time"
                ( do
                    context
                      "cmd"
                      (it "returns" (cmdWrapper t tf "sleep 1" `shouldReturn` ()))
                    context
                      "hostCmd without timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" Nothing)
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                    context
                      "hostCmd with timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 2" (Just (TimeoutMicros 3_000_000)))
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                )
              context
                "commands take too long"
                ( do
                    context
                      "cmd"
                      (it "throws in error" (cmdWrapper t tf "sleep 2" `shouldThrow` (const True :: Selector B9Error)))
                    context
                      "hostCmd without timeout"
                      ( it
                          "returns Left"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 2" Nothing)
                              `shouldReturn` Left (TimeoutMicros 1_500_000)
                          )
                      )
                    context
                      "hostCmd with timeout"
                      ( it
                          "returns Left"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" (Just (TimeoutMicros 100_000)))
                              `shouldReturn` Left (TimeoutMicros 100_000)
                          )
                      )
                )
          )
    )
  context
    "timeout_factor is Just 2"
    ( do
        let tf = Just 2
        context
          "default_timeout_seconds is Just .75 seconds "
          ( do
              let t = Just (TimeoutMicros 750_000)
              context
                "commands finish in time"
                ( do
                    context
                      "cmd"
                      (it "returns" (cmdWrapper t tf "sleep 1" `shouldReturn` ()))
                    context
                      "hostCmd without timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" Nothing)
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                    context
                      "hostCmd with timeout"
                      ( it
                          "returns Right True"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 2" (Just (TimeoutMicros 1_500_000)))
                              `shouldReturn` Right ExitSuccess
                          )
                      )
                )
              context
                "commands take too long"
                ( do
                    context
                      "cmd"
                      (it "throws in error" (cmdWrapper t tf "sleep 2" `shouldThrow` (const True :: Selector B9Error)))
                    context
                      "hostCmd without timeout"
                      ( it
                          "returns Left"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 2" Nothing)
                              `shouldReturn` Left (TimeoutMicros 1_500_000)
                          )
                      )
                    context
                      "hostCmd with timeout"
                      ( it
                          "returns Left"
                          ( b9Wrapper t tf (hostCmdEither HostCommandNoStdin "sleep 1" (Just (TimeoutMicros 100_000)))
                              `shouldReturn` Left (TimeoutMicros 200_000)
                          )
                      )
                )
          )
    )

cmdWrapper :: HasCallStack => Maybe Timeout -> Maybe Int -> String -> IO ()
cmdWrapper t tf = b9Wrapper t tf . cmd

b9Wrapper :: HasCallStack => Maybe Timeout -> Maybe Int -> B9 a -> IO a
b9Wrapper t tf effect =
  withTempBuildDirs $ \cfgOverride ->
    let cfg = overrideTimeoutFactor tf (overrideDefaultTimeout t cfgOverride)
     in runB9ConfigActionWithOverrides (runB9 effect) cfg

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
