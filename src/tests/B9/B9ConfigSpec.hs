{-# LANGUAGE NumericUnderscores #-}

module B9.B9ConfigSpec
  ( spec,
  )
where

import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
import B9.B9Config
import B9.B9Monad
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.IO.B9Extras
import Test.Hspec(Spec, it, HasCallStack)
import Test.QuickCheck (property, (===))
import Text.Printf
import Data.ConfigFile.B9Extras
  ( CPDocument,
    CPError,
    CPGet,
    CPOptionSpec,
    CPReadException (..),
    addSectionCP,
    emptyCP,
    mergeCP,
    readCP,
    readCPDocument,
    setShowCP,
    toStringCP,
  )

spec :: HasCallStack => Spec
spec = 
  it "forall valid configs: parse . render == id" $ property $
    \cfg -> Right cfg === renderThenParseB9Config cfg
   
renderThenParseB9Config :: B9Config -> Either CPError B9Config
renderThenParseB9Config = b9ConfigToCPDocument >=> parseB9Config


writeThenReadConfig :: B9Config -> IO B9Config
writeThenReadConfig cfgIn = error "TODO"
  
withConfig :: String -> B9 a -> IO a
withConfig cfgFileContents testAction = 
  withTempBuildDirs $ \cfg -> do
    let cfgFileName = 
          fromMaybe 
            (error "Internal Error")
            (cfg ^. customDefaulB9ConfigPath)
    runB9ConfigActionWithOverrides (runB9 testAction) cfg


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
