{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Test.Hspec(Spec, it, shouldBe, HasCallStack, describe)
import Test.QuickCheck (property, (===), (==>))
import Text.Printf
import qualified Data.Text as Text
import Data.ConfigFile.B9Extras
  ( 
    CPError
  )
import NeatInterpolation as Neat  
import Data.Either (isRight)

spec :: HasCallStack => Spec
spec = do
  it "forall valid configs: parse . render == id" $ property $
    \cfg -> 
      let actual = renderThenParseB9Config cfg 
      in isRight actual ==> (Right cfg === actual)
  
  describe "parse textual configuration" $ do  
    let 
      exampleConfig = Text.unpack [Neat.text|
          [global]
          build_dir_root: Nothing
          keep_temp_dirs: False
          log_file: Nothing
          max_cached_shared_images: Just 2
          repository: Nothing
          repository_cache: Just (InB9UserDir "repo-cache")
          unique_build_dirs: True
          verbosity: Just LogNothing
          timeout_factor: 3
          default_timeout_seconds: 10
          ext4_attributes: ["attr1", "attr2"]
        |] 
    it "correctly parses verbosity" $ do
      cfg <- withConfig exampleConfig getB9Config
      _verbosity cfg `shouldBe` Just LogNothing

    it "correctly parses timeout_factor" $ do
      cfg <- withConfig exampleConfig getB9Config
      _timeoutFactor cfg `shouldBe` Just 3

    it "correctly parses default_timeout" $ do
      cfg <- withConfig exampleConfig getB9Config
      _defaultTimeout cfg `shouldBe` Just (TimeoutMicros 10_000_000)

    it "correctly parses ext4_attributes" $ do 
      cfg <- withConfig exampleConfig getB9Config
      _ext4Attributes cfg `shouldBe` ["attr1", "attr2"]
      
    it "correctly parses missing ext4_attributes" $ do 
      let exampleConfigNoExt4 = Text.unpack [Neat.text|
          [global]
          build_dir_root: Nothing
          keep_temp_dirs: False
          log_file: Nothing
          max_cached_shared_images: Just 2
          repository: Nothing
          repository_cache: Just (InB9UserDir "repo-cache")
          unique_build_dirs: True
          verbosity: Just LogNothing
          timeout_factor: 3
          default_timeout_seconds: 10
        |] 
      cfg <- withConfig exampleConfigNoExt4 getB9Config
      _ext4Attributes cfg `shouldBe` ["^64bit"]
      

renderThenParseB9Config :: B9Config -> Either CPError B9Config
renderThenParseB9Config = b9ConfigToCPDocument >=> parseB9Config

withConfig :: String -> B9 a -> IO a
withConfig cfgFileContents testAction = 
  withTempBuildDirs $ \cfg -> do
    let cfgFileName = 
          fromMaybe 
            (error "Internal Error")
            (cfg ^. customDefaulB9ConfigPath)
    cfgFilePath <- resolve cfgFileName 
    writeFile cfgFilePath cfgFileContents
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
