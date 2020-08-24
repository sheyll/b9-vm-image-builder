module B9.DiskImageBuilderSpec (spec) where

import Test.Hspec 
import B9
import Control.Exception
import Control.Eff
import System.Directory
import System.Environment
import System.Process
import Control.Arrow ((>>>))

spec :: Spec
spec = do
    it "can extract the virtual size from qemu-img info output" $ do
      e <- b9Wrapper [] $ do
        d <- getBuildDir
        let outFile = d </> "test.raw"
        materializeImageSource
          (EmptyImage "test" Ext4 Raw (ImageSize 10 MB))
          (Image outFile Raw Ext4)
        getVirtualSizeForRawImage outFile
      e `shouldBe` Right (10 * 1024 * 1024)

    it "passes the mkfs.ext4 options defined in the B9Config" $ do
      let expectedOptions = ["^metadata_csum", "64bit"]
      actual <- b9Wrapper expectedOptions $ do
        d <- getBuildDir
        let outFile = d </> "test.raw"
        materializeImageSource 
          (EmptyImage "test" Ext4 Raw (ImageSize 10 MB))
          (Image outFile Raw Ext4)
        lift (readProcess "tune2fs" ["-l", outFile] "")
      let fsOptions = lines >>> map (stripPrefix "Filesystem features:") >>> catMaybes >>> mconcat >>> words $ actual
      fsOptions `shouldContain` ["64bit"]
      fsOptions `shouldNotContain` ["metadata_csum"]

b9Wrapper :: HasCallStack => [String] -> B9 a -> IO a
b9Wrapper ext4TestAttributes effect =
  withTempBuildDirs $ \cfgOverride ->
    let cfg = overrideExt4Attributes ext4TestAttributes cfgOverride
     in runB9ConfigActionWithOverrides (runB9 effect) cfg

withTempBuildDirs :: HasCallStack => (B9ConfigOverride -> IO a) -> IO a
withTempBuildDirs k =
  bracket acquire release use
  where
    acquire = do
      nixOutDirEnv <- lookupEnv "NIX_BUILD_TOP"
      let rootDir = maybe InTempDir (((.) . (.)) Path (</>)) nixOutDirEnv
      repoRelPath <- printf "testsDiskImageBuilderSpec-test-repo-%U" <$> randomUUID
      buildRelPath <- printf "DiskImageBuilderSpec-root-%U" <$> randomUUID
      cfgRelPath <- printf "DiskImageBuilderSpec-b9cfg-%U" <$> randomUUID
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
