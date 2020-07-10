module B9.Shake.SharedImageRulesSpec
  ( spec,
  )
where

import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Monad
import B9.DiskImages
import B9.Repository
import B9.RepositoryIO
import B9.Shake.SharedImageRules
import B9.Vm
import Control.Exception
import Control.Monad
import qualified Data.Set as Set
import System.Directory
import System.Environment
import System.FilePath
import System.IO.B9Extras
import Test.Hspec
import Text.Printf
import Development.Shake as Shake


testShakeBuild :: B9ConfigOverride -> IO ()
testShakeBuild cfg = shake shakeOptions $ do 
  enableSharedImageRules cfg
  customSharedImageAction (SharedImageName "test") $ 
            liftIO 
            (b9Build
              cfg
              (void (assemble
                  (Artifact (IID "test-image") 
                    (VmImages 
                      [ 
                      ImageTarget
                        (Share "test" Raw KeepSize)
                        (EmptyImage "test" Ext4 Raw (ImageSize 10 MB))
                        NotMounted
                      ] 
                      NoVmScript)))))
  action (needSharedImage (SharedImageName "test"))

spec :: HasCallStack => Spec
spec = do
  context "missing shared image" $ do   
    it "builds a missing image" $ do
      withTempBuildDirs $ \cfg -> do
        testShakeBuild cfg 
        actualImages <- b9Build cfg (allCachedSharedImages <$> getSharedImages)
        Set.size actualImages `shouldBe` 1

b9Build :: HasCallStack => B9ConfigOverride -> B9 a -> IO a
b9Build cfg effect =
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



