module B9.RepositoryIOSpec
  ( spec,
  )
where

import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Error
import B9.B9Monad
import B9.BuildInfo
import B9.DiskImages
import B9.Repository
import B9.RepositoryIO
import B9.Vm
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Data.Set as Set
import System.FilePath
import System.Environment
import System.Directory
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec =
  describe "RepositoryIO" $ do
    let shareAndLookupTestImages mkCfg =
          withTempBuildDirs $ \cfgWithRepo -> do
            let cfg = mkCfg cfgWithRepo
            sharedImagesExpected <- concat <$> replicateM 3 (shareTestImages cfg)
            sharedImagesActual <- allCachedSharedImages <$> b9Build cfg getSharedImages
            return (sharedImagesExpected, sharedImagesActual)
          where
            multipleTestTargets =
              [ ( t,
                  ImageTarget
                    (Share t Raw KeepSize)
                    (EmptyImage t Ext4 Raw (ImageSize 10 MB))
                    NotMounted
                )
                | t <- ["testImg0", "testImg1"]
              ]
            shareTestImages cfg =
              do
                threadDelay 1200000
                forM multipleTestTargets $ \(t, dest) ->
                  b9Build
                    cfg
                    ( assemble
                        (Artifact (IID t) (VmImages [dest] NoVmScript))
                        *> (SharedImage (SharedImageName t) <$> (SharedImageDate <$> getBuildDate) <*> (SharedImageBuildId <$> getBuildId) <*> pure Raw <*> pure Ext4)
                    )
    describe "getSharedImages" $ do
      describe "Without autmatic cleanup"
        $ it "returns all shared images that were built"
        $ do
          (sharedImagesExpected, sharedImagesActual) <- shareAndLookupTestImages noCleanupCfg
          sharedImagesActual `shouldBe` Set.fromList sharedImagesExpected
      describe "with automatic cleanup after build enabled in _maxLocalSharedImageRevisions" $ do
        describe "with an invalid parameter _maxLocalSharedImageRevisions == Just 0" $ do
          it "does nothing and exits with error" $
            shareAndLookupTestImages (cleanupAfterBuildCfg (-1)) `shouldThrow` (const True :: Selector SomeException)
        describe "with an invalid parameter _maxLocalSharedImageRevisions == Just -1" $ do
          it "does nothing and exits with error" $
            shareAndLookupTestImages (cleanupAfterBuildCfg 0) `shouldThrow` (const True :: Selector B9Error)
        describe "with a valid parameter _maxLocalSharedImageRevisions == Just 1" $ do
          it "returns the latest of all shared images that were built" $ do
            (sharedImagesExpected, sharedImagesActual) <-
              shareAndLookupTestImages (cleanupAfterBuildCfg 1)
            sharedImagesActual `shouldBe` keepNLatestSharedImages 1 (Set.fromList sharedImagesExpected)
        describe "with a valid parameter _maxLocalSharedImageRevisions == Just 2" $ do
          it "returns the latest two of all shared images that were built" $ do
            (sharedImagesExpected, sharedImagesActual) <-
              shareAndLookupTestImages (cleanupAfterBuildCfg 2)
            sharedImagesActual `shouldBe` keepNLatestSharedImages 2 (Set.fromList sharedImagesExpected)
        describe "with a valid parameter _maxLocalSharedImageRevisions == Just 3000" $ do
          it "returns the latest 3000 of all shared images that were built" $ do
            (sharedImagesExpected, sharedImagesActual) <-
              shareAndLookupTestImages (cleanupAfterBuildCfg 3000)
            sharedImagesActual `shouldBe` Set.fromList sharedImagesExpected

noCleanupCfg :: B9Config -> B9Config
noCleanupCfg c =
  c {_maxLocalSharedImageRevisions = Nothing}

cleanupAfterBuildCfg :: Int -> B9Config -> B9Config
cleanupAfterBuildCfg n c =
  c {_maxLocalSharedImageRevisions = Just n}

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
      ensureSystemPath tmpCfgPath
      tmpBuildPathFileName <- resolve tmpBuildPath
      return (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath)
    release (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath) = do
      let cleanupTmpPath = removePathForcibly <=< resolve
      cleanupTmpPath tmpRepoPath 
      cleanupTmpPath tmpCfgPath 
      removePathForcibly tmpBuildPathFileName
    use (tmpRepoPath, tmpBuildPathFileName, tmpCfgPath) =
      let cfg = 
                defaultB9Config {_repositoryCache = Just tmpRepoPath
                                ,_projectRoot = Just tmpBuildPathFileName}
          oCfg = overrideB9Config (const cfg) 
                  (overrideWorkingDirectory tmpBuildPathFileName
                    (overrideB9ConfigPath tmpCfgPath
                    noB9ConfigOverride
                  ))
       in k oCfg
