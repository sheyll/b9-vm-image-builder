module B9.RepositoryIOSpec
  ( spec,
  )
where

import B9 (ppShow)
import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Error
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
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable
import System.Directory
import System.Environment
import System.FilePath
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec = do
  let cleanCacheAndLookupImages mkCfg buildAction =
        withTempBuildDirs $ \cfgWithRepo -> do
          let cfg = overrideB9Config mkCfg cfgWithRepo
              buildCfg = overrideB9Config noCleanupCfg cfgWithRepo 
          x <- buildAction buildCfg 
          y <- allCachedSharedImages 
                <$> b9Build cfg 
                      (  cleanLocalRepoCache 
                      *> infoL "SEARCHING FOR SHARED IMAGES" 
                      *> getSharedImages
                      )
          return (x, y)
      shareAndLookupTestImages mkCfg =
        withTempBuildDirs $ \cfgWithRepo -> do
          let cfg = overrideB9Config mkCfg cfgWithRepo
          putStrLn (ppShow cfg)
          sharedImagesExpected <- shareTestImages cfg 
          sharedImagesActual <- allCachedSharedImages 
            <$> b9Build cfg getSharedImages
          return (sharedImagesExpected, sharedImagesActual)
      testBuilds3X2 =
        replicate 3 
        [ ( t,
            ImageTarget
              (Share t Raw KeepSize)
              (EmptyImage t Ext4 Raw (ImageSize 10 MB))
              NotMounted
          )
          | t <- ["testImg0", "testImg1"]
        ]
      shareTestImages cfg = fmap concat <$>
       forM testBuilds3X2 $ \testTargets ->
        do
          threadDelay 1200000
          forM testTargets $ \(t, dest) ->
            b9Build
              cfg
              ( assemble
                  (Artifact (IID t) (VmImages [dest] NoVmScript))
                  *> ( SharedImage (SharedImageName t)
                         <$> (SharedImageDate <$> getBuildDate)
                         <*> (SharedImageBuildId <$> getBuildId)
                         <*> pure Raw
                         <*> pure Ext4
                     )
              )
  describe "shared_image_cache_cleanup" $ do
    context "_maxLocalSharedImageRevisions == Nothing" $ do
      context "no images in cache" $ do
        it "does nothing and returns no error" $
          cleanCacheAndLookupImages noCleanupCfg (const (return ())) 
          >>= (`shouldBe` mempty) . snd
      context "two images names with each three versions" $ do
        it "removes ALL images" $
          cleanCacheAndLookupImages noCleanupCfg shareTestImages 
          >>= (`shouldBe` mempty) . snd
    context "_maxLocalSharedImageRevisions == Just 1" $ do
      context "no images in cache" $ do
        it "does nothing and returns no error" $
          cleanCacheAndLookupImages 
            (cleanupAfterBuildCfg 1) 
            (const (return ())) 
            >>= (`shouldBe` mempty) . snd
      context "two images names with each three versions" $ do
        it "retains the latest image of each subset with the same name (somehow they must procreate, right? ;)" $ do
          (generatedImages, actual) <- 
            cleanCacheAndLookupImages 
              (cleanupAfterBuildCfg 1) 
              (\c -> shareTestImages c *> 
                       (b9Build c (allCachedSharedImages <$> getSharedImages)))
          let expected = 
                fold 
                  (Map.map 
                    (Set.drop 2) 
                    (groupBySharedImageName generatedImages))
          actual `shouldBe` expected 
  -- TODO describe "pull shared images" $ do
  describe "create & share images" $ do
    describe "Without autmatic cleanup"
      $ it "returns all shared images that were built"
      $ do
        (sharedImagesExpected, sharedImagesActual) 
          <- shareAndLookupTestImages noCleanupCfg
        sharedImagesActual `shouldBe` Set.fromList sharedImagesExpected
    describe "with automatic cleanup after build enabled in _maxLocalSharedImageRevisions" $ do
      describe "with an invalid parameter _maxLocalSharedImageRevisions == Just 0" $ do
        it "does nothing and exits with error" $
          shareAndLookupTestImages (cleanupAfterBuildCfg (-1)) 
            `shouldThrow` (const True :: Selector SomeException)
      describe "with an invalid parameter _maxLocalSharedImageRevisions == Just -1" $ do
        it "does nothing and exits with error" $
          shareAndLookupTestImages (cleanupAfterBuildCfg 0) 
            `shouldThrow` (const True :: Selector B9Error)
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
      nixOutDirEnv <- lookupEnv "NIX_BUILD_TOP" -- TODO remove all this, using the TMPDIR is enough
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
