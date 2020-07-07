module B9.RepositoryIOSpec
  ( spec,
  )
where

import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Monad
import B9.BuildInfo
import B9.B9Error
import B9.DiskImages
import B9.Repository
import B9.RepositoryIO
import B9.Vm
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec =
  describe "RepositoryIO" $ do
    let shareAndLookupTestImages mkCfg =
          withTempRepo $ \cfgWithRepo -> do
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
            sharedImagesActual `shouldBe` Set.take 1 (Set.fromList sharedImagesExpected)
        describe "with a valid parameter _maxLocalSharedImageRevisions == Just 2" $ do
          it "returns the latest two of all shared images that were built" $ do
            (sharedImagesExpected, sharedImagesActual) <-
              shareAndLookupTestImages (cleanupAfterBuildCfg 2)
            sharedImagesActual `shouldBe` Set.take 2 (Set.fromList sharedImagesExpected)
        describe "with a valid parameter _maxLocalSharedImageRevisions == Just 3000" $ do
          it "returns the latest 3000 of all shared images that were built" $ do
            (sharedImagesExpected, sharedImagesActual) <-
              shareAndLookupTestImages (cleanupAfterBuildCfg 3000)
            sharedImagesActual `shouldBe` Set.take 3000 (Set.fromList sharedImagesExpected)



type SharedImageTable = Map SharedImageName RepoImagesMap

imageTableRetainLatest :: Int -> SharedImageTable -> SharedImageTable
imageTableRetainLatest maxRevisions it =
  error "TODO"  


repoTableToImageTable :: RepoImagesMap -> SharedImageTable
repoTableToImageTable =
  Map.foldrWithKey 
    (\repo imgs accR ->
       foldr 
        (\img ->
            Map.alter 
              (\mExisting ->
                case mExisting of 
                  Just existing ->
                    Just 
                      (Map.alter 
                        (Just . (maybe (Set.singleton img) (Set.insert img)))
                        repo 
                        existing)

                  Nothing ->
                    Just (Map.singleton repo (Set.singleton img)))
              (sharedImageName img)
        )
        accR
        imgs
    )
    Map.empty


noCleanupCfg :: B9Config -> B9Config
noCleanupCfg c =
  c {_maxLocalSharedImageRevisions = Nothing}

cleanupAfterBuildCfg :: Int -> B9Config -> B9Config
cleanupAfterBuildCfg n c =
  c {_maxLocalSharedImageRevisions = Just n}

b9Build :: HasCallStack => B9Config -> B9 a -> IO a
b9Build cfg e =
  runB9ConfigAction
    ( localB9Config
        (const cfg)
        (runB9 e)
    )

withTempRepo :: HasCallStack => (B9Config -> IO a) -> IO a
withTempRepo k =
  bracket acquire release use
  where
    acquire = do
      repoRelPath <- printf "RepositoryIOSpec-test-repo-%U" <$> randomUUID
      let tmpRepoPath = InTempDir repoRelPath
      ensureSystemPath tmpRepoPath
      return tmpRepoPath
    release =
      removePathForcibly <=< resolve
    use tmpRepoPath =
      let cfg = defaultB9Config {_repositoryCache = Just tmpRepoPath}
       in k cfg
