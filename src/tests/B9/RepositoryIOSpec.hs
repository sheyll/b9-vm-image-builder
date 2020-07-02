module B9.RepositoryIOSpec (spec) where

import B9.Artifact.Readable
import B9.Artifact.Readable.Interpreter (assemble)
import B9.B9Config
import B9.B9Monad
import B9.BuildInfo
import B9.DiskImages
import B9.RepositoryIO
import B9.Vm
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import System.Directory
import System.IO.B9Extras
import Test.Hspec
import Text.Printf

spec :: HasCallStack => Spec
spec =
  describe "RepositoryIO" $ do
    describe "Without autmatic cleanup" $ do
      describe "getSharedImages" $ do
        it "returns all shared images that were built" $ do
          let multipleTestTargets =
                [ ( t,
                    ImageTarget
                      (Share t Raw KeepSize)
                      (EmptyImage t Ext4 Raw (ImageSize 10 MB))
                      NotMounted
                  )
                  | t <- ["testImg0", "testImg1", "testImg2"]
                ]
          (buildIds, sharedImages) <-
            withTempRepo $ \cfgWithRepo -> do
              buildIds <-
                concat
                  <$> replicateM
                    3
                    ( do
                        threadDelay 2000000
                        forM multipleTestTargets $ \(t, dest) -> do
                          b9Build
                            (noCleanupCfg cfgWithRepo)
                            ( assemble
                                (Artifact (IID "test") (VmImages [dest] NoVmScript))
                                *> ((SharedImageName t,) . SharedImageBuildId <$> getBuildId)
                            )
                    )
              sharedImages <-
                b9Build (noCleanupCfg cfgWithRepo) getSharedImages
              return (buildIds, sharedImages)
          let sharedImageBuildIds =
                [(sharedImageName s, sharedImageBuildId s) | (Cache, cs) <- sharedImages, s <- cs]
          sharedImageBuildIds `shouldBe` buildIds

noCleanupCfg :: B9Config -> B9Config
noCleanupCfg c =
  c {_maxLocalSharedImageRevisions = Nothing}

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
