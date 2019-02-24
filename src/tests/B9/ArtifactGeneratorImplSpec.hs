module B9.ArtifactGeneratorImplSpec (spec) where
import Test.Hspec
import Data.Text ()

import B9.ArtifactGenerator
import B9.ArtifactGeneratorImpl
import B9.Content.Environment as Env
import B9.DiskImages
import B9.ExecEnv
import B9.Vm
import B9.ShellScript

spec :: Spec
spec =
  describe "assemble" $ do

     it "replaces '$...' variables in SourceImage Image file paths" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [transientCOW "${variable}" ""] NoVmScript
           expected = transientCOW "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in SourceImage 'From' names" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [transientShared "${variable}" ""] NoVmScript
           expected = transientShared "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name of a shared image" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [shareCOW "${variable}" ""] NoVmScript
           expected = shareCOW "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name and path of a live installer image" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [liveInstallerCOWImage "${variable}" ""] NoVmScript
           expected = liveInstallerCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the file name of an image exported as LocalFile" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [localCOWImage "${variable}" ""] NoVmScript
           expected = localCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in mount point of an image" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [localCOWImage "" "${variable}"] NoVmScript
           expected = localCOWImage "" "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in shared directory source and mount point (RO)" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [] (emptyScriptWithSharedDirRO "${variable}")
           expected = emptyScriptWithSharedDirRO "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in shared directory source and mount point (RW)" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [] (emptyScriptWithSharedDirRW "${variable}")
           expected = emptyScriptWithSharedDirRW "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in VmImages build script instructions" $
       let e = CGEnv (Env.fromStringPairs [("variable","value")]) []
           src = vmImagesArtifact "" [] (buildScript "${variable}")
           expected = buildScript "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected

transientCOW :: FilePath -> FilePath -> ImageTarget
transientCOW fileName mountPoint =
  ImageTarget Transient
              (CopyOnWrite (Image fileName QCow2 Ext4))
              (MountPoint mountPoint)

transientShared :: FilePath -> FilePath -> ImageTarget
transientShared name mountPoint =
  ImageTarget Transient
              (From name KeepSize)
              (MountPoint mountPoint)

shareCOW :: FilePath -> FilePath -> ImageTarget
shareCOW destName mountPoint =
  ImageTarget (Share destName QCow2 KeepSize)
              (CopyOnWrite (Image "cowSource" QCow2 Ext4))
              (MountPoint mountPoint)

liveInstallerCOWImage :: FilePath -> FilePath -> ImageTarget
liveInstallerCOWImage destName mountPoint =
  ImageTarget (LiveInstallerImage destName destName KeepSize)
              (CopyOnWrite (Image "cowSource" QCow2 Ext4))
              (MountPoint mountPoint)

localCOWImage :: FilePath -> FilePath -> ImageTarget
localCOWImage destName mountPoint =
  ImageTarget (LocalFile (Image destName QCow2 Ext4) KeepSize)
              (CopyOnWrite (Image "cowSource" QCow2 Ext4))
              (MountPoint mountPoint)

vmImagesArtifact :: String -> [ImageTarget] -> VmScript -> ArtifactGenerator
vmImagesArtifact iid imgs script =
  Artifact (IID iid) (VmImages imgs script)

emptyScriptWithSharedDirRO :: String -> VmScript
emptyScriptWithSharedDirRO arg =
  VmScript X86_64 [SharedDirectoryRO arg (MountPoint arg) ] (Run "" [])

emptyScriptWithSharedDirRW :: String -> VmScript
emptyScriptWithSharedDirRW arg =
  VmScript X86_64 [SharedDirectory arg (MountPoint arg) ] (Run "" [])

buildScript :: String -> VmScript
buildScript arg =
  VmScript X86_64 [SharedDirectory arg (MountPoint arg),
                   SharedDirectoryRO arg NotMounted]
                  (As arg [In arg [Run arg [arg]]])
