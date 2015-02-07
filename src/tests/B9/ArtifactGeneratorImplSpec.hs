module B9.ArtifactGeneratorImplSpec (spec) where
import Test.Hspec
import Data.Text ()

import B9.ArtifactGenerator
import B9.ArtifactGeneratorImpl
import B9.DiskImages
import B9.ExecEnv
import B9.Vm
import B9.ShellScript

spec :: Spec
spec =
  describe "assemble" $ do

     it "replaces '$...' variables in SourceImage Image file paths" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [transientCOWImage "${variable}" ""] NoVmScript
           expected = transientCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in SourceImage 'From' names" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [transientSharedImage "${variable}" ""] NoVmScript
           expected = transientSharedImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name of a shared image" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [shareCOWImage "${variable}" ""] NoVmScript
           expected = shareCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name and path of a live installer image" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [liveInstallerCOWImage "${variable}" ""] NoVmScript
           expected = liveInstallerCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in the file name of an image exported as LocalFile" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [localCOWImage "${variable}" ""] NoVmScript
           expected = localCOWImage "value" ""
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in mount point of an image" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [localCOWImage "" "${variable}"] NoVmScript
           expected = localCOWImage "" "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [actual] _))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in shared directory source and mount point (RO)" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [] (emptyScriptWithSharedDirRO "${variable}")
           expected = emptyScriptWithSharedDirRO "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in shared directory source and mount point (RW)" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [] (emptyScriptWithSharedDirRW "${variable}")
           expected = emptyScriptWithSharedDirRW "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected

     it "replaces '$...' variables in VmImages build script instructions" $
       let e = Environment [("variable","value")] []
           src = vmImagesArtifact [] (buildScript "${variable}")
           expected = buildScript "value"
           (Right [igEnv]) = execCGParser (parseArtifactGenerator src) e
           (Right (IG _ (VmImages [] actual))) = execIGEnv igEnv
       in actual `shouldBe` expected



transientCOWImage :: FilePath -> FilePath -> ImageTarget
transientCOWImage fileName mountPoint =
  ImageTarget Transient
              (CopyOnWrite (Image fileName QCow2 Ext4))
              (MountPoint mountPoint)

transientSharedImage :: FilePath -> FilePath -> ImageTarget
transientSharedImage name mountPoint =
  ImageTarget Transient
              (From name KeepSize)
              (MountPoint mountPoint)

shareCOWImage :: FilePath -> FilePath -> ImageTarget
shareCOWImage destName mountPoint =
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

vmImagesArtifact :: [ImageTarget] -> VmScript -> ArtifactGenerator
vmImagesArtifact imgs script =
  Artifact (VmImages imgs script)

emptyScriptWithSharedDirRO :: String -> VmScript
emptyScriptWithSharedDirRO arg =
  VmScript arg X86_64 [SharedDirectoryRO arg (MountPoint arg) ] (Run "" [])

emptyScriptWithSharedDirRW :: String -> VmScript
emptyScriptWithSharedDirRW arg =
  VmScript arg X86_64 [SharedDirectory arg (MountPoint arg) ] (Run "" [])

buildScript :: String -> VmScript
buildScript arg =
  VmScript arg X86_64 [SharedDirectory arg (MountPoint arg),
                       SharedDirectoryRO arg NotMounted]
                      (As arg [In arg [Run arg [arg]]])
