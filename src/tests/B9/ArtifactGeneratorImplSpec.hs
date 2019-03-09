module B9.ArtifactGeneratorImplSpec
  ( spec
  )
where

import           B9.Artifact.Readable
import           B9.Artifact.Readable.Interpreter
import           B9.DiskImages
import           B9.ExecEnv
import           B9.ShellScript
import           B9.Vm
import           Data.Text                      ( )
import           Test.Hspec

spec :: Spec
spec = describe "assemble" $ do
  it "replaces '$...' variables in SourceImage Image file paths"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [transientCOW "${variable}" ""] NoVmScript]
        expected = transientCOW "value" ""
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in SourceImage 'From' names"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [transientShared "${variable}" ""] NoVmScript]
        expected = transientShared "value" ""
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in the name of a shared image"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [shareCOW "${variable}" ""] NoVmScript]
        expected = shareCOW "value" ""
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in the name and path of a live installer image"
    $ let
        src = Let
          [("variable", "value")]
          [ vmImagesArtifact ""
                             [liveInstallerCOWImage "${variable}" ""]
                             NoVmScript
          ]
        expected = liveInstallerCOWImage "value" ""
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it
      "replaces '$...' variables in the file name of an image exported as LocalFile"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [localCOWImage "${variable}" ""] NoVmScript]
        expected = localCOWImage "value" ""
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in mount point of an image"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [localCOWImage "" "${variable}"] NoVmScript]
        expected = localCOWImage "" "value"
        (Right [IG _ _ (VmImages [actual] _)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in shared directory source and mount point (RO)"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [] (emptyScriptWithSharedDirRO "${variable}")]
        expected = emptyScriptWithSharedDirRO "value"
        (Right [IG _ _ (VmImages [] actual)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in shared directory source and mount point (RW)"
    $ let
        src = Let
          [("variable", "value")]
          [vmImagesArtifact "" [] (emptyScriptWithSharedDirRW "${variable}")]
        expected = emptyScriptWithSharedDirRW "value"
        (Right [IG _ _ (VmImages [] actual)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected
  it "replaces '$...' variables in VmImages build script instructions"
    $ let
        src = Let [("variable", "value")]
                  [vmImagesArtifact "" [] (buildScript "${variable}")]
        expected = buildScript "value"
        (Right [IG _ _ (VmImages [] actual)]) =
          runArtifactGenerator mempty "" "" src
      in
        actual `shouldBe` expected

transientCOW :: FilePath -> FilePath -> ImageTarget
transientCOW fileName mountPoint = ImageTarget
  Transient
  (CopyOnWrite (Image fileName QCow2 Ext4))
  (MountPoint mountPoint)

transientShared :: FilePath -> FilePath -> ImageTarget
transientShared name mountPoint =
  ImageTarget Transient (From name KeepSize) (MountPoint mountPoint)

shareCOW :: FilePath -> FilePath -> ImageTarget
shareCOW destName mountPoint = ImageTarget
  (Share destName QCow2 KeepSize)
  (CopyOnWrite (Image "cowSource" QCow2 Ext4))
  (MountPoint mountPoint)

liveInstallerCOWImage :: FilePath -> FilePath -> ImageTarget
liveInstallerCOWImage destName mountPoint = ImageTarget
  (LiveInstallerImage destName destName KeepSize)
  (CopyOnWrite (Image "cowSource" QCow2 Ext4))
  (MountPoint mountPoint)

localCOWImage :: FilePath -> FilePath -> ImageTarget
localCOWImage destName mountPoint = ImageTarget
  (LocalFile (Image destName QCow2 Ext4) KeepSize)
  (CopyOnWrite (Image "cowSource" QCow2 Ext4))
  (MountPoint mountPoint)

vmImagesArtifact :: String -> [ImageTarget] -> VmScript -> ArtifactGenerator
vmImagesArtifact iid imgs script = Artifact (IID iid) (VmImages imgs script)

emptyScriptWithSharedDirRO :: String -> VmScript
emptyScriptWithSharedDirRO arg =
  VmScript X86_64 [SharedDirectoryRO arg (MountPoint arg)] (Run "" [])

emptyScriptWithSharedDirRW :: String -> VmScript
emptyScriptWithSharedDirRW arg =
  VmScript X86_64 [SharedDirectory arg (MountPoint arg)] (Run "" [])

buildScript :: String -> VmScript
buildScript arg = VmScript
  X86_64
  [SharedDirectory arg (MountPoint arg), SharedDirectoryRO arg NotMounted]
  (As arg [In arg [Run arg [arg]]])
