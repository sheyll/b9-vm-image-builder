module B9.DiskImageBuilderSpec (spec) where
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

  describe "substImageTarget" $ do

     it "replaces '$...' variables in SourceImage Image file paths" $
       let e = [("variable","value")]
           src = transientCOWImage "${variable}" ""
           expected = transientCOWImage "value" ""
           actual = substImageTarget e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in SourceImage 'From' names" $
       let e = [("variable","value")]
           src = transientSharedImage "${variable}" ""
           expected = transientSharedImage "value" ""
           actial = substImageTarget e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name of a shared image" $
       let e = [("variable","value")]
           src = shareCOWImage "${variable}" ""
           expected = shareCOWImage "value" ""
           actual = substImageTarget e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in the name and path of a live installer image" $
       let e = [("variable","value")]
           src = liveInstallerCOWImage "${variable}" ""
           expected = liveInstallerCOWImage "value" ""
           actual = substImageTarget e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in the file name of an image exported as LocalFile" $
       let e = [("variable","value")]
           src = localCOWImage "${variable}" ""
           expected = localCOWImage "value" ""
           actual = substImageTarget e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in mount point of an image" $
       let e = [("variable","value")]
           src = localCOWImage "" "${variable}"
           expected = localCOWImage "" "value"
           actual = substImageTarget e src
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
