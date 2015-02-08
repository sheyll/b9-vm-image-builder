module B9.VmSpec (spec) where
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

  describe "substVmScript" $ do

     it "replaces '$...' variables in shared directory source and mount point (RO)" $
       let e = [("variable","value")]
           src = emptyScriptWithSharedDirRO "${variable}"
           expected = emptyScriptWithSharedDirRO "value"
           actual = substVmScript e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in shared directory source and mount point (RW)" $
       let e = [("variable","value")]
           src = emptyScriptWithSharedDirRW "${variable}"
           expected = emptyScriptWithSharedDirRW "value"
           actual = substVmScript e src
       in actual `shouldBe` expected

     it "replaces '$...' variables in VmImages build script instructions" $
       let e = [("variable","value")]
           src = buildScript "${variable}"
           expected = buildScript "value"
           actual = substVmScript e src
       in actual `shouldBe` expected


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
