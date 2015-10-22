module B9.DSL.InterpreterSpec (spec) where
import Test.Hspec

import B9 hiding (CloudInit)
import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter

spec :: Spec
spec = do
    describe "compile (General)" $
        do it "traces documentation" $
               dumpToStrings (compile (doc "test")) `shouldBe`
               ["logTrace test"]
    describe "compile exportEmptyCloudInitIso" $
        do it "appends the build id to the instance id" $
               dumpToStrings (compile exportEmptyCloudInitIso) `shouldContain`
               ["getBuildId"]
           it "creates unique cloud-init handles" $
               dumpToResult
                   (compile $
                    do hnd1 <- exportEmptyCloudInitIso
                       hnd2 <- exportEmptyCloudInitIso
                       return (hnd1 == hnd2)) `shouldBe`
               False
           it "creates a temporary directory containing the instance id" $
               let (Handle _ iid,cmds) =
                       runPureDump (compile exportEmptyCloudInitIso)
               in cmds `shouldContain` ["mkTemp CloudInit/" ++ iid]
           it "creates a meta-data file" $
               let (Handle _ iid,cmds) =
                       runPureDump (compile exportEmptyCloudInitIso)
               in cmds `shouldContain`
                  [ "mkTemp CloudInit/" ++ iid
                  , "mkDir " ++ "/BUILD/CloudInit" </> iid ++ "-XXXX"
                  , "renderContentToFile " ++
                    "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX/meta-data " ++
                    "Concat [FromString \"#cloud-config\\n\"," ++
                    "RenderYaml (" ++
                    "ASTObj [(\"instance-id\",ASTString \"test-instance-id-build-id-1234-0\")]" ++
                    ")] " ++ "Environment []"]
           it "creates a cloud-init ISO image" $
               let (Handle _ iid,cmds) =
                       runPureDump (compile exportEmptyCloudInitIso)
                   tmpDir = "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX"

               in cmds `shouldContain`
                  [printf "createFileSystemFromDirectory %s TODO" tmpDir ]

-- * cloud-init tests
exportEmptyCloudInitIso :: Program (Handle 'CloudInit)
exportEmptyCloudInitIso = do
    i <- newCloudInit "test-instance-id"
    writeCloudInitIso i "test.iso"
    return i

-- * vmImage tests

vmImageExport :: Program ()
vmImageExport = do
    i1 <- from "test"
    writeImg i1 "test.qcow" QCow2 Ext4 ShrinkToMinimum

vmImageMultiExport :: Program ()
vmImageMultiExport = do
    i1 <- from "test"
    writeImg i1 "test-1" QCow2 Ext4 ShrinkToMinimum
    writeImg i1 "test-2" Vmdk Ext4 KeepSize

vmImageMount :: Program ()
vmImageMount = do
    i1 <- from "test"
    e <- lxc "test"
    mount e i1 "/mnt/test"

vmImageMultiMountAndExport :: Program ()
vmImageMultiMountAndExport = do
    i1 <- from "test"
    e <- lxc "test"
    mount e i1 "/mnt/test-1"
    mount e i1 "/mnt/test-2"
    writeImg i1 "test-1" QCow2 Ext4 ShrinkToMinimum
    writeImg i1 "test-2" Vmdk Ext4 KeepSize

vmImageMultiEnvMultiMount :: Program ()
vmImageMultiEnvMultiMount = do
    i1 <- from "test"
    e1 <- lxc "test-e1"
    e2 <- lxc "test-e2"
    mount e1 i1 "/mnt/test-1"
    mount e2 i1 "/mnt/test-2"

dslExample1 :: Program ()
dslExample1 = do
    "x" $= "3"
    ci <- newCloudInit "blah-ci-${x}"
    writeCloudInitIso ci "test.iso"
    writeCloudInitDir ci "test"
    writeCloudInitVFat ci "test.vfat"
    addMetaData ci (ASTString "test")
    addUserData ci (ASTString "test")
    e <- lxc "container-id"
    mountDirRW e "tmp" "/mnt/HOST_TMP"
    addFile
        e
        (fileSpec "/etc/httpd.conf")
        (FromTextFile (Source ExpandVariables "httpd.conf.in"))
    sh e "ls -la"
    addFile
        ci
        (fileSpec "/etc/httpd.conf")
        (FromTextFile (Source ExpandVariables "httpd.conf.in"))
    sh ci "ls -la"
    doc "From here there be dragons:"
    rootImage "fedora" "testv1-root" e
    dataImage "testv1-data" e
    img <- from "schlupfi"
    mountDir e "/tmp" "/mnt/HOST_TMP"
    share img "wupfi"
    resize img 64 GB
    resizeToMinimum img

dslExample2 :: Program ()
dslExample2 = do
    env <- lxc "c1"
    sh env "ls -lR /"
