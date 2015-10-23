module B9.DSL.InterpreterSpec (spec) where
import Test.Hspec

import B9 hiding (CloudInit)
import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter
import B9.ShellScript

spec :: Spec
spec = do
    describe "compile (General)" $
        do it "traces documentation" $
               dumpToStrings (compile (doc "test")) `shouldBe`
               ["logTrace test"]
    emptyCloudInitIsoExamples
    cloudInitWithContentExamples
    exportCloudInitExamples

-- * Cloud init examples

emptyCloudInitIsoExamples :: Spec
emptyCloudInitIsoExamples =
    describe "compile exportEmptyCloudInitIso" $
    do it "appends the build id to the instance id" $
           dumpToStrings (compile emptyCloudInitIso) `shouldContain`
           ["getBuildId"]
       it "creates unique cloud-init handles" $
           dumpToResult
               (compile $
                do hnd1 <- emptyCloudInitIso
                   hnd2 <- emptyCloudInitIso
                   return (hnd1 == hnd2)) `shouldBe`
           False
       it "creates a temporary directory containing the instance id" $
           let (Handle _ iid,cmds) =
                   runPureDump (compile emptyCloudInitIso)
           in cmds `shouldContain` ["mkTemp CloudInit/" ++ iid]
       it "creates a meta-data file" $
           let (Handle _ iid,cmds) =
                   runPureDump (compile emptyCloudInitIso)
           in cmds `shouldContain`
              [ "mkTemp CloudInit/" ++ iid
              , "mkDir " ++ "/BUILD/CloudInit" </> iid ++ "-XXXX"
              , "renderContentToFile " ++
                "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX/meta-data " ++
                show mdContent ++ " Environment []"]
       it "creates a cloud-init ISO image" $
           let cmds = dumpToStrings (compile emptyCloudInitIso)
               tmpDir =
                   "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX"
               src =
                   show
                       (ImageFromDir
                            tmpDir
                            "cidata"
                            ISO9660
                            Raw
                            (ImageSize 10 MB))
               dst = show (LocalFile (Image "test.iso" Raw ISO9660) KeepSize)
               canMove = True
           in cmds `shouldContain`
              [printf "convertImageTo %s %s %s" (show canMove) src dst]
  where
    mdContent :: Content
    mdContent =
        Concat
            [ FromString "#cloud-config\n"
            , RenderYaml
                  (ASTObj
                       [ ( "instance-id"
                         , ASTString "test-instance-id-build-id-1234-0")])]


emptyCloudInitIso :: Program (Handle 'CloudInit)
emptyCloudInitIso = do
    i <- newCloudInit "test-instance-id"
    writeCloudInitIso i "test.iso"
    return i


exportCloudInitExamples :: Spec
exportCloudInitExamples =
    describe "compile exportCloudInitToAllFormats" $
    do let cmds = dumpToStrings (compile exportCloudInitToAllFormats)
           tmpDir = "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX"
       it "creates a cloud-init ISO image" $
           let src =
                   show
                       (ImageFromDir
                            tmpDir
                            "cidata"
                            ISO9660
                            Raw
                            (ImageSize 10 MB))
               dst = show (LocalFile (Image "test.iso" Raw ISO9660) KeepSize)
           in cmds `shouldContain`
              [printf "convertImageTo False %s %s" src dst]
       it "creates cloud-init directories" $
           cmds `shouldContain`
           [printf "copyDirectory %s %s" tmpDir "test.d"]
  where
    exportCloudInitToAllFormats = do
        i <- newCloudInit "test-instance-id"
        writeCloudInitDir i "test.d"
        writeCloudInitVFat i "test.vfat"
        writeCloudInitIso i "test.iso"
        writeCloudInitIso i "test-1s.iso"

cloudInitWithContentExamples :: Spec
cloudInitWithContentExamples =
    describe "compile cloudInitWithContent" $
    do let cmds = dumpToStrings (compile cloudInitWithContent)
       do it "correctly merges meta-data" $
              cmds `shouldContain`
              [ printf
                    "renderContentToFile %s %s %s"
                    mdPath
                    (show mdContent)
                    (show templateVars)]
          it "correctly merges user-data" $
              cmds `shouldContain`
              [ printf
                    "renderContentToFile %s %s %s"
                    udPath
                    (show udContent)
                    (show templateVars)]
  where
    mdPath :: FilePath
    mdPath = "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX/meta-data"

    udPath :: FilePath
    udPath = "/BUILD/CloudInit/test-instance-id-build-id-1234-0-XXXX/user-data"

    templateVars :: Environment

    templateVars = Environment [("x","3")]

    mdContent :: Content
    mdContent =
        Concat
            [ FromString "#cloud-config\n"
            , RenderYaml
                  (ASTMerge
                       [ ASTObj
                             [ ( "instance-id"
                               , ASTString "test-instance-id-build-id-1234-0")]
                       , ASTObj [("bootcmd", ASTArr [ASTString "ifdown eth0"])]
                       , ASTObj [("bootcmd", ASTArr [ASTString "ifup eth0"])]])]


    udContent :: Content
    udContent =
        Concat
            [ FromString "#cloud-config\n"
            , RenderYaml
                  (ASTMerge
                       [ ASTObj []
                       , ASTObj [("write_files",
                                  ASTArr
                                  [ASTObj [("path", ASTString "file1.txt")
                                          ,("owner", ASTString "user1:group1")
                                          ,("permissions", ASTString "0642")
                                          ,("content", ASTEmbed (FromString "file1"))]])]
                       , ASTObj [("runcmd",ASTArr[ASTString "ls -la /tmp"])]])]
    cloudInitWithContent :: Program (Handle 'CloudInit)
    cloudInitWithContent = do
        "x" $= "3"
        i <- newCloudInit "test-instance-id"
        writeCloudInitIso i "test.iso"
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifdown eth0"])])
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifup eth0"])])
        addFile i (FileSpec "file1.txt" (0,6,4,2) "user1" "group1") (FromString "file1")
        sh i "ls -la /tmp"
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
