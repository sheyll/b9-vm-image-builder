module B9.DSL.InterpreterSpec (spec) where
import B9 hiding (CloudInit)
import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter
import B9.ShellScript
import Control.Lens hiding (from)
import Data.Function
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "compile (General)" $
        do it "traces documentation" $
               dumpToStrings (compile (doc "test")) `shouldBe`
               ["logTrace test"]
    candySpecs
    localDirExamples
    cloudInitIsoImageExamples
    cloudInitMultiVfatImageExamples
    cloudInitDirExamples
    cloudInitWithContentExamples

-- * Specs for /candy/ functions.
candySpecs :: Spec
candySpecs = do
    describe "addFile" $
        it "strips the directory and  does not replace templates" $
        do let actual = do
                   d <- newDirectory
                   addFile d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileContent
                       d
                       (fileSpec "test.txt")
                       (FromTextFile
                            (Source NoConversion "/some/path/test.txt"))
           actual `hasSameEffectAs` expected
    describe "addExe" $
        it "is equal to addFile, but changes permissions to 0755" $
        do let actual = do
                   d <- newDirectory
                   addExe d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileContent
                       d
                       (fileSpec "test.txt" & fileSpecPermissions .~
                        (0, 7, 5, 5))
                       (FromTextFile
                            (Source NoConversion "/some/path/test.txt"))
           actual `hasSameEffectAs` expected
    describe "addFileP" $
        it "is equal to addFile, but changes permissions to the given value" $
        property $
        \perm ->
             do let actual = do
                        d <- newDirectory
                        addFileP d "/some/path/test.txt" perm
                    expected = do
                        d <- newDirectory
                        addFileContent
                            d
                            (fileSpec "test.txt" & fileSpecPermissions .~
                             perm)
                            (FromTextFile
                                 (Source NoConversion "/some/path/test.txt"))
                actual `hasSameEffect` expected
    describe "addTemplate" $
        it "strips the directory and replaces template variables" $
        do let actual = do
                   d <- newDirectory
                   addTemplate d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileContent
                       d
                       (fileSpec "test.txt")
                       (FromTextFile
                            (Source ExpandVariables "/some/path/test.txt"))
           actual `hasSameEffectAs` expected
    describe "addTemplateExe" $
        it "is equal to addTemplate, but changes permissions to 0755" $
        do let actual = do
                   d <- newDirectory
                   addTemplateExe d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileContent
                       d
                       (fileSpec "test.txt" & fileSpecPermissions .~
                        (0, 7, 5, 5))
                       (FromTextFile
                            (Source ExpandVariables "/some/path/test.txt"))
           actual `hasSameEffectAs` expected
    describe "addTemplateP" $
        it "is equal to addTemplate, but changes permissions to the given value" $
        property $
        \perm ->
             do let actual = do
                        d <- newDirectory
                        addTemplateP d "/some/path/test.txt" perm
                    expected = do
                        d <- newDirectory
                        addFileContent
                            d
                            (fileSpec "test.txt" & fileSpecPermissions .~
                             perm)
                            (FromTextFile
                                 (Source ExpandVariables "/some/path/test.txt"))
                actual `hasSameEffect` expected
  where
    hasSameEffectAs :: Program a -> Program a -> IO ()
    hasSameEffectAs = shouldContain `on` (dumpToStrings . compile)
    hasSameEffect
        :: Eq a
        => Program a -> Program a -> Bool
    hasSameEffect = isInfixOf `on` (dumpToStrings . compile)

-- * 'SLocalDirectory' examples

localDirExamples :: Spec
localDirExamples =
    describe "compile exportDir" $
    do it "create temporary intermediate directory" $
         let expectedCmds = dumpToStrings $ mkTempDir "local-dir"
         in actualCmds `shouldContain` expectedCmds
       it "copies the temporary intermediate directory to all exports" $
         let exportsCmds = dumpToStrings $ do
               src' <- getRealPath "/BUILD/local-dir-XXXX"
               dest' <- ensureParentDir "/tmp/test.d"
               copyDir src' dest'
         in actualCmds `shouldContain` exportsCmds
  where
    actualCmds = dumpToStrings $ compile $ do
        d <- newDirectory
        exportDir d "/tmp/test.d"

-- * Cloud init examples

minimalMetaData :: String -> Content
minimalMetaData iid =
    Concat
        [ FromString "#cloud-config\n"
        , RenderYaml (ASTObj [("instance-id", ASTString iid)])]

minimalUserData :: Content
minimalUserData = Concat [FromString "#cloud-config\n", RenderYaml (ASTObj [])]

cloudInitIsoImageExamples :: Spec
cloudInitIsoImageExamples =
    describe "compile cloudInitIsoImage" $
    do it "appends the build id to the instance id" $
           dumpToStrings (compile cloudInitIsoImage) `shouldContain`
           ["getBuildId"]
       it "creates unique cloud-init handles" $
           dumpToResult
               (compile $
                do hnd1 <- cloudInitIsoImage
                   hnd2 <- cloudInitIsoImage
                   return (hnd1 == hnd2)) `shouldBe`
           False
       it "generates meta-data into the file system image temp dir" $
           let (Handle _ iid,actualCmds) =
                   runPureDump (compile cloudInitIsoImage)
               expectedCmds = dumpToStrings expectedProg
               expectedProg = do
                   tmpDir <- mkTempDir "file-system-content"
                   let metaDataFile = tmpDir </> "meta-data"
                       expectedContent = minimalMetaData iid
                       expectedEnv = Environment []
                   absMetaDataFile <- ensureParentDir metaDataFile
                   renderContentToFile
                       absMetaDataFile
                       expectedContent
                       expectedEnv
           in actualCmds `shouldContain` expectedCmds
       it "generates user-data into the file system image temp dir" $
           let actualCmds = dumpToStrings (compile cloudInitIsoImage)
               expectedCmds = dumpToStrings expectedProg
               expectedProg = do
                   let expectedContent = minimalUserData
                       expectedEnv = Environment []
                       absUserDataFile =
                           "/abs/path//BUILD/file-system-content-XXXX/user-data"
                   renderContentToFile
                       absUserDataFile
                       expectedContent
                       expectedEnv
           in actualCmds `shouldContain` expectedCmds
       it "generates an ISO image" $
           let actualCmds = dumpToStrings (compile cloudInitIsoImage)
               expectedCmds = dumpToStrings expectedProg
               expectedProg = do
                   let files =
                           [ (tmpDir </> "meta-data", fileSpec "meta-data")
                           , (tmpDir </> "user-data", fileSpec "user-data")]
                       fsc = FileSystemCreation ISO9660 "cidata" 2 MB
                       tmpDir = "/abs/path//BUILD/file-system-content-XXXX"
                       tmpImg = "/BUILD/file-system-image-XXXX"
                       dstImg = "test.iso"
                   createFileSystem tmpImg fsc files
                   dstImg' <- ensureParentDir dstImg
                   copy tmpImg dstImg'
           in actualCmds `shouldContain` expectedCmds
  where
    cloudInitIsoImage :: Program (Handle 'CloudInit)
    cloudInitIsoImage = do
        i <- newCloudInit "test-instance-id"
        writeCloudInit i ISO9660 "test.iso"
        return i

cloudInitMultiVfatImageExamples :: Spec
cloudInitMultiVfatImageExamples =
    describe "compile cloudInitVfatImage" $
    do it "generates test1.vfat" $
           let actualCmds = dumpToStrings (compile cloudInitVfatImage)
               expectedCmds = dumpToStrings (expectedProg "test1.vfat")
           in actualCmds `shouldContain` expectedCmds
       it "generates test2.vfat" $
           let actualCmds = dumpToStrings (compile cloudInitVfatImage)
               expectedCmds = dumpToStrings (expectedProg "test2.vfat")
           in actualCmds `shouldContain` expectedCmds
  where
    expectedProg dstImg = do
        let files =
                [ (tmpDir </> "meta-data", fileSpec "meta-data")
                , (tmpDir </> "user-data", fileSpec "user-data")]
            fsc = FileSystemCreation VFAT "cidata" 2 MB
            tmpDir = "/abs/path//BUILD/file-system-content-XXXX"
            tmpImg = "/BUILD/file-system-image-XXXX"
        createFileSystem tmpImg fsc files
        dstImg' <- ensureParentDir dstImg
        copy tmpImg dstImg'
    cloudInitVfatImage :: Program (Handle 'CloudInit)
    cloudInitVfatImage = do
        i <- newCloudInit "test-instance-id"
        writeCloudInit i VFAT "test1.vfat"
        writeCloudInit i VFAT "test2.vfat"
        return i

cloudInitDirExamples :: Spec
cloudInitDirExamples =
    describe "compile cloudInitDir" $
    do let (Handle _ iid,actualCmds) = runPureDump $ compile cloudInitDir
       it "generates a temporary directory" $
           do let createTempDirCmds = dumpToStrings $ do mkTempDir "local-dir"
              actualCmds `shouldContain` createTempDirCmds
       it "renders user-data and meta-data into the temporary directory" $
           do let renderMetaData =
                      dumpToStrings $
                      do void $ B9.B9IO.getBuildId
                         t <- mkTempDir "local-dir"
                         let m = t </> "meta-data"
                         m' <- ensureParentDir m
                         renderContentToFile
                             m'
                             (minimalMetaData iid)
                             (Environment [])
                         let u = t </> "user-data"
                         u' <- ensureParentDir u
                         renderContentToFile
                             u'
                             minimalUserData
                             (Environment [])
              actualCmds `shouldContain` renderMetaData
       it "copies the temporary directory to the destination directories" $
           do let copyToOutputDir =
                      dumpToStrings $
                      do destDir <- ensureParentDir "test.d"
                         copyDir "/abs/path//BUILD/local-dir-XXXX" destDir
              actualCmds `shouldContain` copyToOutputDir
  where
    cloudInitDir :: Program (Handle 'CloudInit)
    cloudInitDir = do
        i <- newCloudInit "test-instance-id"
        writeCloudInitDir i "test.d"
        return i

cloudInitWithContentExamples :: Spec
cloudInitWithContentExamples =
    describe "compile cloudInitWithContent" $
    do it "correctly merges meta-data" $
           cmds `shouldContain`
           (dumpToStrings (renderContentToFile mdPath mdContent templateVars))
       it "correctly merges user-data" $
           cmds `shouldContain`
           (dumpToStrings (renderContentToFile udPath udContent templateVars))
  where
    mdPath = "/abs/path//BUILD/file-system-content-XXXX/meta-data"
    udPath = "/abs/path//BUILD/file-system-content-XXXX/user-data"
    templateVars = Environment [("x","3")]
    mdContent =
        Concat
            [ FromString "#cloud-config\n"
            , RenderYaml
                  (ASTMerge
                       [ ASTObj
                             [ ( "instance-id"
                               , ASTString iid)]
                       , ASTObj [("bootcmd", ASTArr [ASTString "ifdown eth0"])]
                       , ASTObj [("bootcmd", ASTArr [ASTString "ifup eth0"])]])]
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
    (Handle _ iid, cmds) = runPureDump $ compile cloudInitWithContent
    cloudInitWithContent = do
        "x" $= "3"
        i <- newCloudInit "test-instance-id"
        writeCloudInit i ISO9660 "test.iso"
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifdown eth0"])])
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifup eth0"])])
        addFileContent i (FileSpec "file1.txt" (0,6,4,2) "user1" "group1") (FromString "file1")
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
    c <- newCloudInit "blah-ci-${x}"
    writeCloudInit c ISO9660 "test.iso"
    writeCloudInitDir c "test"
    writeCloudInit c VFAT "test.vfat"
    addMetaData c (ASTString "test")
    addUserData c (ASTString "test")
    e <- lxc "container-id"
    mountDirRW e "tmp" "/mnt/HOST_TMP"
    addFileContent
        e
        (fileSpec "/etc/httpd.conf")
        (FromTextFile (Source ExpandVariables "httpd.conf.in"))
    sh e "ls -la"
    addFileContent
        c
        (fileSpec "/etc/httpd.conf")
        (FromTextFile (Source ExpandVariables "httpd.conf.in"))
    sh c "ls -la"
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
