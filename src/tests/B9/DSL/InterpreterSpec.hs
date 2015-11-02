module B9.DSL.InterpreterSpec (spec) where
import B9 hiding (CloudInit)
import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter
import B9.SpecExtra
import Control.Lens hiding (from)
import Data.Function
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "compile (General)" $
        do it "traces documentation" $
               (doc "test") `shouldMeanIo` (logTrace "test")
    readOnlyFileExamples
    candySpecs
    localDirExamples
    cloudInitIsoImageExamples
    cloudInitMultiVfatImageExamples
    cloudInitDirExamples
    cloudInitWithContentExamples
    vmImageCreationSpec

-- * Examples for 'ReadOnlyFile' artifacts

readOnlyFileExamples :: Spec
readOnlyFileExamples =
    describe "ReadOnlyFile" $
    do it "has no effects if unused" $
           do let actual = do
                      create SReadOnlyFile "/tmp/test.file"
                  expected = return ()
              actual `shouldMeanIo` expected
       it "is copied to a file when exporting" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      export fH $ Just "/tmp/test.file.copy"
                  expected = do
                      src' <- getRealPath "/tmp/test.file"
                      dst' <- ensureParentDir "/tmp/test.file.copy"
                      copy src' dst'
              actual `shouldMeanIo` expected
       it "can be added to LocalDirectory" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      dirH <- newDirectory
                      add dirH SReadOnlyFile (fileSpec "test.file", fH)
                  expected = do
                      tmpDir <- mkTempDir "local-dir"
                      src' <- getRealPath "/tmp/test.file"
                      dst' <- ensureParentDir (tmpDir </> "test.file")
                      copy src' dst'
              actual `shouldMeanIo` expected
       it "can be added to FileSystemImage" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      fsH <-
                          create
                              SFileSystemImage
                              (FileSystemCreation ISO9660 "cidata" 1 MB)
                      add fsH SReadOnlyFile (fileSpec "test.file", fH)
                  expected = do
                      img <- mkTemp "file-system-image-cidata"
                      tmpDir <- mkTempDir "file-system-content"
                      src' <- getRealPath "/tmp/test.file"
                      dst' <- ensureParentDir (tmpDir </> "test.file")
                      copy src' dst'
                      createFileSystem
                          img
                          (FileSystemCreation ISO9660 "cidata" 1 MB)
                          tmpDir
                          [fileSpec "test.file"]
              actual `shouldMeanIo` expected
       it
           "can be added to FileSystemImage, which can be exported and added to another FileSystemImage" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      fsH <-
                          create
                              SFileSystemImage
                              (FileSystemCreation ISO9660 "cidata" 1 MB)
                      add fsH SReadOnlyFile (fileSpec "test.file", fH)
                      fileSysFileH <- export fsH Nothing
                      fsH2 <-
                          create
                              SFileSystemImage
                              (FileSystemCreation VFAT "blub" 1 MB)
                      add
                          fsH2
                          SReadOnlyFile
                          (fileSpec "test1.iso", fileSysFileH)
                  expected = do
                      -- Allocate all /automatic/ file names:
                      img1 <- mkTemp "file-system-image-cidata"
                      tmpDir1 <- mkTempDir "file-system-content"
                      img1Exp <- mkTemp "tmp-file"
                      img2 <- mkTemp "file-system-image-blub"
                      tmpDir2 <- mkTempDir "file-system-content"
                      -- Copy the input file to the directory from which the ISO
                      -- is created:
                      src1' <- getRealPath "/tmp/test.file"
                      dst1' <- ensureParentDir (tmpDir1 </> "test.file")
                      copy
                          src1'
                          dst1'
                      -- Generate the first image:
                      createFileSystem
                          img1
                          (FileSystemCreation ISO9660 "cidata" 1 MB)
                          tmpDir1
                          [fileSpec "test.file"]
                      -- Copy the image file to a temporary file:
                      img1ExpSrc <- getRealPath img1
                      img1Exp' <- ensureParentDir (tmpDir2 </> img1Exp)
                      copy img1ExpSrc img1Exp'
                      -- Copy the temp image file to the directory from which
                      -- the VFAT is created using the new name (test1.iso):
                      img2Img1Src <- getRealPath img1Exp
                      img2Img1Dst <- ensureParentDir (tmpDir2 </> "test1.iso")
                      copy img2Img1Src img2Img1Dst
                      -- Generate the second image:
                      createFileSystem
                          img2
                          (FileSystemCreation VFAT "blub" 1 MB)
                          tmpDir2
                          [fileSpec "test1.iso"]
                      return ()
              actual `shouldMeanIo` expected
       it "can be added to CloudInit" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      c <- newCloudInit "iid-1"
                      add c SReadOnlyFile (fileSpec "test.file", fH)
                      writeCloudInitDir c "/tmp/ci.d"
                  expected = do
                      src <- mkTempDir "local-dir"
                      dst <- ensureParentDir "/tmp/ci.d"
                      moveDir src dst
              actual `shouldMeanIo` expected
       it "can be exported from GeneratedContent" $
           do let actual = do
                      fcH <- createContent (FromString "test-content")
                      fH <- writeContentTmp fcH
                      export fH (Just "/tmp/rendered-content.file")
                  expected = do
                      src <- mkTemp "generated-content-0"
                      src' <- ensureParentDir src
                      renderContentToFile src' (FromString "test-content") (Environment [])
              actual `shouldMeanIo` expected
       it "is exported to a (new) ReadOnlyFile" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      export fH (Just "/tmp/test.file.copy")
                  expected = do
                      src <- getRealPath "/tmp/test.file"
                      dst <- ensureParentDir "/tmp/test.file.copy"
                      copy src dst
              actual `shouldMeanIo` expected

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
                   addFileFull
                       d
                       (Source NoConversion "/some/path/test.txt") (fileSpec "test.txt")
           actual `hasSameEffectAs` expected
    describe "addExe" $
        it "is equal to addFile, but changes permissions to 0755" $
        do let actual = do
                   d <- newDirectory
                   addExe d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileFull
                       d
                       (Source NoConversion "/some/path/test.txt")
                       (fileSpec "test.txt" & fileSpecPermissions .~
                        (0, 7, 5, 5))
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
                        addFileFull
                            d
                            (Source NoConversion "/some/path/test.txt")
                            (fileSpec "test.txt" & fileSpecPermissions .~ perm)
                actual `hasSameEffect` expected
    describe "addTemplate" $
        it "strips the directory and replaces template variables" $
        do let actual = do
                   d <- newDirectory
                   addTemplate d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileFull
                       d
                       (Source ExpandVariables "/some/path/test.txt")
                       (fileSpec "test.txt")
           actual `hasSameEffectAs` expected
    describe "addTemplateExe" $
        it "is equal to addTemplate, but changes permissions to 0755" $
        do let actual = do
                   d <- newDirectory
                   addTemplateExe d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileFull
                       d
                       (Source ExpandVariables "/some/path/test.txt")
                       (fileSpec "test.txt" & fileSpecPermissions .~
                        (0, 7, 5, 5))
           actual `hasSameEffectAs` expected
    describe "addTemplateP" $
        it
            "is equal to addTemplate, but changes permissions to the given value" $
        property $
        \perm ->
             do let actual = do
                        d <- newDirectory
                        addTemplateP d "/some/path/test.txt" perm
                    expected = do
                        d <- newDirectory
                        addFileFull
                            d
                            (Source ExpandVariables "/some/path/test.txt")
                            (fileSpec "test.txt" & fileSpecPermissions .~ perm)
                actual `hasSameEffect` expected

-- TODO error when converting file systems!
-- * vmImage tests

vmImageCreationSpec :: Spec
vmImageCreationSpec =
    describe "compile VmImage" $
    do it
           "converts an image from Raw to temporary QCow2 image, resizes it and moves it to the output path" $
           let expected = do
                   src <- mkTemp "tmp-file"
                   cf <- mkTemp "converted-img-file"
                   src' <- getRealPath src
                   cf' <- ensureParentDir cf
                   convertVmImage src' Raw cf' QCow2
                   resizeVmImage
                       (Image cf' QCow2 Ext4)
                       (ResizeImage (ImageSize 3 MB))
                   dest' <- ensureParentDir "/tmp/test.qcow2"
                   moveFile cf' dest'
               actual = do
                   -- create a raw Ext4 image
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemCreation Ext4 "" 10 MB)
                   rawFile <- export rawImg Nothing
                   -- convert to qcow2
                   qcow2Img <-
                       create SVmImage (rawFile, VmImageSpec Raw Ext4 KeepSize)
                   export
                       qcow2Img
                       ( Just "/tmp/test.qcow2"
                       , Just $
                         VmImageSpec QCow2 Ext4 (ResizeImage (ImageSize 3 MB)))
           in actual `shouldMeanIo` expected
       it
           "it resizes an image twice, first with the input and then with the output resize parameter" $
           let expected = do
                   src <- mkTemp "tmp-file"
                   r1 <- mkTemp "resized-src-img-file"
                   cf <- mkTemp "converted-img-file"
                   src' <- getRealPath src
                   r1' <- ensureParentDir r1
                   copy src' r1'
                   resizeVmImage (Image r1' Raw Ext4) ShrinkToMinimum
                   cf' <- ensureParentDir cf
                   convertVmImage r1' Raw cf' QCow2
                   resizeVmImage
                       (Image cf' QCow2 Ext4)
                       (ResizeImage (ImageSize 3 MB))
                   dest' <- ensureParentDir "/tmp/test.qcow2"
                   moveFile cf' dest'
               actual = do
                   -- create a raw Ext4 image
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemCreation Ext4 "" 10 MB)
                   rawFile <- export rawImg Nothing
                   -- convert to qcow2
                   qcow2Img <-
                       create
                           SVmImage
                           (rawFile, VmImageSpec Raw Ext4 ShrinkToMinimum)
                   export
                       qcow2Img
                       ( Just "/tmp/test.qcow2"
                       , Just $
                         VmImageSpec QCow2 Ext4 (ResizeImage (ImageSize 3 MB)))
           in actual `shouldMeanIo` expected
       it
           "doesn't resize the image if both input and output size are == 'KeepSize'" $
           let actual = do
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemCreation Ext4 "" 10 MB)
                   rawFile <- export rawImg Nothing
                   qcow2Img <-
                       create SVmImage (rawFile, VmImageSpec Raw Ext4 KeepSize)
                   export
                       qcow2Img
                       ( Just "dest.qcow2"
                       , Just $ VmImageSpec QCow2 Ext4 KeepSize)
               expected = do
                   raw <- mkTemp "tmp-file"
                   converted <- mkTemp "converted-img-file"
                   raw' <- ensureParentDir raw
                   converted' <- ensureParentDir converted
                   convertVmImage raw' Raw converted' QCow2
                   dst' <- ensureParentDir "dest.qcow2"
                   moveFile converted' dst'
           in actual `shouldMeanIo` expected
       it "fails if creation and export spec contain different file systems" $
           let actual = do
                   -- create a raw Ext4 image
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemCreation Ext4 "" 10 MB)
                   rawFile <- export rawImg Nothing
                   -- convert to qcow2
                   qcow2Img <-
                       create SVmImage (rawFile, VmImageSpec Raw Ext4 KeepSize)
                   export
                       qcow2Img
                       ( Just "/tmp/test.qcow2"
                       , Just $ VmImageSpec Raw VFAT KeepSize)
           in (print $ length $ toList actual) `shouldThrow` anyException


-- * 'SLocalDirectory' examples

localDirExamples :: Spec
localDirExamples =
    describe "compile exportDir" $
    do it "creates a temporary intermediate directory" $
           let expectedCmds = mkTempDir "local-dir"
           in actualCmds `shouldMeanIo` expectedCmds
       it "copies the temporary intermediate directory to all exports" $
           let exportsCmds = do
                   src' <- mkTempDir "local-dir"
                   dest' <- ensureParentDir "/tmp/test.d"
                   moveDir src' dest'
           in actualCmds `shouldMeanIo` exportsCmds
  where
    actualCmds = do
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
           cloudInitIsoImage `shouldMeanIo` B9.B9IO.getBuildId
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
                   metaDataFile <- mkTemp "generated-content-0"
                   let expectedContent = minimalMetaData iid
                       expectedEnv = Environment []
                   absMetaDataFile <- ensureParentDir metaDataFile
                   renderContentToFile
                       absMetaDataFile
                       expectedContent
                       expectedEnv
           in actualCmds `should've` expectedCmds
       it "generates user-data into the file system image temp dir" $
           let expectedProg = do
                   let expectedContent = minimalUserData
                       expectedEnv = Environment []
                   userDataFile <- mkTemp "generated-content-2"
                   absUserDataFile <- ensureParentDir userDataFile
                   renderContentToFile
                       absUserDataFile
                       expectedContent
                       expectedEnv
           in cloudInitIsoImage `shouldMeanIo` expectedProg
       it "generates an ISO image" $
           let expectedProg = do
                   let files = [fileSpec "meta-data", fileSpec "user-data"]
                       fsc = FileSystemCreation ISO9660 "cidata" 2 MB
                       tmpDir = "/BUILD/file-system-content-XXXX"
                       tmpImg = "/BUILD/file-system-image-cidata-XXXX"
                       dstImg = "test.iso"
                   createFileSystem tmpImg fsc tmpDir files
                   tmpImg' <- getRealPath tmpImg
                   dstImg' <- ensureParentDir dstImg
                   copy tmpImg' dstImg'
           in cloudInitIsoImage `shouldMeanIo` expectedProg
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
           cloudInitVfatImage `shouldMeanIo` (expectedProg "test1.vfat")
       it "generates test2.vfat" $
           cloudInitVfatImage `shouldMeanIo` (expectedProg "test2.vfat")
  where
    expectedProg dstImg = do
        let files = [fileSpec "meta-data", fileSpec "user-data"]
            fsc = FileSystemCreation VFAT "cidata" 2 MB
            tmpDir = "/BUILD/file-system-content-XXXX"
            tmpImg = "/BUILD/file-system-image-cidata-XXXX"
        createFileSystem tmpImg fsc tmpDir files
        tmpImg' <- getRealPath tmpImg
        dstImg' <- ensureParentDir dstImg
        copy tmpImg' dstImg'
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
           do cloudInitDir `shouldMeanIo` (mkTempDir "local-dir")
       it "renders user-data and meta-data into the temporary directory" $
           do let renderMetaData =
                      dumpToStrings $
                      do m <- mkTemp "generated-content-0"
                         u <- mkTemp "generated-content-2"
                         u' <- ensureParentDir u
                         renderContentToFile
                             u'
                             minimalUserData
                             (Environment [])
                         m' <- ensureParentDir m
                         renderContentToFile
                             m'
                             (minimalMetaData iid)
                             (Environment [])
              actualCmds `should've` renderMetaData
       it "copies the temporary directory to the destination directories" $
           do let copyToOutputDir =
                      dumpToStrings $
                      do destDir <- ensureParentDir "test.d"
                         moveDir "/BUILD/local-dir-XXXX" destDir
              actualCmds `should've` copyToOutputDir
  where
    cloudInitDir :: Program (Handle 'CloudInit)
    cloudInitDir = do
        i <- newCloudInit "test-instance-id"
        writeCloudInitDir i "test.d"
        return i

cloudInitWithContentExamples :: Spec
cloudInitWithContentExamples =
    describe "compile cloudInitWithContent" $
    do it "merges meta-data" $
           cmds `should've`
           (dumpToStrings (renderContentToFile mdPath mdContent templateVars))
       it "merges user-data" $
           cmds `should've`
           (dumpToStrings (renderContentToFile udPath udContent templateVars))
  where
    mdPath = "/abs/path//BUILD/generated-content-0-XXXX"
    udPath = "/abs/path//BUILD/generated-content-2-XXXX"
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
                                          ,("content", ASTEmbed (FromBinaryFile "/BUILD/tmp-file-XXXX"))]])]
                       , ASTObj [("runcmd",ASTArr[ASTString "ls -la /tmp"])]])]
    (Handle _ iid, cmds) = runPureDump $ compile cloudInitWithContent
    cloudInitWithContent = do
        "x" $= "3"
        i <- newCloudInit "test-instance-id"
        writeCloudInit i ISO9660 "test.iso"
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifdown eth0"])])
        addMetaData i (ASTObj [("bootcmd", ASTArr [ASTString "ifup eth0"])])
        addFileFromContent i (FromString "file1") (FileSpec "file1.txt" (0,6,4,2) "user1" "group1")
        sh i "ls -la /tmp"
        return i

-- * DSL examples

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
    addFileFull
        e
        (Source ExpandVariables "httpd.conf.in")
        (fileSpec "/etc/httpd.conf")
    sh e "ls -la"
    addTemplate c "httpd.conf"
    sh c "ls -la"
    doc "From here there be dragons:"
    rootImage "fedora" "testv1-root" e
    dataImage "testv1-data" e
    {-
    img <- from "schlupfi"
    mountDir e "/tmp" "/mnt/HOST_TMP"
    share img "wupfi"
    resize img 64 GB
    resizeToMinimum img
    -}

dslExample2 :: Program ()
dslExample2 = do
    env <- lxc "c1"
    sh env "ls -lR /"

vmImageResizeBoth :: Program ()
vmImageResizeBoth = do
    -- create a raw Ext4 image
    rawImg <- create SFileSystemImage (FileSystemCreation Ext4 "data-fs" 10 MB)
    rawFile <- export rawImg Nothing
    -- convert to qcow2
    qcow2Img <- create SVmImage (rawFile, VmImageSpec Raw Ext4 ShrinkToMinimum)
    qcow2F <-
        export
            qcow2Img
            ( Just "/tmp/test1.qcow2"
            , Just $ VmImageSpec QCow2 Ext4 (ResizeImage (ImageSize 3 MB)))
    rawImg2 <- create SFileSystemImage (FileSystemCreation Ext4 "root-fs" 10 MB)
    rawFile2 <- export rawImg2 Nothing
    -- convert to qcow2
    qcow2Img2 <- create SVmImage (rawFile2, VmImageSpec Raw Ext4 ShrinkToMinimum)
    void $
        export
            qcow2Img2
            ( Just "/tmp/test1.qcow2"
            , Just $ VmImageSpec QCow2 Ext4 (ResizeImage (ImageSize 3 MB)))
    addFile rawImg "/etc/passwd"
    add rawImg2 SReadOnlyFile (fileSpec "img", qcow2F)
