module B9.DSL.InterpreterSpec (spec) where
import B9 hiding (CloudInit)
import B9.B9IO
import B9.DSL
import B9.DSL.Interpreter
import B9.FileSystems
import B9.SpecExtra
import Control.Lens hiding (from)
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "compile (General)" $
        do it "traces documentation" $
               (doc "test") `shouldDoIo` (logTrace "test")
    readOnlyFileSpec
    fsImgSpec
    candySpec
    localDirSpec
    cloudInitIsoImageSpec
    cloudInitMultiVfatImageSpec
    cloudInitDirSpec
    cloudInitWithContentSpec
    vmImageCreationSpec
    partitionedDiskSpec
    sharedImageSpec
    updateServerImageSpec
    containerExecutionSpec

-- * Examples for 'ReadOnlyFile' artifacts

readOnlyFileSpec :: Spec
readOnlyFileSpec =
    describe "ReadOnlyFile" $
    do it "has no effects if unused" $
           do let actual = do
                      create SReadOnlyFile "/tmp/test.file"
                  expected = return ()
              actual `shouldDoIo` expected
       it "is copied to a file when exporting" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      export fH $ Just "/tmp/test.file.copy"
                  expected = do
                      src' <- getRealPath "/tmp/test.file"
                      dst' <- ensureParentDir "/tmp/test.file.copy"
                      copy src' dst'
              actual `shouldDoIo` expected
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
              actual `shouldDoIo` expected
       it "can be added to FileSystemImage" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      fsH <-
                          create
                              SFileSystemImage
                              (FileSystemSpec ISO9660 "cidata" 1 MB)
                      add fsH SReadOnlyFile (fileSpec "test.file", fH)
                  expected = do
                      img <- mkTemp "file-system-image-cidata"
                      tmpDir <- mkTempDir "file-system-content"
                      src' <- getRealPath "/tmp/test.file"
                      dst' <- ensureParentDir (tmpDir </> "test.file")
                      copy src' dst'
                      createFileSystem
                          img
                          (FileSystemSpec ISO9660 "cidata" 1 MB)
                          tmpDir
                          [fileSpec "test.file"]
              actual `shouldDoIo` expected
       it
           "can be added to FileSystemImage, which can be exported and added to another FileSystemImage" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      fsH <-
                          create
                              SFileSystemImage
                              (FileSystemSpec ISO9660 "cidata" 1 MB)
                      add fsH SReadOnlyFile (fileSpec "test.file", fH)
                      fileSysFileH <- export fsH (Nothing, Nothing)
                      fsH2 <-
                          create
                              SFileSystemImage
                              (FileSystemSpec VFAT "blub" 1 MB)
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
                          (FileSystemSpec ISO9660 "cidata" 1 MB)
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
                          (FileSystemSpec VFAT "blub" 1 MB)
                          tmpDir2
                          [fileSpec "test1.iso"]
                      return ()
              actual `shouldDoIo` expected
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
              actual `shouldDoIo` expected
       it "can be exported from GeneratedContent" $
           do let actual = do
                      fcH <- createContent (FromString "test-content")
                      fH <- writeContentTmp fcH
                      export fH (Just "/tmp/rendered-content.file")
                  expected = do
                      src <- mkTemp "generated-content-1"
                      src' <- ensureParentDir src
                      renderContentToFile src' (FromString "test-content") (Environment [])
              actual `shouldDoIo` expected
       it "is exported to a (new) ReadOnlyFile" $
           do let actual = do
                      fH <- create SReadOnlyFile "/tmp/test.file"
                      export fH (Just "/tmp/test.file.copy")
                  expected = do
                      src <- getRealPath "/tmp/test.file"
                      dst <- ensureParentDir "/tmp/test.file.copy"
                      copy src dst
              actual `shouldDoIo` expected

-- * Spec for 'SFileSystemImage's

fsImgSpec :: Spec
fsImgSpec = do
    describe "compile SFileSystemImage" $
        do it "creates an empty Ext4 image" $
               shouldDoIo
                   (do fs <-
                           create
                               SFileSystemImage
                               (FileSystemSpec Ext4 "test-label" 10 MB)
                       void $ export fs (Just "out.raw", Nothing))
                   (do fs <- mkTemp "file-system-image-test-label"
                       c <- mkTempDir "file-system-content"
                       createFileSystem
                           fs
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                           c
                           []
                       fs' <- getRealPath fs
                       dest <- ensureParentDir "out.raw"
                       copy fs' dest)
           it "shrinks an Ext4 image" $
               shouldDoIo
                   (do fs <-
                           create
                               SFileSystemImage
                               (FileSystemSpec Ext4 "test-label" 10 MB)
                       void $ export fs (Just "out.raw", Just ShrinkFileSystem))
                   (do fs <- mkTemp "file-system-image-test-label"
                       c <- mkTempDir "file-system-content"
                       r <- mkTemp "file-system-resize"
                       createFileSystem
                           fs
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                           c
                           []
                       fs' <- getRealPath fs
                       r' <- ensureParentDir r
                       copy fs' r'
                       resizeFileSystem r ShrinkFileSystem Ext4
                       r'' <- getRealPath r
                       dest <- ensureParentDir "out.raw"
                       copy r'' dest)
           it "can be exported to several differently resized images" $
               shouldDoIo
                   (do fs <-
                           create
                               SFileSystemImage
                               (FileSystemSpec Ext4 "test-label" 10 MB)
                       void $ export fs (Just "out1.raw", Just (FileSystemResize 10 MB))
                       void $ export fs (Just "out2.raw", Just ShrinkFileSystem))
                   (do fs <- mkTemp "file-system-image-test-label"
                       c <- mkTempDir "file-system-content"
                       r1 <- mkTemp "file-system-resize"
                       r2 <- mkTemp "file-system-resize"
                       createFileSystem
                           fs
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                           c
                           []
                       fs1' <- getRealPath fs
                       r1' <- ensureParentDir r1
                       copy fs1' r1'
                       resizeFileSystem r1 (FileSystemResize 10 MB) Ext4
                       r1'' <- getRealPath r1
                       dest1 <- ensureParentDir "out1.raw"
                       copy r1'' dest1
                       fs2' <- getRealPath fs
                       r2' <- ensureParentDir r2
                       copy fs2' r2'
                       resizeFileSystem r2 ShrinkFileSystem Ext4
                       r2'' <- getRealPath r2
                       dest2 <- ensureParentDir "out2.raw"
                       copy r2'' dest2)

-- * Spec for /candy/ functions.
candySpec :: Spec
candySpec = do
    describe "addFile" $
        it "strips the directory and  does not replace templates" $
        do let actual = do
                   d <- newDirectory
                   addFile d "/some/path/test.txt"
               expected = do
                   d <- newDirectory
                   addFileFull
                       d
                       (Source NoConversion "/some/path/test.txt")
                       (fileSpec "test.txt")
           actual `shouldDo` expected
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
           actual `shouldDo` expected
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
                actual `does` expected
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
           actual `shouldDo` expected
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
           actual `shouldDo` expected
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
                actual `does` expected
    describe "mountAndShareSharedImage" $
        it "is implemented" $
        shouldDo
            (do env <- lxc "env"
                mountAndShareSharedImage "from" "to" "mp" env)
            (do env <- lxc "env"
                h <- fromShared "from"
                h' <- mount env h "mp"
                h' `sharedAs` "to"
            )

-- * 'SLocalDirectory' examples

localDirSpec :: Spec
localDirSpec =
    describe "compile exportDir" $
    do it "creates a temporary intermediate directory" $
           let expectedCmds = mkTempDir "local-dir"
           in actualCmds `shouldDoIo` expectedCmds
       it "copies the temporary intermediate directory to all exports" $
           let exportsCmds = do
                   src' <- mkTempDir "local-dir"
                   dest' <- ensureParentDir "/tmp/test.d"
                   moveDir src' dest'
           in actualCmds `shouldDoIo` exportsCmds
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

cloudInitIsoImageSpec :: Spec
cloudInitIsoImageSpec =
    describe "compile cloudInitIsoImage" $
    do it "appends the build id to the instance id" $
           cloudInitIsoImage `shouldDoIo` B9.B9IO.getBuildId
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
                   metaDataFile <- mkTemp "meta-data"
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
                   userDataFile <- mkTemp "user-data"
                   absUserDataFile <- ensureParentDir userDataFile
                   renderContentToFile
                       absUserDataFile
                       expectedContent
                       expectedEnv
           in cloudInitIsoImage `shouldDoIo` expectedProg
       it "generates an ISO image" $
           let expectedProg = do
                   let files = [fileSpec "meta-data", fileSpec "user-data"]
                       fsc = FileSystemSpec ISO9660 "cidata" 2 MB
                       tmpDir = "/BUILD/file-system-content-XXXX"
                       tmpImg = "/BUILD/file-system-image-cidata-XXXX"
                       dstImg = "test.iso"
                   createFileSystem tmpImg fsc tmpDir files
                   tmpImg' <- getRealPath tmpImg
                   dstImg' <- ensureParentDir dstImg
                   copy tmpImg' dstImg'
           in cloudInitIsoImage `shouldDoIo` expectedProg
  where
    cloudInitIsoImage :: Program (Handle 'CloudInit)
    cloudInitIsoImage = do
        i <- newCloudInit "test-instance-id"
        writeCloudInit i ISO9660 "test.iso"
        return i

cloudInitMultiVfatImageSpec :: Spec
cloudInitMultiVfatImageSpec =
    describe "compile cloudInitVfatImage" $
    do it "generates test1.vfat" $
           cloudInitVfatImage `shouldDoIo` (expectedProg "test1.vfat")
       it "generates test2.vfat" $
           cloudInitVfatImage `shouldDoIo` (expectedProg "test2.vfat")
  where
    expectedProg dstImg = do
        let files = [fileSpec "meta-data", fileSpec "user-data"]
            fsc = FileSystemSpec VFAT "cidata" 2 MB
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

cloudInitDirSpec :: Spec
cloudInitDirSpec =
    describe "compile cloudInitDir" $
    do let (Handle _ iid,actualCmds) = runPureDump $ compile cloudInitDir
       it "generates a temporary directory" $
           do cloudInitDir `shouldDoIo` (mkTempDir "local-dir")
       it "renders user-data and meta-data into the temporary directory" $
           do let renderMetaData =
                      dumpToStrings $
                      do m <- mkTemp "meta-data"
                         u <- mkTemp "user-data"
                         m' <- ensureParentDir m
                         renderContentToFile
                             m'
                             (minimalMetaData iid)
                             (Environment [])
                         u' <- ensureParentDir u
                         renderContentToFile
                             u'
                             minimalUserData
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

cloudInitWithContentSpec :: Spec
cloudInitWithContentSpec =
    describe "compile cloudInitWithContent" $
    do it "merges meta-data" $
           cmds `should've`
           (dumpToStrings (renderContentToFile mdPath mdContent templateVars))
       it "merges user-data" $
           cmds `should've`
           (dumpToStrings (renderContentToFile udPath udContent templateVars))
  where
    mdPath = "/abs/path//BUILD/meta-data-XXXX"
    udPath = "/abs/path//BUILD/user-data-XXXX"
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
                   resizeVmImage cf' 3 MB QCow2
                   dest' <- ensureParentDir "/tmp/test.qcow2"
                   moveFile cf' dest'
               actual = do
                   -- create a raw Ext4 image
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemSpec Ext4 "" 10 MB)
                   rawFile <- export rawImg (Nothing, Nothing)
                   -- convert to qcow2
                   qcow2Img <- create SVmImage (rawFile, Raw)
                   export
                       qcow2Img
                       ( Just "/tmp/test.qcow2"
                       , Just QCow2
                       , Just (ImageSize 3 MB))
           in actual `shouldDoIo` expected
       it
           "it converts an image from Raw to Vmdk" $
           let expected = do
                   src <- mkTemp "tmp-file"
                   conv <- mkTemp "converted-img-file"
                   src' <- ensureParentDir src
                   conv' <- ensureParentDir conv
                   convertVmImage src' Raw conv' Vmdk
                   dest' <- ensureParentDir "/tmp/test.vmdk"
                   moveFile conv' dest'
                   return ()
               actual = do
                   -- create a raw Ext4 image
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemSpec Ext4 "root" 10 MB)
                   rawFile <- export rawImg (Nothing, Nothing)
                   -- convert to qcow2
                   qcow2Img <- create SVmImage (rawFile, Raw)
                   export
                       qcow2Img
                       ( Just "/tmp/test.vmdk"
                       , Just Vmdk
                       , Nothing)
           in actual `shouldDoIo` expected
       it
           "copies and moves an image if neither conversion nor resize is required" $
           let actual = do
                   rawImg <-
                       create
                           SFileSystemImage
                           (FileSystemSpec Ext4 "root" 10 MB)
                   rawFile <- export rawImg (Nothing, Nothing)
                   qcow2Img <-
                       create SVmImage (rawFile, Raw)
                   export
                       qcow2Img
                       ( Just "/tmp/dest.raw"
                       , Nothing
                       , Nothing)
               expected = do
                   raw <- mkTemp "tmp-file"
                   tmp <- mkTemp "tmp-file"
                   raw' <- getRealPath raw
                   tmp' <- ensureParentDir tmp
                   copy raw' tmp'
                   dst <- ensureParentDir "/tmp/dest.raw"
                   moveFile tmp' dst
           in actual `shouldDoIo` expected

-- * Partition extraction examples

partitionedDiskSpec :: Spec
partitionedDiskSpec =
    describe "compile PartionedVmImage" $
    do it "extracts the selected partition" $
           let actual = do
                   rawPartitionedFile <-
                       create SReadOnlyFile "/tmp/partitioned.raw"
                   partitionedImg <-
                       create SPartitionedVmImage rawPartitionedFile
                   _rawPart2File <-
                       export partitionedImg (Just "/tmp/part2.raw", MBRPartition 2)
                   return ()
               expected = do
                   src <- getRealPath "/tmp/partitioned.raw"
                   dst <- ensureParentDir "/tmp/part2.raw"
                   extractPartition (MBRPartition 2) src dst
           in actual `shouldDoIo` expected

-- * VmImage respository IO

sharedImageSpec :: Spec
sharedImageSpec =
    describe "compile ShareImageRepository" $
    it
        "supports lookup, get and put vm-image operations"
        (shouldDoIo
             (do imgH <- fromShared "source-image"
                 sharedAs imgH "out-shared")
             (do (_,cachedImg) <-
                     imageRepoLookup (SharedImageName "source-image")
                 cachedImg' <- getRealPath cachedImg
                 imageRepoPublish
                     cachedImg'
                     QCow2
                     (SharedImageName "out-shared")))

-- * LiveInstaller image generation

updateServerImageSpec :: Spec
updateServerImageSpec =
    describe "exportForUpdateServer" $
    do let actual = do
               -- TODO extract this to DSL.h:
               srcF <- create SReadOnlyFile srcFile
               srcImg <- create SVmImage (srcF, QCow2)
               outDirH <- create SLocalDirectory ()
               usRoot <- create SUpdateServerRoot outDirH
               add usRoot SSharedVmImage (SharedImageName machine, srcImg)
               void $ export outDirH (Just outDir)
           srcFile = "source.qcow2"
           outDir = "EXPORT"
           machine = "webserver"
       it
           "converts an input image in arbitrary format to a temporary Raw image inside a given directory" $
           shouldDoIo
               actual
               (do tmpDir <- mkTempDir "local-dir"
                   let tmpImg = tmpBase </> "0.raw"
                       tmpSize = tmpBase </> "0.size"
                       tmpVersion = tmpBase </> "VERSION"
                       tmpBase =
                           tmpDir </> "machines" </> machine </> "disks/raw"
                   src <- getRealPath srcFile
                   mkDir tmpBase
                   convertVmImage src QCow2 tmpImg Raw
                   size <- B9.B9IO.readFileSize tmpImg
                   renderContentToFile
                       tmpSize
                       (FromString (show size))
                       (Environment [])
                   bId <- B9.B9IO.getBuildId
                   bT <- B9.B9IO.getBuildDate
                   renderContentToFile
                       tmpVersion
                       (FromString (printf "%s-%s" bId bT))
                       (Environment [])
                   dst' <- ensureParentDir outDir
                   moveDir tmpDir dst'
                   return ())

-- * Containerized Build Specs

containerExecutionSpec :: Spec
containerExecutionSpec =
    describe "lxc environment" $
    do let envSpec =
               ExecEnvSpec "test-env" LibVirtLXC $
               Resources AutomaticRamSize 2 X86_64
           testFileSpec1 =
               (FileSpec "/root/sub1/sub1.1/passwd" (0, 7, 6, 7) "root" "users")
           testFileSpec2 =
               (FileSpec "/build/issue" (0, 7, 7, 7) "root" "users")
           testProg = do
               e <- boot envSpec
               sh e "touch /test1"
               sh e "touch /test2"
               addFileFull e (Source NoConversion "/etc/issue") testFileSpec2
               addFileFull e (Source NoConversion "/etc/passwd") testFileSpec1
               rootImgFile <- create SReadOnlyFile "test-in.qcow2"
               rootImg <- create SVmImage (rootImgFile, QCow2)
               rootOutImg <- mount e rootImg "/"
               void $ export rootOutImg (Nothing, Nothing, Nothing)
       it "converts the input images into 'Raw' format" $
           testProg `shouldDoIo`
           do conv <- mkTemp "converted-img-file"
              src' <- getRealPath "test-in.qcow2"
              conv' <- ensureParentDir conv
              convertVmImage src' QCow2 conv' Raw
       it "exports the input images as new output images" $
           testProg `shouldDoIo`
           do conv <- mkTemp "converted-img-file"
              dest <- mkTemp "resized-img-file"
              conv' <- getRealPath conv
              dest' <- ensureParentDir dest
              moveFile conv' dest'
       it "copies one added file into a directory to mount" $
           shouldDoIo
               testProg
               (do
                   -- create a tmp dir that will containa all included files
                   -- with unique names. this directory will be bind mounted in
                   -- the container. all added files are exported and the
                   -- resulting files are moved to new temp files in the tmp dir
                   -- from above. the tmp dir artifact is an incoming dependency
                   -- of the execution environment.
                   incDir <- mkTempDir "included-files"
                   copyOfFile1 <- mkTemp "tmp-file"
                   includedFile1 <- mkTempIn incDir "added-file"
                   -- export the ReadOnlyFile "/etc/passwd":
                   realPathFile1 <- getRealPath "/etc/passwd"
                   realPathCopyOfFile1 <- ensureParentDir copyOfFile1
                   copy realPathFile1 realPathCopyOfFile1
                   -- move the exported file into the mounted directory for inclusion
                   realPathIncFile1 <- ensureParentDir includedFile1
                   moveFile realPathCopyOfFile1 realPathIncFile1)
       it "copies both added file into a directory to mount" $
           shouldDoIo
               testProg
               (do incDir <- mkTempDir "included-files"
                   copyOfFile1 <- mkTemp "tmp-file"
                   includedFile1 <- mkTempIn incDir "added-file"
                   copyOfFile2 <- mkTemp "tmp-file"
                   includedFile2 <- mkTempIn incDir "added-file"
                   -- copy file 1
                   realPathFile1 <- getRealPath "/etc/passwd"
                   realPathCopyOfFile1 <- ensureParentDir copyOfFile1
                   copy realPathFile1 realPathCopyOfFile1
                   -- copy file 2
                   realPathFile2 <- getRealPath "/etc/issue"
                   realPathCopyOfFile2 <- ensureParentDir copyOfFile2
                   copy realPathFile2 realPathCopyOfFile2
                   -- move file 1
                   realPathIncFile1 <- ensureParentDir includedFile1
                   moveFile realPathCopyOfFile1 realPathIncFile1
                   -- move file 2
                   realPathIncFile2 <- ensureParentDir includedFile2
                   moveFile realPathCopyOfFile2 realPathIncFile2)
       it "generates a script to copy, chmod and chown the added files" $
           shouldDoIo
               testProg
               (do buildId <- getBuildId
                   incDir <- mkTempDir "included-files"
                   inc1 <- mkTempIn incDir "added-file"
                   inc2 <- mkTempIn incDir "added-file"
                   conv <- mkTemp "converted-img-file"
                   conv' <- ensureParentDir conv
                   inc1' <- ensureParentDir inc1
                   inc2' <- ensureParentDir inc2
                   let addScript =
                           copyIncludedFile
                               (includedFileGuestFilePath buildId inc1)
                               testFileSpec1 <>
                           copyIncludedFile
                               (includedFileGuestFilePath buildId inc1)
                               testFileSpec2
                   executeInEnv envSpec [])
       it "runs all added scripts" $ shouldDoIo testProg (do fail "todo")

includedFileGuestFilePath :: String -> FilePath -> FilePath
includedFileGuestFilePath buildId includedFile =
    "/" ++ buildId </> "included-files" </> takeFileName includedFile

copyIncludedFile :: FilePath -> FileSpec -> Script
copyIncludedFile src (FileSpec destPath (s,u,g,o) userName groupName) =
    Begin
        [ Run "cp" [src, destPath]
        , Run "chmod" [printf "%d%d%d%d" s u g o, destPath]
        , Run "chown" [printf "%s:%s" userName groupName]]

-- * DSL examples

dslExample1 :: Program ()
dslExample1 = do
    "x" $= "3"
    c <- newCloudInit "blah-ci"
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
