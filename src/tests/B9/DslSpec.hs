{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module B9.DslSpec (spec) where
import B9 hiding (CloudInit)
import B9.B9IO
import B9.Dsl
import B9.SpecExtra
import Test.Hspec
import Test.QuickCheck (property)

-- TODO split

spec :: Spec
spec = do
    someStateSpec
    fileInclusionSpec
    fsImgSpec
    addFileSpec
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
    loggingSpec

-- * Examples for the extensible state
someStateSpec :: Spec
someStateSpec = do
    let hnd11 = Handle SCloudInit "test1"
        hnd12 = Handle SCloudInit "test2"
        hnd21 = Handle SGeneratedContent "test1"
    describe "putArtifactState" $
        do it "stores a value such that it can be read" $
               shouldResultIn
                   (do putArtifactState hnd11 "test"
                       useArtifactState hnd11)
                   (Just "test")
           it
               "does not overwrite values of other handles in the same artifact type" $
               shouldResultIn
                   (do putArtifactState hnd11 "test"
                       putArtifactState hnd12 "XXX"
                       useArtifactState hnd11)
                   (Just "test")
           it
               "does not overwrite values of other handles with the same title and different artifact types" $
               shouldResultIn
                   (do putArtifactState hnd11 "test"
                       putArtifactState hnd21 "XXX"
                       useArtifactState hnd11)
                   (Just "test")
           it
               "can be called multiple times with different keys to store independent value" $
               shouldResultIn
                   (do putArtifactState hnd11 "test1"
                       putArtifactState hnd12 "test2"
                       putArtifactState hnd21 "test3"
                       (,,) <$> useArtifactState hnd11 <*>
                           useArtifactState hnd12 <*>
                           useArtifactState hnd21)
                   (Just "test1", Just "test2", Just "test3")
    describe "modifyArtifactState" $
        do it "adds a new entry" $
               shouldResultIn
                   (do modifyArtifactState hnd11 (const (Just "test"))
                       useArtifactState hnd11)
                   (Just "test")
           it "modify an new entry" $
               shouldResultIn
                   (do modifyArtifactState hnd11 (const (Just "tset"))
                       modifyArtifactState
                           hnd11
                           (fmap (reverse :: String -> String))
                       useArtifactState hnd11)
                   (Just "test")


-- * Examples for 'ReadOnlyFile' artifacts

fileInclusionSpec :: Spec
fileInclusionSpec =
    describe "FreeFile" $
    do it "has no effects if unused" $
           do let actual = externalFile "/tmp/test.file"
                  expected = return ()
              actual `shouldDoIo` expected
       it "is moved if only a single copy exists" $
           do let actual = do
                      fH <- externalFileTempCopy "/tmp/test.file"
                      export fH "/tmp/test.file.copy"
                  expected = do
                      src <- getRealPath "/tmp/test.file"
                      tmp <- mkTemp "test.file-0"
                      dst' <- ensureParentDir "/tmp/test.file.copy"
                      copy src tmp
                      moveFile tmp dst'
              actual `shouldDoIo` expected
       it "is copied n-1 times and moved once for n copies" $
           do let actual = do
                      fH <- externalFileTempCopy "/tmp/test.file"
                      export fH "/tmp/test.file.copy1"
                      export fH "/tmp/test.file.copy2"
                      export fH "/tmp/test.file.copy3"
                      export fH "/tmp/test.file.copy4"
                  expected = do
                      src <- getRealPath "/tmp/test.file"
                      tmp <- mkTemp "test.file-0"
                      dst1 <- ensureParentDir "/tmp/test.file.copy1"
                      dst2 <- ensureParentDir "/tmp/test.file.copy2"
                      dst3 <- ensureParentDir "/tmp/test.file.copy3"
                      dst4 <- ensureParentDir "/tmp/test.file.copy4"
                      copy src tmp
                      copy tmp dst1
                      copy tmp dst2
                      copy tmp dst3
                      moveFile tmp dst4
              actual `shouldDoIo` expected
       it "can be added to LocalDirectory" $
           do let actual = do
                      fH <- externalFileTempCopy "/tmp/test.file"
                      dirH <- newDirectory
                      add dirH SFreeFile (fileSpec "test.file", fH)
                  expected = do
                      ext <- getRealPath "/tmp/test.file"
                      src <- mkTemp "test.file-0"
                      tmpDir <- mkTempDir "local-dir"
                      let dst = tmpDir </> "test.file"
                      copy ext src
                      moveFile src dst
              actual `shouldDoIo` expected
       it "can be added to FileSystemImage" $
           do let actual = do
                      fsH <-
                          create
                              SFileSystemBuilder
                              (FileSystemSpec ISO9660 "cidata" 1 MB)
                      fH <- externalFileTempCopy "/tmp/test.file"
                      add fsH SFreeFile (fileSpec "test.file", fH)
                  expected = do
                      img <- mkTemp "cidata.ISO9660"
                      tmpDir <- mkTempDir "cidata.ISO9660.d"
                      ext <- getRealPath "/tmp/test.file"
                      src <- mkTemp "test.file-3"
                      copy ext src
                      let dst = tmpDir </> "test.file"
                      moveFile src dst
                      createFileSystem
                          img
                          (FileSystemSpec ISO9660 "cidata" 1 MB)
                          tmpDir
                          [fileSpec "test.file"]
              actual `shouldDoIo` expected
       it
           "can be added to FileSystemImage, which can be exported and added to another FileSystemImage" $
           do let actual = do
                      fH <- externalFileTempCopy "/tmp/test.file"
                      fsH <-
                          create
                              SFileSystemBuilder
                              (FileSystemSpec ISO9660 "cidata" 1 MB)
                      add fsH SFreeFile (fileSpec "test.file", fH)
                      fileSysImgH <- extract fsH SFileSystemImage ()
                      fileSysFileH <- extract fileSysImgH SFreeFile ()
                      fsH2 <-
                          create
                              SFileSystemBuilder
                              (FileSystemSpec VFAT "blub" 1 MB)
                      add fsH2 SFreeFile (fileSpec "test1.iso", fileSysFileH)
                  expected = do
                      -- Allocate all /automatic/ file names:
                      ext <- getRealPath "/tmp/test.file"
                      src1 <- mkTemp "test.file-0"
                      img1 <- mkTemp "cidata.ISO9660"
                      tmpDir1 <- mkTempDir "cidata.ISO9660.d"
                      img2 <- mkTemp "blub.VFAT"
                      tmpDir2 <- mkTempDir "blub.VFAT.d"
                      -- Copy the input file to the directory from which the ISO
                      -- is created:
                      copy
                          ext
                          src1
                      let dst1 = tmpDir1 </> "test.file"
                      moveFile src1 dst1
                      -- Generate the first image:
                      createFileSystem
                          img1
                          (FileSystemSpec ISO9660 "cidata" 1 MB)
                          tmpDir1
                          [fileSpec "test.file"]
                      -- Generate the second image:
                      let dst2 = tmpDir2 </> "test1.iso"
                      moveFile img1 dst2
                      createFileSystem
                          img2
                          (FileSystemSpec VFAT "blub" 1 MB)
                          tmpDir2
                          [fileSpec "test1.iso"]
                      return ()
              actual `shouldDoIo` expected
       it "can be added to CloudInit" $
           do let actual = do
                      fH <- externalFileTempCopy "/tmp/test.file"
                      c <- newCloudInit "iid-1"
                      add c SFreeFile (fileSpec "test.file", fH)
                      writeCloudInitDir c "/tmp/ci.d"
                  expected = do
                      src <- mkTempDir "local-dir"
                      dst <- ensureParentDir "/tmp/ci.d"
                      moveDir src dst
              actual `shouldDoIo` expected
       it "can be exported from GeneratedContent" $
           do let actual = do
                      fcH <- createContent (FromString "test-content") "test-c"
                      fH <- extract fcH SFreeFile ()
                      void $ export fH "/tmp/rendered-content.file"
                  expected = do
                      src <- mkTemp "test-c-0"
                      _dst <- ensureParentDir "/tmp/rendered-content.file"
                      renderContentToFile
                          src
                          (FromString "test-content")
                          (Environment [])
              actual `shouldDoIo` expected

-- * Spec for 'SFileSystemImage's

fsImgSpec :: Spec
fsImgSpec =
    describe "compile SFileSystemImage" $
    do it "creates an empty Ext4 image" $
           shouldDoIo
               (do fs <-
                       create
                           SFileSystemBuilder
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                   fsImg <- extract fs SFileSystemImage ()
                   export fsImg "out-img.raw")
               (do fs <- mkTemp "test-label.Ext4"
                   c <- mkTempDir "test-label.Ext4.d"
                   dest <- ensureParentDir "out-img.raw"
                   createFileSystem
                       fs
                       (FileSystemSpec Ext4 "test-label" 10 MB)
                       c
                       []
                   moveFile fs dest)
       it "shrinks an Ext4 image" $
           shouldDoIo
               (do fs <-
                       create
                           SFileSystemBuilder
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                   fsImg <- extract fs SFileSystemImage ()
                   fsImgShrunk <-
                       extract fsImg SFileSystemImage ShrinkFileSystem
                   export fsImgShrunk "out-img.raw")
               (do fs <- mkTemp "test-label.Ext4"
                   c <- mkTempDir "test-label.Ext4.d"
                   r <- mkTemp "test-label.Ext4-1-resized"
                   dest <- ensureParentDir "out-img.raw"
                   createFileSystem
                       fs
                       (FileSystemSpec Ext4 "test-label" 10 MB)
                       c
                       []
                   moveFile fs r
                   resizeFileSystem r ShrinkFileSystem Ext4
                   moveFile r dest)
       it "can be exported to several differently resized images" $
           shouldDoIo
               (do fs <-
                       create
                           SFileSystemBuilder
                           (FileSystemSpec Ext4 "test-label" 10 MB)
                   fsImg <- extract fs SFileSystemImage ()
                   fsImg10MB <-
                       extract fsImg SFileSystemImage (FileSystemResize 10 MB)
                   fsImgShrunk <-
                       extract fsImg SFileSystemImage ShrinkFileSystem
                   export fsImg10MB "out1.raw"
                   export fsImgShrunk "out2.raw")
               (do fs <- mkTemp "test-label.Ext4"
                   c <- mkTempDir "test-label.Ext4.d"
                   r1 <- mkTemp "test-label.Ext4-1-resized"
                   r2 <- mkTemp "test-label.Ext4-1-resized"
                   dest1 <- ensureParentDir "out1.raw"
                   dest2 <- ensureParentDir "out2.raw"
                   createFileSystem
                       fs
                       (FileSystemSpec Ext4 "test-label" 10 MB)
                       c
                       []
                   copy fs r1
                   moveFile fs r2
                   resizeFileSystem r1 (FileSystemResize 10 MB) Ext4
                   resizeFileSystem r2 ShrinkFileSystem Ext4
                   moveFile r1 dest1
                   moveFile r2 dest2)

-- * Spec for /candy/ functions.
addFileSpec :: Spec
addFileSpec = do
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
                h' `sharedAs` "to")

-- * 'SLocalDirectory' examples

localDirSpec :: Spec
localDirSpec =
    describe "compile exportDir" $
    do it "copies the temporary intermediate directory to all exports" $
           shouldDoIo
               (do d <- newDirectory
                   exportDir d "/tmp/test1.d"
                   exportDir d "/tmp/test2.d"
                   exportDir d "/tmp/test3.d"
                   exportDir d "/tmp/test4.d")
               (do src <- mkTempDir "local-dir"
                   dest1 <- ensureParentDir "/tmp/test1.d"
                   dest2 <- ensureParentDir "/tmp/test2.d"
                   dest3 <- ensureParentDir "/tmp/test3.d"
                   dest4 <- ensureParentDir "/tmp/test4.d"
                   copyDir src dest4
                   copyDir src dest3
                   copyDir src dest2
                   moveDir src dest1)
       it "creates the exported copies" $
           shouldDoIo
               (do d <- newDirectory
                   exportDir d "/tmp/test1.d")
               (do src <- mkTempDir "local-dir"
                   dest1 <- ensureParentDir "/tmp/test1.d"
                   moveDir src dest1)

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
       it "generates an iso image with meta- and user-data" $
           let (Handle _ iid,actualCmds) =
                   runPureDump (compile cloudInitIsoImage)
               expectedCmds = dumpToStrings expectedProg
               expectedProg = do
                   tmpIso <- mkTemp "cidata.ISO9660"
                   isoDir <- mkTempDir "cidata.ISO9660.d"
                   isoDst <- ensureParentDir "test.iso"
                   metaDataFile <-
                       mkTemp "iid-123-meta-data-1"
                   userDataFile <-
                       mkTemp "iid-123-user-data-2"
                   renderContentToFile
                       metaDataFile
                       (minimalMetaData iid)
                       (Environment [])
                   moveFile metaDataFile (isoDir </> "meta-data")
                   renderContentToFile
                       userDataFile
                       minimalUserData
                       (Environment [])
                   moveFile userDataFile (isoDir </> "user-data")
                   createFileSystem
                       tmpIso
                       (FileSystemSpec ISO9660 "cidata" 2 MB)
                       isoDir
                       [fileSpec "meta-data", fileSpec "user-data"]
                   moveFile tmpIso isoDst
           in actualCmds `should've` expectedCmds
  where
    cloudInitIsoImage :: Program (Handle 'CloudInit)
    cloudInitIsoImage = do
        i <- newCloudInit "iid-123"
        writeCloudInit i ISO9660 "test.iso"
        return i

cloudInitMultiVfatImageSpec :: Spec
cloudInitMultiVfatImageSpec =
    describe "compile cloudInitVfatImage" $
    do it "generates test1.vfat" $
           cloudInitVfatImage `shouldDoIo` expectedProg "test1.vfat"
       it "generates test2.vfat" $
           cloudInitVfatImage `shouldDoIo` expectedProg "test2.vfat"
  where
    expectedProg dstImg = do
        let files = [fileSpec "meta-data", fileSpec "user-data"]
            fsc = FileSystemSpec VFAT "cidata" 2 MB
            tmpDir = "/BUILD/cidata.VFAT.d-XXXX.d"
            tmpImg = "/BUILD/cidata.VFAT-XXXX"
        dstImg' <- ensureParentDir dstImg
        createFileSystem tmpImg fsc tmpDir files
        moveFile tmpImg dstImg'
    cloudInitVfatImage :: Program (Handle 'CloudInit)
    cloudInitVfatImage = do
        i <- newCloudInit "iid-123"
        writeCloudInit i VFAT "test1.vfat"
        writeCloudInit i VFAT "test2.vfat"
        return i

cloudInitDirSpec :: Spec
cloudInitDirSpec =
    describe "compile cloudInitDir" $
    do let (Handle _ iid,actualCmds) = runPureDump $ compile cloudInitDir
       it "generates a temporary directory" $
           cloudInitDir `shouldDoIo` mkTempDir "local-dir"
       it "renders user-data and meta-data into the temporary directory" $
           do let renderMetaData =
                      dumpToStrings $
                      do m <- mkTemp "iid-123-meta-data-1"
                         u <- mkTemp "iid-123-user-data-2"
                         renderContentToFile
                             m
                             (minimalMetaData iid)
                             (Environment [])
                         renderContentToFile
                             u
                             minimalUserData
                             (Environment [])
              actualCmds `should've` renderMetaData
       it "copies the temporary directory to the destination directories" $
           do let copyToOutputDir =
                      dumpToStrings $
                      do srcDir <- mkTempDir "local-dir"
                         destDir <- ensureParentDir "test.d"
                         moveDir srcDir destDir
              actualCmds `should've` copyToOutputDir
  where
    cloudInitDir :: Program (Handle 'CloudInit)
    cloudInitDir = do
        i <- newCloudInit "iid-123"
        writeCloudInitDir i "test.d"
        return i

cloudInitWithContentSpec :: Spec
cloudInitWithContentSpec =
    describe "compile cloudInitWithContent" $
    do it "merges meta-data" $
           cmds `should've`
           dumpToStrings (renderContentToFile mdPath mdContent templateVars)
       it "merges user-data" $
           cmds `should've`
           dumpToStrings (renderContentToFile udPath udContent templateVars)
  where
    mdPath = "/BUILD/iid-123-meta-data-2-XXXX"
    udPath = "/BUILD/iid-123-user-data-3-XXXX"
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
                                          ,("content", ASTEmbed
                                            (FromBinaryFile
                                             "/BUILD/contents-of-file1.txt-9-10-file1.txt-XXXX"))]])]
                       , ASTObj [("runcmd",ASTArr[ASTString "ls -la /tmp"])]])]
    (Handle _ iid, cmds) = runPureDump $ compile cloudInitWithContent
    cloudInitWithContent = do
        "x" $= "3"
        i <- newCloudInit "iid-123"
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
                   convSrc <-
                       mkTemp
                           "image.Ext4-1-3"
                   convDst <- mkTemp "image.Ext4-1-3-QCow2"
                   resized <-
                       mkTemp
                           "image.Ext4-1-3-QCow2-5-resized-3-MB"
                   dest <- ensureParentDir "/tmp/test.qcow2"
                   convertVmImage convSrc Raw convDst QCow2
                   resizeVmImage resized 3 MB QCow2
                   moveFile resized dest
               actual = do
                   -- create a raw Ext4 image
                   rawFS <-
                       create SFileSystemBuilder (FileSystemSpec Ext4 "" 10 MB)
                   -- extract to qcow2
                   rawFS' <- extract rawFS SFileSystemImage ()
                   rawImg <- extract rawFS' SVmImage ()
                   qCowImg <- extract rawImg SVmImage (Left QCow2)
                   smallerImg <-
                       extract qCowImg SVmImage (Right (ImageSize 3 MB))
                   void $ export smallerImg "/tmp/test.qcow2"
           in actual `shouldDoIo` expected
       it "it converts an image from Raw to Vmdk" $
           let expected = do
                   origFile <- getRealPath "in.raw"
                   srcFile <- mkTemp "in.raw-0"
                   srcImg <- mkTemp "in.raw-0-1"
                   convSrc <-
                       mkTemp
                           "in.raw-0-1-2"
                   convDest <- mkTemp "in.raw-0-1-2-Vmdk"
                   dest <- ensureParentDir "/tmp/test.vmdk"
                   copy origFile srcFile
                   moveFile srcFile srcImg
                   moveFile srcImg convSrc
                   convertVmImage convSrc QCow2 convDest Vmdk
                   moveFile convDest dest
                   return ()
               actual = do
                   -- create a raw Ext4 image
                   rawImg <- fromFile "in.raw" SVmImage QCow2
                   vmdkImg <- extract rawImg SVmImage (Left Vmdk)
                   export vmdkImg "/tmp/test.vmdk"
           in actual `shouldDoIo` expected

-- * Partition extraction examples

partitionedDiskSpec :: Spec
partitionedDiskSpec =
    describe "compile PartionedVmImage" $
    it "extracts the selected partition" $
    let actual = do
            partitionedImg <- fromFile "/tmp/in.raw" SPartitionedVmImage ()
            rawPart2File <- extract partitionedImg SFreeFile (MBRPartition 2)
            export rawPart2File "/tmp/part2.raw"
        expected = do
            src <- getRealPath "/tmp/in.raw"
            raw <- mkTemp "in.raw-0"
            img <- mkTemp "in.raw-0-1-partitioned-vm-image"
            extracted <- mkTemp "in.raw-0-1-partitioned-vm-image-2-partition-2"
            dst <- ensureParentDir "/tmp/part2.raw"
            copy src raw
            moveFile raw img
            extractPartition (MBRPartition 2) img extracted
            moveFile extracted dst
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
                 srcTmp <- mkTemp "xxx.qcow2-0"
                 outTmp <-
                     mkTemp "xxx.qcow2-0-1-out-shared"
                 copy cachedImg' srcTmp
                 moveFile srcTmp outTmp
                 imageRepoPublish outTmp QCow2 (SharedImageName "out-shared")))

-- * LiveInstaller image generation

updateServerImageSpec :: Spec
updateServerImageSpec =
    describe "exportForUpdateServer" $
    do let actual = do
               -- TODO extract this to Dsl.hs:
               srcImg <- fromFile srcFile SVmImage QCow2
               outDirH <- create SLocalDirectory ()
               usRoot <- extract outDirH SUpdateServerRoot ()
               add usRoot SVmImage (SharedImageName machine, srcImg)
               export outDirH outDir
           srcFile = "source.qcow2"
           outDir = "EXPORT"
           machine = "webserver"
       it
           "converts an input image in arbitrary format to a temporary Raw image inside a given directory" $
           shouldDoIo
               actual
               (do src <- getRealPath srcFile
                   srcCopy <- mkTemp "source.qcow2-0"
                   srcImg <-
                       mkTemp
                           "source.qcow2-0-1"
                   tmpDir <- mkTempDir "local-dir"
                   srcImgCopy <-
                       mkTemp
                           "source.qcow2-0-1-2-webserver"
                   dst <- ensureParentDir outDir
                   copy src srcCopy
                   moveFile srcCopy srcImg
                   moveFile srcImg srcImgCopy
                   let tmpImg = tmpBase </> "0.raw"
                       tmpSize = tmpBase </> "0.size"
                       tmpVersion = tmpBase </> "VERSION"
                       tmpBase =
                           tmpDir </> "machines" </> machine </> "disks/raw"
                   mkDir tmpBase
                   convertVmImage srcImgCopy QCow2 tmpImg Raw
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
                   moveDir tmpDir dst
                   return ())

-- * Containerized Build Specs

containerExecutionSpec :: Spec
containerExecutionSpec =
    describe "lxc environment" $
    do let envSpec =
               ExecEnvSpec "test-env" LibVirtLXC $
               Resources AutomaticRamSize 2 X86_64
           passwdSpec =
               FileSpec "/root/sub1/sub1.1/passwd" (0, 7, 6, 7) "root" "users"
           issueSpec = FileSpec "/build/issue" (0, 7, 7, 7) "root" "users"
           testProg = do
               e <- boot envSpec
               mountDir e "/hostRO" "/guestRO"
               mountDirRW e "/hostRW" "/guestRW"
               sh e "touch /test1"
               addFileFull e (Source NoConversion "/etc/issue") issueSpec
               sh e "touch /test2"
               addFileFull e (Source NoConversion "/etc/passwd") passwdSpec
               outputFile e "/etc/httpd/httpd.conf" "out-httpd.conf"
               rootImg <- fromFile "test-in.qcow2" SVmImage QCow2
               rootOutImg <- mount e rootImg "/"
               void $ export rootOutImg "img-out.raw"
       it
           "start a container, with all images and host directories mounted in it, copies input files onto a converted image, then executes custom commands in the container, copies output file from the container to the host and writes the modified image" $
           shouldDoIo testProg $
           do
              -- copy "a" "b"
              buildId <- B9.B9IO.getBuildId
              incDir <- mkTempDir "included-files"
              outDir <- mkTempDir "output-files"
              issueIn <- getRealPath "/etc/issue"
              issue <- mkTemp "issue-1"
              issueInc <- mkTempIn incDir "added-file"
              passwdIn <- getRealPath "/etc/passwd"
              passwd <- mkTemp "passwd-3"
              passwdInc <- mkTempIn incDir "added-file"
              tmpOut <- mkTempIn outDir "test-env-httpd.conf"
              destOut <- ensureParentDir "out-httpd.conf"
              imgIn <- getRealPath "test-in.qcow2"
              img <- mkTemp "test-in.qcow2-6"
              imgCopy <- mkTemp "test-in.qcow2-6-7"
              imgConvSrc <- mkTemp "test-in.qcow2-6-7-8"
              rawImg <- mkTemp "test-in.qcow2-6-7-8-Raw"
              mountedImg <- mkTemp "test-in.qcow2-6-7-8-Raw-10-mounted-at-root"
              mountedImgCopy <-
                  mkTemp "test-in.qcow2-6-7-8-Raw-10-mounted-at-root-12"
              imgOut <- ensureParentDir "img-out.raw"
              copy imgIn img
              moveFile img imgCopy
              moveFile imgCopy imgConvSrc
              convertVmImage imgConvSrc QCow2 rawImg Raw
              moveFile rawImg mountedImg
              copy passwdIn passwd
              moveFile passwd passwdInc
              copy issueIn issue
              moveFile issue issueInc
              let incScript =
                      Run "touch /test1" [] <>
                      incFileScript buildId issueInc issueSpec <>
                      Run "touch /test2" [] <>
                      incFileScript buildId passwdInc passwdSpec <>
                      Run
                          "cp"
                          [ "/etc/httpd/httpd.conf"
                          , outputFileContainerPath buildId </>
                            takeFileName tmpOut]
              executeInEnv
                  envSpec
                  incScript
                  [ SharedDirectoryRO
                        incDir
                        (MountPoint (includedFileContainerPath buildId))
                  , SharedDirectory
                        outDir
                        (MountPoint (outputFileContainerPath buildId))
                  , SharedDirectoryRO "/hostRO" (MountPoint "/guestRO")
                  , SharedDirectory "/hostRW" (MountPoint "/guestRW")]
                  [(Image mountedImg Raw Ext4, MountPoint "/")]
              moveFile tmpOut destOut
              moveFile mountedImg mountedImgCopy
              moveFile mountedImgCopy imgOut

loggingSpec :: Spec
loggingSpec =
    describe "LogEvents" $
    it "are generated from the 'CanLog (Program a)' instance" $
    (do traceL "trace log"
        dbgL "debug" "log"
        infoL "info log"
        errorL "error log"
        return ()) `shouldDoIo`
    (do logMsg LogTrace "trace log"
        logMsg LogDebug "debug log"
        logMsg LogInfo "info log"
        logMsg LogError "error log"
        return ())