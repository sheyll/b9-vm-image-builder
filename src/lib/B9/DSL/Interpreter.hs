-- | Compile a 'Program' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.DSL.Interpreter where -- TODO rename to Compiler

import B9.B9IO
import B9.Content
       (Content(..), Environment(..), AST(..), YamlObject(..), astMerge,
        FileSpec(..), fileSpecPath)
import B9.DSL hiding (use)
import B9.DiskImages
       (Image(..), FileSystem(..), ImageSize(..), ImageType(..), Mounted,
        MountPoint(..), SharedImageName(..), siImgType, printMountPoint,
        PartitionSpec(..))
import B9.ExecEnv
import B9.FileSystems (FileSystemSpec(..))
import B9.ShellScript (Script(..), toBashOneLiner)
import Control.Lens hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Function
import Data.Graph as Graph
import Data.Map as Map hiding (null)
import Data.Tree as Tree
import System.FilePath
import Text.Printf (printf)

-- | The monad used to compile a 'Program' into an 'IoProgram'
type IoCompiler = StateT Ctx IoProgram

-- | This monad contains all information gathered in 'Ctx' but is
-- 'ReaderT'. This is mainly to prevent an action added with 'addAction' to be
-- able to change the state, especially by adding more actions (which would not
-- be executed).
type IoProgBuilder = ReaderT Ctx IoProgram

-- | A wrapper around 'Handle' with existential quantification over the
-- 'Artifact' type.
data SomeHandle where
        SomeHandle :: Handle a -> SomeHandle

instance Show SomeHandle where
    show (SomeHandle h) = "<" ++ show h ++ ">"

instance Eq SomeHandle where
    (==) = (==) `on` show

instance Ord SomeHandle where
    compare = compare `on` show

-- | The internal state of the 'IoCompiler' monad
data Ctx = Ctx
    { _nextVertex :: Vertex
    , _vars :: Map String String
    , _ci :: Map (Handle 'CloudInit) CiCtx
    , _generatedContent :: Map (Handle 'GeneratedContent) Content
    , _localDirs :: Map (Handle 'LocalDirectory) DirCtx
    , _fsBuilder :: Map (Handle 'FileSystemBuilder) FsBuilderCtx
    , _fsImages :: Map (Handle 'FileSystemImage) FsCtx
    , _externalFiles :: Map (Handle 'ExternalFile) FilePath
    , _localFiles :: Map (Handle 'FreeFile) FileCtx
    , _vmImages :: Map (Handle 'VmImage) VmImgCtx
    , _partitionedImgs :: Map (Handle 'PartitionedVmImage) (Handle 'FreeFile)
    , _updateServerRoots :: Map (Handle 'UpdateServerRoot) (Handle 'LocalDirectory)
    , _execEnvs :: Map (Handle 'ExecutionEnvironment) ExecEnvCtx
    , _actions :: Map Vertex [IoProgBuilder ()]
    , _hToV :: Map SomeHandle Vertex
    , _vToH :: Map Vertex SomeHandle
    , _dependencies :: [Edge]
    }

instance Default Ctx where
    def = Ctx 0 def def def def def def def def def def def def def def def []

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaDataH :: Handle 'GeneratedContent
    , _userDataH :: Handle 'GeneratedContent
    } deriving (Show)

instance Default CiCtx where
    def =
        CiCtx
            (singletonHandle SGeneratedContent)
            (singletonHandle SGeneratedContent)

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx -- TODO reduce
    { _dirTempDir :: FilePath
    , _dirExports :: [FilePath]
    } deriving (Show)

-- | Context of a 'SFileSystemBuilder'
data FsBuilderCtx = FsBuilderCtx
    { _fsFiles :: [FileSpec]
    , _fsTempDir :: FilePath
    , _fsImgH :: Handle 'FileSystemImage
    } deriving (Show)

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsFileH :: Handle 'FreeFile
    , _fsType :: FileSystem
    } deriving (Show)

-- | Context of a 'SFreeFile'
data FileCtx = FileCtx
    { _fFileName :: FilePath
    , _fCopies :: [FilePath]
    } deriving (Show)

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFile :: Handle 'FreeFile
    , _vmiType :: ImageType
    } deriving (Show)

-- | Context of a 'ExecutionEnvironment'
data ExecEnvCtx = ExecEnvCtx
    { _execImages :: [Mounted Image]
    , _execBindMounts :: [SharedDirectory]
    , _execScript :: Script
    , _execIncFiles :: [(FilePath, FileSpec)]
    , _execIncDir :: FilePath
    , _execEnvSpec :: ExecEnvSpec
    } deriving (Show)

instance Default ExecEnvCtx where
    def = ExecEnvCtx def def def def def def

makeLenses ''Ctx
makeLenses ''CiCtx
makeLenses ''DirCtx
makeLenses ''FsBuilderCtx
makeLenses ''FsCtx
makeLenses ''FileCtx
makeLenses ''VmImgCtx
makeLenses ''ExecEnvCtx

-- | Compile a 'Program' to an 'IoProgram'
compile :: Program a -> IoProgram a
compile p = evalStateT compileSt def
  where
    compileSt = do
        lift $
            do b <- getBuildId
               logTrace $
                   printf
                       "==[B9-PREPARE]=======================================================[%s]"
                       b
        createPredefinedHandles
        result <- interpret p
        runAllActions
        lift $
            do b <- getBuildId
               logTrace $
                   printf
                       "==[B9-FINISHED]======================================================[%s]"
                       b
        return result

-- | Compile a 'Program' but run no actions, instead just print out information
-- about the program using 'logTrace'
inspect :: Show a => Program a -> IoProgram String
inspect p = evalStateT compileSt def
  where
    compileSt = do
        createPredefinedHandles
        res <- interpret $ p
        mG <- dependencyGraph
        case mG of
            Just g -> do
                handles <- use vToH
                return $ printDependencyGraph g handles
            Nothing -> do
                return $ "No artifacts." ++ show res

-- | Setup the predefined global handles, e.g. 'imageRepositoryH'
createPredefinedHandles :: IoCompiler ()
createPredefinedHandles = allocPredefinedHandle imageRepositoryH
  where
    allocPredefinedHandle h = do
        v <- addVertex
        void $ storeHandle h v
        actions . at v ?= []

-- | Run all actions in correct order according to the dependency graph.
runAllActions :: IoCompiler ()
runAllActions = do
    lift $
        do b <- getBuildId
           logTrace $
               printf
                   "==[B9-EXECUTE]=======================================================[%s]"
                   b
    mG <- dependencyGraph
    case mG of
        Just g -> do
            forM_ (topSort g) runActionForVertex
        Nothing -> do
            lift $ logTrace "No artifacts."
            return ()
  where
    runActionForVertex vertex = do
        Just actionsForVertex <- use $ actions . at vertex
        runIoProgBuilder $ sequence_ actionsForVertex

-- | Generate a graph from the artifact dependencies in the compiler context.
dependencyGraph :: IoCompiler (Maybe Graph)
dependencyGraph = do
    maxVertex <- use nextVertex
    if (maxVertex > 0)
        then do
            deps <- use dependencies
            return $ Just $ buildG (0, maxVertex - 1) deps
        else return Nothing

-- | Show the dependency graph from the compiler context.
printDependencyGraph :: Graph -> Map Vertex SomeHandle -> String
printDependencyGraph g handles =
    unlines $
    "digraph artifactDependencyGraph {" :
    (fmap (printEdge handles) (edges g)) ++
    "}" :
    "Dependency forest:" :
    Tree.drawForest (fmap (printVertex handles) <$> dff g) :
    "Build order:" : (printVertex handles <$> topSort g)

-- | Convert an edge to a formatted string
printEdge :: Map Vertex SomeHandle -> Edge -> String
printEdge handles (u,v) = printf "  %s   ->    %s" (show (printVertex handles u)) (show (printVertex handles v))

-- | Convert a vertex to a formatted string
printVertex :: Map Vertex SomeHandle -> Vertex -> String
printVertex handles v =
    printf "%s(%d)" (printSomeHandle $ Map.lookup v handles) v

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle (Handle _s t))) = t
printSomeHandle Nothing = "??error??"

instance Interpreter IoCompiler where
    -- Create
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        (hnd@(Handle _ iid),_) <-
            allocHandle
                SCloudInit
                ("cloudinit-" ++ iidPrefix ++ "-" ++ buildId)
        mH <-
            runCreate
                SGeneratedContent
                ( Concat
                      [ FromString "#cloud-config\n"
                      , RenderYaml $ ASTObj [("instance-id", ASTString iid)]]
                , iidPrefix ++ "-meta-data")
        hnd --> mH
        uH <-
            runCreate
                SGeneratedContent
                ( Concat [FromString "#cloud-config\n", RenderYaml $ ASTObj []]
                , iidPrefix ++ "-user-data")
        hnd --> uH
        ci . at hnd ?= CiCtx mH uH
        return hnd
    runCreate SExecutionEnvironment e = do
        (hnd,_) <- allocHandle SExecutionEnvironment (e ^. execEnvTitle)
        incDir <- lift (mkTempDir "included-files" >>= ensureParentDir)
        buildId <- lift $ B9.B9IO.getBuildId
        execEnvs . at hnd ?=
            (def &~
             do execEnvSpec .= e
                execIncDir .= incDir
                execBindMounts .=
                    [ SharedDirectoryRO
                          incDir
                          (MountPoint $ includedFileContainerPath buildId)])
        addAction hnd $
            do Just es <- view (execEnvs . at hnd)
               lift $
                   executeInEnv
                       (es ^. execEnvSpec)
                       (es ^. execScript)
                       (es ^. execBindMounts)
                       (es ^. execImages)
        return hnd
    runCreate SExternalFile fn = do
        (hnd,_) <- allocHandle SExternalFile $ takeFileName fn
        fn' <- lift $ getRealPath fn
        externalFiles . at hnd ?= fn'
        return hnd
    runCreate SFileSystemBuilder fsSpec@(FileSystemSpec t fsLabel _ _) = do
        let title =
                show t ++ "-" ++
                (if null fsLabel
                     then "image"
                     else fsLabel)
        (hnd,_) <- allocHandle SFileSystemBuilder fsLabel
        tmpFileH <- runCreate SFreeFile $ Just title
        hnd --> tmpFileH
        Just (FileCtx tmpFile _) <- use $ localFiles . at tmpFileH
        fH <- createFsImage tmpFileH t
        tmpDir <- lift (mkTempDir (title <.> "d") >>= ensureParentDir)
        fsBuilder . at hnd ?= FsBuilderCtx [] tmpDir fH
        addAction hnd $
            do Just fileSys <- view $ fsBuilder . at hnd
               lift $
                   createFileSystem tmpFile fsSpec tmpDir (fileSys ^. fsFiles)
        return hnd
    runCreate SFreeFile mTempName = do
        -- TODO escape tempName, allow only a-zA-Z0-9.-_:+=
        let tempName = maybe "tmp-file" takeFileName mTempName
        tempFile <- lift $ mkTemp tempName
        tempFile' <- lift $ ensureParentDir tempFile
        (hnd,_) <- allocHandle SFreeFile tempName
        localFiles . at hnd ?= FileCtx tempFile' []
        addAction hnd $
            do Just (FileCtx _ destinations) <- view (localFiles . at hnd)
               lift $
                   do case reverse destinations of
                          (lastCopy:firstCopies) -> do
                              mapM_ (copy tempFile') (reverse firstCopies)
                              moveFile tempFile' lastCopy
                          [] ->
                              logTrace $
                              printf "No copies of %s required" tempFile'
        return hnd
    runCreate SGeneratedContent (c,title) = do
        (hnd,_) <- allocHandle SGeneratedContent title
        generatedContent . at hnd ?= c
        return hnd
    runCreate SLocalDirectory () = do
        tmp <- lift (mkTempDir "local-dir" >>= ensureParentDir)
        (hnd,_) <- allocHandle SLocalDirectory tmp
        localDirs . at hnd ?= DirCtx tmp []
        addAction hnd $
            do Just (DirCtx src dests) <- view $ localDirs . at hnd
               case reverse dests of
                   [] ->
                       lift $ logTrace $
                       printf "Warning: '%s' not exported." (show hnd)
                   (lastDest:firstDests) ->
                       lift $
                       do mapM_ (copyDir src) (reverse firstDests)
                          moveDir src lastDest
        return hnd
    runCreate sa _src =
        fail $ printf "Create %s: Not Yet Implemented" (show sa)
    -- Update
    runUpdate hnd@(Handle SGeneratedContent _) c = do
        generatedContent . at hnd . traverse %=
            \cOld ->
                 Concat [cOld, c]
        return ()
    runUpdate hnd _src =
        fail $ printf "Update %s: Not Yet Implemented" (show hnd)
    -- Add
    runAdd hnd@(Handle SCloudInit _) SCloudInitMetaData ast = do
        Just (CiCtx mH _) <- use $ ci . at hnd
        generatedContent . at mH . traverse %=
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml $ ast' `astMerge` ast]
    runAdd hnd@(Handle SCloudInit _) SCloudInitUserData ast = do
        Just (CiCtx _ uH) <- use $ ci . at hnd
        generatedContent . at uH . traverse %=
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml $ ast' `astMerge` ast]
    runAdd hnd@(Handle SCloudInit _) SExecutableScript scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)
    runAdd hnd@(Handle SCloudInit _) SFreeFile (fspec,fH) = do
        fH --> hnd
        fName <- freeFileTempCopy fH (takeFileName $ fspec ^. fileSpecPath)
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))
    runAdd (Handle SDocumentation _) SDocumentation str = lift $ logTrace str
    runAdd _hnd@(Handle SExecutionEnvironment _) SExecutableScript _cmds = do
        return ()
    runAdd hnd@(Handle SExecutionEnvironment _) SFreeFile (destSpec,srcH) = do
        srcH --> hnd
        Just eCxt <- use $ execEnvs . at hnd
        incFile <- lift $ mkTempIn (eCxt ^. execIncDir) "added-file"
        incFile' <- lift $ ensureParentDir incFile
        copyFreeFile srcH incFile'
        (execEnvs . at hnd . traverse . execIncFiles) <>=
            [(incFile', destSpec)]
        bId <- lift $ B9.B9IO.getBuildId
        (execEnvs . at hnd . traverse . execScript) <>=
            incFileScript bId incFile' destSpec
    runAdd _hnd@(Handle SExecutionEnvironment _) SLocalDirectory _mnts = do
        return ()
    runAdd fsH@(Handle SFileSystemBuilder _) SFreeFile (fSpec,fH) = do
        fsBuilder . at fsH . traverse . fsFiles <>= [fSpec]
        Just fileSys <- use $ fsBuilder . at fsH
        let tmpDir = fileSys ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec
    runAdd (Handle SImageRepository _) SVmImage (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- use $ vmImages . at vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH snStr
        vmI --> imageRepositoryH
        addAction imageRepositoryH $ lift $ imageRepoPublish imgFile srcType sn
    runAdd dirH@(Handle SLocalDirectory _) SFreeFile (fSpec,fH) = do
        Just localDir <- use $ localDirs . at dirH
        copyFreeFile' fH (localDir ^. dirTempDir) fSpec
        fH --> dirH
    runAdd hnd@(Handle SUpdateServerRoot _) SVmImage (sn,vmI) = do
        Just destDirH <- use $ updateServerRoots . at hnd
        Just tmpDirCtx <- use $ localDirs . at destDirH
        let destDir = tmpDirCtx ^. dirTempDir
            vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
            SharedImageName snStr = sn
        Just (VmImgCtx srcFileH srcType) <- use $ vmImages . at vmI
        srcFile <- freeFileTempCopy srcFileH snStr
        vmI --> hnd
        addAction hnd $ lift $
            do let imgFile = vmDestDir </> "0.raw"
                   sizeFile = vmDestDir </> "0.size"
                   versionFile = vmDestDir </> "VERSION"
               mkDir vmDestDir
               if srcType /= Raw
                   then convertVmImage srcFile srcType imgFile Raw
                   else moveFile srcFile imgFile
               imgSize <- B9.B9IO.readFileSize imgFile
               renderContentToFile
                   sizeFile
                   (FromString (show imgSize))
                   (Environment [])
               bId <- B9.B9IO.getBuildId
               bT <- B9.B9IO.getBuildDate
               renderContentToFile
                   versionFile
                   (FromString (printf "%s-%s" bId bT))
                   (Environment [])
    runAdd (Handle SVariableBindings _) STemplateVariable (k,v) =
        vars . at k ?= v
    runAdd hnde sa _src =
        fail $ printf "Add %s -> %s: Not Yet Implemented" (show sa) (show hnde)
    -- Conversion
    runConvert hnd@(Handle SCloudInit _) SCloudInitMetaData () = do
        Just (CiCtx (Handle SGeneratedContent h) _) <- use $ ci . at hnd
        return (Handle SCloudInitMetaData h)
    runConvert hnd@(Handle SCloudInit _) SCloudInitUserData () = do
        Just (CiCtx _ (Handle SGeneratedContent h)) <- use $ ci . at hnd
        return (Handle SCloudInitUserData h)
    runConvert (Handle SCloudInitMetaData h) SGeneratedContent () =
        return (Handle SGeneratedContent h)
    runConvert (Handle SCloudInitUserData h) SGeneratedContent () =
        return (Handle SGeneratedContent h)
    runConvert hnd@(Handle SExecutionEnvironment _) SVmImage (imgH,mp) = do
        Just (VmImgCtx _ srcType) <- use $ vmImages . at imgH
        rawImgH <- runConvert imgH SVmImage (Left Raw)
        rawImgH --> hnd
        Just (VmImgCtx rawImgFileH _) <- use $ vmImages . at rawImgH
        rawImgFile <-
            freeFileTempCopy
                rawImgFileH
                (printf "mounted-at-%s" (printMountPoint mp))
        (execEnvs . at hnd . traverse . execImages) <>=
            [(Image rawImgFile Raw Ext4, mp)]
        -- TODO pass along or get rid of the file system type!
        outH <- runConvert rawImgH SVmImage (Left srcType)
        hnd --> outH
        return outH
    runConvert hnd@(Handle SExternalFile hndT) SFreeFile () = do
        Just externalFileName <- use $ externalFiles . at hnd
        tmpFileH <- runCreate SFreeFile $ Just $ hndT ++ "-copy"
        Just (FileCtx tmpFile _) <- use $ localFiles . at tmpFileH
        hnd --> tmpFileH
        addAction hnd $ void $ lift $ copy externalFileName tmpFile
        return tmpFileH
    runConvert hnd@(Handle SFileSystemBuilder _) SFileSystemImage () = do
        Just fileSys <- use $ fsBuilder . at hnd
        return (fileSys ^. fsImgH)
    runConvert hnd@(Handle SFileSystemBuilder _) SFreeFile () = do
        Just fileSys <- use $ fsBuilder . at hnd
        runConvert (fileSys ^. fsImgH) SFreeFile ()
    runConvert hnd@(Handle SFileSystemBuilder _) SVmImage () = do
        Just fileSys <- use $ fsBuilder . at hnd
        runConvert (fileSys ^. fsImgH) SVmImage ()
    runConvert hnd@(Handle SFileSystemImage _) SFileSystemImage destSize = do
        Just (FsCtx inFileH fS) <- use $ fsImages . at hnd
        outFileH <- runConvert inFileH SFreeFile "resized"
        Just (FileCtx outFile _) <- use $ localFiles . at outFileH
        inFileH --> hnd
        hnd --> outFileH
        addAction hnd $ lift $ resizeFileSystem outFile destSize fS
        createFsImage outFileH fS
    runConvert hnd@(Handle SFileSystemImage _) SFreeFile () = do
        Just (FsCtx fH _fS) <- use $ fsImages . at hnd
        return fH
    runConvert hnd@(Handle SFileSystemImage _) SVmImage () = do
        Just (FsCtx fH _) <- use $ fsImages . at hnd
        fH' <- runConvert fH SFreeFile $ printf "Raw-image"
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH
    runConvert hnd@(Handle SFreeFile _) SExternalFile dest = do
        dest' <- lift $ ensureParentDir dest
        newFileH <- runCreate SExternalFile dest'
        hnd --> newFileH
        copyFreeFile hnd dest'
        return newFileH
    runConvert hnd@(Handle SFreeFile _) SFileSystemImage fs = do
        copyH <- runConvert hnd SFreeFile (show fs)
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg
    runConvert hnd@(Handle SFreeFile hndT) SFreeFile dest = do
        newFileH <- runCreate SFreeFile (Just $ hndT ++ "-" ++ dest)
        Just (FileCtx newFile _) <- use $ localFiles . at newFileH
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH
    runConvert hnd@(Handle SFreeFile hndT) SPartitionedVmImage () = do
        let partVmImgHndT = hndT ++ "-partitioned-vm-image"
        (partVmImgHnd,_) <- allocHandle SPartitionedVmImage partVmImgHndT
        file <- runConvert hnd SFreeFile $ "partitioned-vm-image"
        partitionedImgs . at partVmImgHnd ?= file
        hnd --> partVmImgHnd
        return partVmImgHnd
    runConvert hnd@(Handle SFreeFile _) SVmImage imgT = do
        newHnd <- runConvert hnd SFreeFile (printf "vm-image-%s" (show imgT))
        createVmImage newHnd imgT
    runConvert hnd@(Handle SGeneratedContent dest) SFreeFile () = do
        destH <- runCreate SFreeFile $ Just dest
        hnd --> destH
        addAction hnd $
            do Just content <- view $ generatedContent . at hnd
               Just (FileCtx destFile _) <- view $ localFiles . at destH
               env <- view $ vars . to Map.toList . to Environment
               lift $ renderContentToFile destFile content env
        return destH
    runConvert (Handle SImageRepository _) SVmImage sharedImgName = do
        (sharedImgInfo,cachedImage) <- lift $ imageRepoLookup sharedImgName
        imgH <- runCreate SExternalFile cachedImage
        imgCopyH <- runConvert imgH SFreeFile ()
        createVmImage imgCopyH (siImgType sharedImgInfo)
    runConvert destDirH@(Handle SLocalDirectory _) SUpdateServerRoot () = do
        (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
        hnd --> destDirH
        updateServerRoots . at hnd ?= destDirH
        return hnd
    runConvert hnd@(Handle SPartitionedVmImage hndT) SFreeFile partSpec@(MBRPartition pIndex) = do
        let dest = hndT ++ "-partition-" ++ show pIndex
        Just srcFileH <- use $ partitionedImgs . at hnd
        Just (FileCtx srcFileName _) <- use $ localFiles . at srcFileH
        destH <- runCreate SFreeFile $ Just $ dest
        Just (FileCtx destFile _) <- use $ localFiles . at destH
        hnd --> destH
        addAction hnd $ lift $ extractPartition partSpec srcFileName destFile
        return destH
    runConvert hnd@(Handle SVmImage _) SFileSystemImage () = do
        hnd' <- runConvert hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- use $ vmImages . at hnd'
        runConvert srcFileH SFileSystemImage Ext4 -- TODO
    runConvert hnd@(Handle SVmImage _) SFreeFile () = do
        Just (VmImgCtx srcFileH _srcType) <- use $ vmImages . at hnd
        return srcFileH
    runConvert hnd@(Handle SVmImage _) SVmImage (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- use $ vmImages . at hnd
        destImgFileH <-
            runConvert srcImgFileH SFreeFile $
            printf "resized-%d-%s" destSize (show destSizeU)
        Just (FileCtx destImgFile _) <- use $ localFiles . at destImgFileH
        srcImgFileH --> hnd
        addAction hnd $ lift $
            resizeVmImage destImgFile destSize destSizeU srcType
        hnd --> destImgFileH
        createVmImage destImgFileH srcType
    runConvert hnd@(Handle SVmImage hndT) SVmImage (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- use $ vmImages . at hnd
        srcFileCopy <- freeFileTempCopy srcImgFileH $ "conversion-src"
        destImgFileH <-
            runCreate SFreeFile $ Just $ hndT ++ "-converted-to-" ++
            show destType
        Just (FileCtx destImgFile _) <- use $ localFiles . at destImgFileH
        srcImgFileH --> hnd
        addAction hnd $ lift $
            convertVmImage srcFileCopy srcType destImgFile destType
        hnd --> destImgFileH
        createVmImage destImgFileH destType
    runConvert hA sB _src =
        fail $
        printf "Convert %s -> %s: Not Yet Implemented" (show hA) (show sB)
    -- Export
    runExport hnd@(Handle SFileSystemImage _) destFile = do
        Just fileSys <- use $ fsImages . at hnd
        runExport (fileSys ^. fsFileH) destFile
    runExport hnd@(Handle SFreeFile _) destFile =
        (lift $ ensureParentDir destFile) >>= copyFreeFile hnd
    runExport hnd@(Handle SLocalDirectory _) destDir = do
        destDir' <- lift $ ensureParentDir destDir
        localDirs . at hnd . traverse . dirExports <>= [destDir']
    runExport hnd@(Handle SVmImage _) destFile = do
        Just (VmImgCtx fH _) <- use $ vmImages . at hnd
        runExport fH destFile
    runExport hnd _dest =
        fail $ printf "Export %s: Not Yet Implemented" (show hnd)

-- | Add a new copy to a 'FreeFile' at the specified destination
copyFreeFile :: Handle 'FreeFile -> FilePath -> IoCompiler ()
copyFreeFile src dest = localFiles . at src . traverse . fCopies <>= [dest]

-- | Add a new copy to a 'FreeFile' using a unique temp file containg
-- a given string for better debugging, and return the path to the copy.
freeFileTempCopy :: Handle 'FreeFile -> String -> IoCompiler FilePath
freeFileTempCopy src name = do
    Just fileCtx <- use $ localFiles . at src
    dest <-
        lift $
        mkTemp $
        printf
            "%s-%s"
            (takeFileName (fileCtx ^. fFileName))
            (takeFileName name)
    dest' <- lift $ ensureParentDir dest
    copyFreeFile src dest'
    return dest'

-- | Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))

-- | Create a 'FsCtx' from an existing file and the file system type.
createFsImage :: Handle 'FreeFile -> FileSystem -> IoCompiler (Handle 'FileSystemImage)
createFsImage fH fs = do
    (hnd,_) <- allocHandle SFileSystemImage $ "fs-img-" ++ show fs
    fsImages . at hnd ?= FsCtx fH fs
    return hnd

-- | Create a vm image entry in the context.
createVmImage :: Handle 'FreeFile -> ImageType -> IoCompiler (Handle 'VmImage)
createVmImage srcFileH vmt = do
    (hnd,_) <- allocHandle SVmImage ("vm-image-" ++ show vmt)
    vmImages . at hnd ?= VmImgCtx srcFileH vmt
    srcFileH --> hnd
    return hnd

-- | Create a @cloud-config@ compatibe @write_files@ 'AST' object.
toUserDataWriteFilesAST :: FileSpec -> Content -> AST Content YamlObject
toUserDataWriteFilesAST (FileSpec fileName (s,u,g,o) userName groupName) content =
    ASTObj
        [ ( "write_files"
          , ASTArr
                [ ASTObj
                      [ ("path", ASTString fileName)
                      , ("owner", ASTString (userName ++ ":" ++ groupName))
                      , ("permissions", ASTString (printf "%i%i%i%i" s u g o))
                      , ("content", ASTEmbed content)]])]

-- | Create a @cloud-config@ compatibe @runcmd@ 'AST' object.
toUserDataRunCmdAST :: Script -> AST Content YamlObject
toUserDataRunCmdAST scr = ASTObj [("runcmd", ASTArr [ASTString cmd])]
  where
    cmd = toBashOneLiner scr

-- * Utilities

-- | Create a new unique handle and store it in the state.
allocHandle :: SArtifact a -> String -> IoCompiler (Handle a, SomeHandle)
allocHandle sa str = do
    v <- addVertex
    let h = formatHandle v sa str
    h' <- storeHandle h v
    actions . at v ?=
        [ lift $ logTrace $
          printf "==[B9-EXEC-ARTIFACT]==============[%s//%s]" (show sa) str]
    return (h, h')

-- | Add a handle to the vertex <-> handle maps in the state and return the
-- existential 'SomeHandle' that was stored in place of the polymorphic 'Handle
-- a'.
storeHandle :: Handle a -> Vertex -> IoCompiler SomeHandle
storeHandle h v = do
  let h' = SomeHandle h
  hToV . at h' ?= v
  vToH . at v ?= h'
  return h'

-- | Return a new and unique vertex (i.e. artifact id)
addVertex :: IoCompiler Vertex
addVertex = do
  v <- use nextVertex
  nextVertex += 1
  return v

-- | Generate a handle with formatted title
formatHandle :: Vertex -> SArtifact a -> String -> Handle a
formatHandle v sa str =
    handle sa $
    if str == ""
        then (show v)
        else (str ++ "-" ++ show v)

-- | Add a dependency of one resource to another
(-->) :: Handle a -> Handle b -> IoCompiler ()
h --> h' = do
    Just v <- lookupVertex h
    Just v' <- lookupVertex h'
    dependencies <>= [(v, v')]

-- | Return the vertex of a handle.
lookupVertex :: Handle a -> IoCompiler (Maybe Vertex)
lookupVertex h = use $ hToV . at (SomeHandle h)

-- | Generate a 'Script' that copies an included file in a
-- container from the mounted directory to the actual destination.
incFileScript :: String -> FilePath -> FileSpec -> Script
incFileScript buildId tmpIncFile fSpec =
    Begin
        [ Run "cp" [srcPath, destPath]
        , Run "chmod" [printf "%d%d%d%d" s u g o, destPath]
        , Run "chown" [printf "%s:%s" userName groupName, destPath]]
  where
    (FileSpec destPath (s,u,g,o) userName groupName) = fSpec
    srcPath = includedFileContainerPath buildId </> incFile
    incFile = takeFileName tmpIncFile

-- | Return the mount point for files from the build host to be
-- included in the container.
includedFileContainerPath :: String -> FilePath
includedFileContainerPath buildId =
    "/" ++ buildId <.> "mnt" </> "included-files"

-- * Support for 'IoProgBuilder's

-- | Add a build action to a handle
addAction :: Handle a -> IoProgBuilder () -> IoCompiler ()
addAction h a = do
  Just v <- lookupVertex h
  actions . at v . traverse <>= [a]

-- | Run an 'IoProgBuilder' action.
runIoProgBuilder :: IoProgBuilder a -> IoCompiler a
runIoProgBuilder a = do
  ctx <- get
  lift $ runReaderT a ctx
