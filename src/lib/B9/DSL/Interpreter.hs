-- | Compile a 'Program' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.DSL.Interpreter where -- TODO rename to Compiler

import B9.B9IO
import B9.Content
       (Content(..), Environment(..), AST(..), YamlObject(..), astMerge,
        FileSpec(..), fileSpecPath)
import B9.DSL
import B9.DiskImages
       (Image(..), FileSystem(..), ImageSize(..), ImageType(..), Mounted,
        MountPoint(..), SharedImageName(..), siImgType, printMountPoint)
import B9.ExecEnv
import B9.FileSystems (FileSystemSpec(..))
import B9.ShellScript (Script(..), toBashOneLiner)
import Control.Lens hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Function
import Data.Graph as Graph
import Data.Map as Map
import Data.Maybe
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
    { _nextVertex :: Graph.Vertex
    , _vars :: Map.Map String String
    , _ci :: Map.Map (Handle 'CloudInit) CiCtx
    , _generatedContent :: Map.Map (Handle 'GeneratedContent) Content
    , _localDirs :: Map.Map (Handle 'LocalDirectory) DirCtx
    , _fsBuilder :: Map.Map (Handle 'FileSystemBuilder) FsBuilderCtx
    , _fsImages :: Map.Map (Handle 'FileSystemImage) FsCtx
    , _externalFiles :: Map.Map (Handle 'ExternalFile) FilePath
    , _localFiles :: Map.Map (Handle 'FreeFile) FileCtx
    , _vmImages :: Map.Map (Handle 'VmImage) VmImgCtx
    , _partitionedImgs :: Map.Map (Handle 'PartitionedVmImage) FilePath
    , _updateServerRoots :: Map.Map (Handle 'UpdateServerRoot) (Handle 'LocalDirectory)
    , _execEnvs :: Map.Map (Handle 'ExecutionEnvironment) ExecEnvCtx
    , _actions :: Map.Map Graph.Vertex [IoProgBuilder ()]
    , _hToV :: Map.Map SomeHandle Graph.Vertex
    , _vToH :: Map.Map Graph.Vertex SomeHandle
    , _dependencies :: [Graph.Edge]
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
    } deriving (Show)

instance Default DirCtx where
    def = DirCtx "/tmp"

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
            forM_ (Graph.topSort g) runActionForVertex
        Nothing -> do
            lift $ logTrace "No artifacts."
            return ()
  where
    runActionForVertex vertex = do
        Just actionsForVertex <- use $ actions . at vertex
        runIoProgBuilder $ sequence_ actionsForVertex

-- | Generate a graph from the artifact dependencies in the compiler context.
dependencyGraph :: IoCompiler (Maybe Graph.Graph)
dependencyGraph = do
    maxVertex <- use nextVertex
    if (maxVertex > 0)
        then do
            deps <- use dependencies
            return $ Just $ Graph.buildG (0, maxVertex - 1) deps
        else return Nothing

-- | Show the dependency graph from the compiler context.
printDependencyGraph :: Graph.Graph -> Map.Map Graph.Vertex SomeHandle -> String
printDependencyGraph g handles =
    unlines $
    "digraph artifactDependencyGraph {" :
    (fmap (printEdge handles) (edges g)) ++
    "}" :
    "Dependency forest:" :
    Tree.drawForest (fmap (printVertex handles) <$> Graph.dff g) :
    "Build order:" : (printVertex handles <$> Graph.topSort g)

-- | Convert an edge to a formatted string
printEdge :: Map.Map Graph.Vertex SomeHandle -> Graph.Edge -> String
printEdge handles (u,v) = printf "  %s   ->    %s" (show (printVertex handles u)) (show (printVertex handles v))

-- | Convert a vertex to a formatted string
printVertex :: Map.Map Graph.Vertex SomeHandle -> Graph.Vertex -> String
printVertex handles v =
    printf "%s(%d)" (printSomeHandle $ Map.lookup v handles) v

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle (Handle _s t))) = t
printSomeHandle Nothing = "??error??"

instance Interpreter IoCompiler where
    -- Create
    runCreate SVmImage (srcFile,vmt) = do
        origH <- runCreate SExternalFile srcFile
        tmpH <- runConvert origH SFreeFile ()
        createVmImage tmpH vmt
    runCreate SUpdateServerRoot destDirH = do
        (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
        hnd --> destDirH
        updateServerRoots . at hnd ?= destDirH
        return hnd
    runCreate SPartitionedVmImage file = do
        (hnd,_) <- allocHandle SPartitionedVmImage "partitioned-disk"
        partitionedImgs . at hnd ?= file
        return hnd
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        (hnd@(Handle _ iid),_) <-
            allocHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        mH <-
            runCreate
                SGeneratedContent
                (Concat
                     [ FromString "#cloud-config\n"
                     , RenderYaml $ ASTObj [("instance-id", ASTString iid)]])
        hnd --> mH
        uH <-
            runCreate
                SGeneratedContent
                (Concat [FromString "#cloud-config\n", RenderYaml $ ASTObj []])
        hnd --> uH
        ci . at hnd ?= CiCtx mH uH
        return hnd
    runCreate SGeneratedContent c = do
         (hnd,_) <-
            allocHandle SGeneratedContent "generated-content"
         generatedContent . at hnd ?= c
         return hnd
    runCreate SLocalDirectory () = do
        tmp <- lift $ mkTempDir "local-dir"
        (hnd,_) <- allocHandle SLocalDirectory tmp
        localDirs . at hnd ?= DirCtx tmp
        return hnd
    runCreate SFileSystemBuilder fsSpec@(FileSystemSpec t fsLabel _ _) = do
        (hnd,_) <- allocHandle SFileSystemBuilder fsLabel
        fsFile <- lift $ mkTemp $ (show t) ++ "-fs-image-" ++ fsLabel
        fH <- runCreate SFileSystemImage (fsFile, t)
        hnd --> fH
        tmpDir <- lift $ mkTempDir "file-system-content"
        fsBuilder . at hnd ?= FsBuilderCtx [] tmpDir fH
        addAction hnd $
            do Just fileSys <- view $ fsBuilder . at hnd
               out' <- lift $ ensureParentDir fsFile
               lift $ createFileSystem out' fsSpec tmpDir (fileSys ^. fsFiles)
        return hnd
    runCreate SFileSystemImage (f,fs) = do
        fExtH <- runCreate SExternalFile f
        fIntH <- runConvert fExtH SFreeFile ()
        createFsImage fIntH fs
    runCreate SExternalFile fn = do
        (hnd,_) <- allocHandle SExternalFile fn
        externalFiles . at hnd ?= fn
        return hnd
    runCreate SExecutionEnvironment e = do
        (hnd,_) <- allocHandle SExecutionEnvironment (e ^. execEnvTitle)
        incDir <- lift $ mkTempDir "included-files"
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
    runCreate SFreeFile mTempName = do
        -- TODO escape tempName, allow only a-zA-Z0-9.-_:+=
        let tempName = fromMaybe "tmp-file" mTempName
        tempFile <- lift $ mkTemp tempName
        (hnd,_) <- allocHandle SFreeFile tempName
        localFiles . at hnd ?= FileCtx tempFile []
        addAction hnd $
            do Just (FileCtx src copies) <- view (localFiles . at hnd)
               lift $
                   do src' <- getRealPath src
                      case copies of
                          (dest1:destRest) -> do
                              ensureParentDir dest1 >>= moveFile src'
                              forM_ destRest $ (>>= copy src') .
                                  ensureParentDir
                          [] ->
                              logTrace $ printf "No copies of %s required" src'
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
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _ STemplateVariable (k,v) = vars . at k ?= v
    runAdd dirH@(Handle SLocalDirectory _) SFreeFile (fSpec,fH) = do
        Just localDir <- use $ localDirs . at dirH
        copyFreeFile' fH (localDir ^. dirTempDir) fSpec
        fH --> dirH
    runAdd fsH@(Handle SFileSystemBuilder _) SFreeFile (fSpec,fH) = do
        fsBuilder . at fsH . traverse . fsFiles <>= [fSpec]
        Just fileSys <- use $ fsBuilder . at fsH
        let tmpDir = fileSys ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec
    runAdd hnd@(Handle SCloudInit _) SExecutableScript scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)
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
    runAdd hnd@(Handle SCloudInit _) SFreeFile (fspec,fH) = do
        fH --> hnd
        fName <- freeFileTempCopy fH (takeFileName $ fspec ^. fileSpecPath)
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))
    runAdd (Handle SImageRepository _) SSharedVmImage (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- use $ vmImages . at vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH snStr
        vmI --> imageRepositoryH
        addAction imageRepositoryH $ lift $
            do srcFile <- getRealPath imgFile
               imageRepoPublish srcFile srcType sn
        return ()
    runAdd hnd@(Handle SUpdateServerRoot _) SSharedVmImage (sn,vmI) = do
        Just destDirH <- use $ updateServerRoots . at hnd
        Just tmpDirCtx <- use $ localDirs . at destDirH
        let destDir = tmpDirCtx ^. dirTempDir
            vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
            SharedImageName snStr = sn
        Just (VmImgCtx srcFileH srcType) <- use $ vmImages . at vmI
        srcFile <- freeFileTempCopy srcFileH snStr
        vmI --> hnd
        addAction hnd $ lift $
            do srcFile' <- getRealPath srcFile
               let imgFile = vmDestDir </> "0.raw"
                   sizeFile = vmDestDir </> "0.size"
                   versionFile = vmDestDir </> "VERSION"
               mkDir vmDestDir
               if srcType /= Raw
                   then convertVmImage srcFile' srcType imgFile Raw
                   else moveFile srcFile' imgFile
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
        return ()
    runAdd hnd@(Handle SExecutionEnvironment _) SMountedVmImage (imgH,mp) = do
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
    runAdd _hnd@(Handle SExecutionEnvironment _) SMountedHostDir _mnts = do
        return ()
    runAdd _hnd@(Handle SExecutionEnvironment _) SExecutableScript _cmds = do
        return ()
    runAdd hnd@(Handle SExecutionEnvironment _) SFreeFile (destSpec,srcH) = do
        srcH --> hnd
        Just eCxt <- use $ execEnvs . at hnd
        incFile <- lift $ mkTempIn (eCxt ^. execIncDir) "added-file"
        copyFreeFile srcH incFile
        (execEnvs . at hnd . traverse . execIncFiles) <>= [(incFile, destSpec)]
        bId <- lift $ B9.B9IO.getBuildId
        (execEnvs . at hnd . traverse . execScript) <>=
            incFileScript bId incFile destSpec
        return ()
    runAdd hnde sa _src =
        fail $ printf "Add %s -> %s: Not Yet Implemented" (show sa) (show hnde)
    -- Conversion
    runConvert hnd@(Handle SFreeFile _) SFreeFile dest = do
        newFileH <- runCreate SFreeFile (Just dest)
        Just (FileCtx newFile _) <- use $ localFiles . at newFileH
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH
    runConvert hnd@(Handle SFreeFile _) SVmImage imgT = do
        newHnd <- runConvert hnd SFreeFile (printf "vm-image-%s" (show imgT))
        createVmImage newHnd imgT
    runConvert hnd@(Handle SFreeFile _) SFileSystemImage fs = do
        newHnd <- runConvert hnd SFreeFile (printf "fs-image-%s" (show fs))
        createFsImage newHnd fs
    runConvert hnd@(Handle SFreeFile _) SExternalFile dest = do
        newFileH <- runCreate SExternalFile dest
        copyFreeFile hnd dest
        hnd --> newFileH
        return newFileH
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
    runConvert hnd@(Handle SVmImage _) SFileSystemImage () = do
        hnd' <- runConvert hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- use $ vmImages . at hnd'
        runConvert srcFileH SFileSystemImage Ext4 -- TODO
    runConvert hnd@(Handle SFileSystemBuilder _) SFileSystemImage () = do
        Just fileSys <- use $ fsBuilder . at hnd
        return (fileSys ^. fsImgH)
    runConvert hnd@(Handle SFileSystemImage _) SVmImage () = do
        Just (FsCtx fH fS) <- use $ fsImages . at hnd
        fH' <- runConvert fH SFreeFile $ printf "%s-to-raw" (show fS)
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH
    runConvert hnd@(Handle SFileSystemImage _) SFileSystemImage destSize = do
        Just (FsCtx fH fS) <- use $ fsImages . at hnd
        tmpH <- runConvert fH SFreeFile $ printf "resized-%s" (show fS)
        Just (FileCtx tmpFile _) <- use $ localFiles . at tmpH
        addAction tmpH $ lift $ resizeFileSystem tmpFile destSize fS
        createFsImage tmpH fS
    runConvert hnd@(Handle SVmImage _) SFreeFile () = do
        Just (VmImgCtx srcFileH _srcType) <- use $ vmImages . at hnd
        return srcFileH
    runConvert hnd@(Handle SVmImage _) SVmImage (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- use $ vmImages . at hnd
        destImgFileH <- runConvert srcImgFileH SFreeFile "resized-img"
        srcImgFileH --> destImgFileH
        Just (FileCtx destImgFile _) <- use $ localFiles . at destImgFileH
        addAction destImgFileH $ lift $
            do imgFile <- getRealPath destImgFile
               resizeVmImage imgFile destSize destSizeU srcType
        destImgH <- createVmImage destImgFileH srcType
        hnd --> destImgH
        return destImgH
    runConvert hnd@(Handle SVmImage _) SVmImage (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- use $ vmImages . at hnd
        destImgFileH <-
            runCreate SFreeFile $ Just $  "conversion-dest-" ++ show destType
        Just (FileCtx destImgFile _) <- use $ localFiles . at destImgFileH
        srcImgFileH --> destImgFileH
        srcImgFileCopy <-
            freeFileTempCopy srcImgFileH $ "conversion-src-" ++ show srcType
        addAction destImgFileH $ lift $
            do
               -- convert
               srcFile' <- getRealPath srcImgFileCopy
               convertedFile' <- ensureParentDir destImgFile
               convertVmImage srcFile' srcType convertedFile' destType
        destImgH <- createVmImage destImgFileH destType
        hnd --> destImgH
        return destImgH
    runConvert hnd@(Handle SPartitionedVmImage _) SFreeFile partSpec = do
        Just srcFileName <- use $ partitionedImgs . at hnd
        destH <- runCreate SFreeFile $ Just $ "extracted-" ++ show partSpec
        Just (FileCtx destFile _) <- use $ localFiles . at destH
        hnd --> destH
        addAction destH $ lift $
            do src <- getRealPath srcFileName
               dst <- ensureParentDir destFile
               extractPartition partSpec src dst
        return destH
    runConvert hnd@(Handle SGeneratedContent _) SFreeFile dest = do
        destH <- runCreate SFreeFile $ Just dest
        hnd --> destH
        addAction destH $
            do Just content <- view $ generatedContent . at hnd
               Just (FileCtx destFile _) <- view $ localFiles . at destH
               destFile' <- lift $ getRealPath destFile
               env <- view $ vars . to Map.toList . to Environment
               lift $ renderContentToFile destFile' content env
        return destH
    runConvert hA sB _src =
        fail $
        printf "Convert %s -> %s: Not Yet Implemented" (show hA) (show sB)
    -- Export
    runExport hnd@(Handle SLocalDirectory _) mDestDir = do
        destDir <- lift $ maybe (mkTempDir "tmp-dir") return mDestDir
        Just tmpDirCtx <- use $ localDirs . at hnd
        let tmpDir = tmpDirCtx ^. dirTempDir
        destDirH <- runCreate SLocalDirectory ()
        hnd --> destDirH
        addAction destDirH $ lift $
            do destDir' <- ensureParentDir destDir
               moveDir tmpDir destDir'
        return destDirH
    runExport _hnd@(Handle SVmImage _) _destFile = do
        return ()
    runExport hnd@(Handle SFileSystemImage _) destFile = do
        Just fileSys <- use $ fsImages . at hnd
        runExport (fileSys ^. fsFileH) destFile
    runExport (Handle SImageRepository _) sharedImgName = do
        (sharedImgInfo,cachedImage) <- lift $ imageRepoLookup sharedImgName
        runCreate SVmImage (cachedImage, siImgType sharedImgInfo)
    runExport hnd _dest =
        fail $ printf "Export %s: Not Yet Implemented" (show hnd)

-- | Internal: Add a new copy to a 'FreeFile' at the specified destination
copyFreeFile :: Handle 'FreeFile -> FilePath -> IoCompiler ()
copyFreeFile src dest = localFiles . at src . traverse . fCopies <>= [dest]

-- | Internal: Add a new copy to a 'FreeFile' using a unique temp file containg
-- a given string for better debugging, and return the path to the copy.
freeFileTempCopy :: Handle 'FreeFile -> String -> IoCompiler FilePath
freeFileTempCopy src name = do
    Just fileCtx <- use $ localFiles . at src
    dest <- lift $ mkTemp $ printf "%s-TO-%s" (fileCtx ^. fFileName) name
    copyFreeFile src dest
    return dest

-- | Internal: Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))

-- | Internal: Create a 'FsCtx' from an existing file and the file system type.
createFsImage :: Handle 'FreeFile -> FileSystem -> IoCompiler (Handle 'FileSystemImage)
createFsImage fH fs = do
    (hnd,_) <- allocHandle SFileSystemImage $ "fs-img-" ++ show fs
    fH --> hnd
    fsImages . at hnd ?= FsCtx fH fs
    return hnd

-- | Internal: Create a vm image entry in the context.
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

-- | Copy a 'ReadOnlyFile' to a non-existing output file.
copyReadOnlyFile :: FilePath -> FilePath -> IoProgram FilePath
copyReadOnlyFile src dst = do
    src' <- getRealPath src
    dst' <- ensureParentDir dst
    copy src' dst'
    return dst'

-- | Copy a 'ReadOnlyFile' to a directory and 'FileSpec' and create
--   missing intermediate directories.
copyReadOnlyFile' :: FilePath
                  -> FileSpec
                  -> FilePath
                  -> IoProgram ()
copyReadOnlyFile' src dstSpec dstDir =
    void $ copyReadOnlyFile src (dstDir </> (dstSpec ^. fileSpecPath))

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
storeHandle :: Handle a -> Graph.Vertex -> IoCompiler SomeHandle
storeHandle h v = do
  let h' = SomeHandle h
  hToV . at h' ?= v
  vToH . at v ?= h'
  return h'

-- | Return a new and unique vertex (i.e. artifact id)
addVertex :: IoCompiler Graph.Vertex
addVertex = do
  v <- use nextVertex
  nextVertex += 1
  return v

-- | Generate a handle with formatted title
formatHandle :: Graph.Vertex -> SArtifact a -> String -> Handle a
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
lookupVertex :: Handle a -> IoCompiler (Maybe Graph.Vertex)
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
