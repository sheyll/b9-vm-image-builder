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
       (Image(..), ImageSource(..), ImageDestination(..), FileSystem(..),
        Partition(..), ImageResize(..), ImageSize(..), ImageType(..),
        SizeUnit(..), Mounted, MountPoint(..), SharedImageName(..), siImgType)
import B9.FileSystems (FileSystemResize(..), FileSystemSpec(..))
import B9.ShellScript (Script(..), toBashOneLiner)
import Control.Lens hiding (from, (<.>))
import Control.Monad.State
import Data.Default
import Data.Function
import Data.Graph as Graph
import Data.List
import Data.Map as Map
import Data.Maybe
import Data.Tree as Tree
import System.FilePath
import Text.Printf (printf)

-- | The monad used to compile a 'Program' into an 'IoProgram'
type IoCompiler = StateT Ctx IoProgram

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
    , _generatedContent :: Map.Map (Handle 'GeneratedContent) ContentCtx
    , _localDirs :: Map.Map (Handle 'LocalDirectory) DirCtx
    , _fileSystems :: Map.Map (Handle 'FileSystemImage) FsCtx
    , _roFiles :: Map.Map (Handle 'ReadOnlyFile) RofCtx
    , _vmImages :: Map.Map (Handle 'VmImage) VmImgCtx
    , _partitionedImgs :: Map.Map (Handle 'PartitionedVmImage) (Handle 'ReadOnlyFile)
    , _updateServerRoots :: Map.Map (Handle 'UpdateServerRoot) (Handle 'LocalDirectory)
    , _actions :: Map.Map Graph.Vertex [IoCompiler ()]
    , _hToV :: Map.Map SomeHandle Graph.Vertex
    , _vToH :: Map.Map Graph.Vertex SomeHandle
    , _dependencies :: [Graph.Edge]
    }

instance Default Ctx where
    def = Ctx 0 def def def def def def def def def def def def []

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaData :: (AST Content YamlObject)
    , _userData :: (AST Content YamlObject)
    , _metaDataH :: Handle 'GeneratedContent
    , _userDataH :: Handle 'GeneratedContent
    } deriving (Show)

instance Default CiCtx where
    def =
        CiCtx
            (ASTObj [])
            (ASTObj [])
            (singletonHandle SGeneratedContent)
            (singletonHandle SGeneratedContent)

-- | Context of a 'SLocalDirectory'
data ContentCtx = ContentCtx
    { _cContent :: Content
    , _cFileH :: Handle 'ReadOnlyFile
    } deriving (Show)

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
    { _dirTempDir :: FilePath
    } deriving (Show)

instance Default DirCtx where
    def = DirCtx "/tmp"

-- | Context of a 'SFileSystemImage'
data FsCtx = FsCtx
    { _fsFiles :: [FileSpec]
    , _fsTempDir :: FilePath
    , _fsFileH :: Handle 'ReadOnlyFile
    , _fsType :: FileSystem
    } deriving (Show)

-- | Context of a 'SReadOnlyFile'
data RofCtx = RofCtx
    { _rofFileName :: FilePath
    } deriving (Show)

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFileHandle :: Handle 'ReadOnlyFile
    , _vmiType :: ImageType
    } deriving (Show)

makeLenses ''Ctx
makeLenses ''CiCtx
makeLenses ''ContentCtx
makeLenses ''DirCtx
makeLenses ''FsCtx
makeLenses ''RofCtx
makeLenses ''VmImgCtx

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
        storeHandle h v
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
        sequence_ actionsForVertex

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
    runCreate SCloudInit iidPrefix = do
        buildId <- lift $ getBuildId
        mh <- interpret $ createContent (FromString "#cloud-config\n")
        uh <- interpret $ createContent (FromString "#cloud-config\n")
        (hnd@(Handle _ iid),_) <-
            allocHandle SCloudInit (iidPrefix ++ "-" ++ buildId)
        let ciCtx =
                def &
                metaData  .~ (ASTObj [("instance-id", ASTString iid)]) &
                userData  .~ (ASTObj []) &
                metaDataH .~ mh &
                userDataH .~ uh
        ci . at hnd ?= ciCtx
        hnd --> mh
        addAction mh $
            do Just ciCtx <- use $ ci . at hnd
               let mC = ciCtx ^. metaData
               interpret $ appendContent mh (RenderYaml mC)
        hnd --> uh
        addAction uh $
            do Just ciCtx <- use $ ci . at hnd
               let uC = ciCtx ^. userData
               interpret $ appendContent uh (RenderYaml uC)
        return hnd
    runCreate SGeneratedContent c = do
        (hnd@(Handle _ hndT),_) <-
            allocHandle SGeneratedContent "generated-content"
        destFile <- lift $ mkTemp hndT
        destFileH <- runCreate SReadOnlyFile destFile
        generatedContent . at hnd ?= ContentCtx c destFileH
        hnd --> destFileH
        addAction destFileH $
            do env <- uses vars (Environment . Map.toList)
               destFile' <- lift $ ensureParentDir destFile
               Just cCtx <- use $ generatedContent . at hnd
               lift $ renderContentToFile destFile' (cCtx ^. cContent) env
        return hnd
    runCreate SLocalDirectory () = do
        tmp <- lift $ mkTempDir "local-dir"
        (hnd,_) <- allocHandle SLocalDirectory tmp
        localDirs . at hnd ?= (def & dirTempDir .~ tmp)
        return hnd
    runCreate SFileSystemImage fsSpec@(FileSystemSpec t fsLabel _ _) = do
        (hnd,_) <- allocHandle SFileSystemImage fsLabel
        out <- lift $ mkTemp $ intercalate "-" ["file-system-image", fsLabel]
        fH <- runCreate SReadOnlyFile out
        tmpDir <- lift $ mkTempDir "file-system-content"
        fileSystems . at hnd ?= FsCtx [] tmpDir fH t
        hnd --> fH
        addAction fH $
            do Just fileSys <- use $ fileSystems . at hnd
               lift $ createFileSystem out fsSpec tmpDir (fileSys ^. fsFiles)
        return hnd
    runCreate SReadOnlyFile fn = do
        (hnd,_) <- allocHandle SReadOnlyFile fn
        roFiles . at hnd ?= RofCtx fn
        return hnd
    runCreate SVmImage (srcFileH,vmt) = do
        (hnd,_) <- allocHandle SVmImage ("vm-image-" ++ show vmt)
        vmImages . at hnd ?= VmImgCtx srcFileH vmt
        srcFileH --> hnd
        return hnd
    runCreate SPartitionedVmImage fileHandle = do
        (hnd,_) <- allocHandle SPartitionedVmImage "partitioned-disk"
        partitionedImgs . at hnd ?= fileHandle
        fileHandle --> hnd
        return hnd
    runCreate SUpdateServerRoot destDirH = do
        (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
        hnd --> destDirH
        updateServerRoots . at hnd ?= destDirH
        return hnd
    runCreate sa _src =
        fail $ printf "Create %s: Not Yet Implemented" (show sa)
    -- Update
    runUpdate hnd@(Handle SGeneratedContent _) c = do
        generatedContent . at hnd . traverse . cContent %=
            \cOld ->
                 Concat [cOld, c]
        return ()
    runUpdate hnd _src =
        fail $ printf "Update %s: Not Yet Implemented" (show hnd)
    -- Add
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _ STemplateVariable (k,v) = vars . at k ?= v
    runAdd dirH@(Handle SLocalDirectory _) SReadOnlyFile (fSpec,fH) = do
        Just localDir <- use $ localDirs . at dirH
        fH --> dirH
        addAction dirH $ copyReadOnlyFile' fH fSpec (localDir ^. dirTempDir)
    runAdd fsH@(Handle SFileSystemImage _) SReadOnlyFile (fSpec,fH) = do
        fileSystems . at fsH . traverse . fsFiles <>= [fSpec]
        Just fileSys <- use $ fileSystems . at fsH
        let tmpDir = fileSys ^. fsTempDir
        fH --> fsH
        addAction fsH $ copyReadOnlyFile' fH fSpec tmpDir
    runAdd hnd@(Handle SCloudInit _) SCloudInitMetaData ast =
        ci . at hnd . traverse . metaData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SCloudInitUserData ast =
        ci . at hnd . traverse . userData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SReadOnlyFile (fspec,fH) = do
        Just (RofCtx fName) <- use $ roFiles . at fH
        fH --> hnd
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))
    runAdd hnd@(Handle SCloudInit _) SExecutableScript scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)
    runAdd (Handle SImageRepository _) SSharedVmImage (sn,vmI) = do
        Just (VmImgCtx srcFileH srcType) <- use $ vmImages . at vmI
        Just (RofCtx srcFile) <- use $ roFiles . at srcFileH
        vmI --> imageRepositoryH
        addAction imageRepositoryH $ lift $
            do srcFile' <- getRealPath srcFile
               imageRepoPublish srcFile' srcType sn
        return ()
    runAdd hnd@(Handle SUpdateServerRoot _) SSharedVmImage (sn,vmI) = do
        Just (VmImgCtx srcFileH srcType) <- use $ vmImages . at vmI
        Just (RofCtx srcFile) <- use $ roFiles . at srcFileH
        Just destDirH <- use $ updateServerRoots . at hnd
        Just tmpDirCtx <- use $ localDirs . at destDirH
        let destDir = tmpDirCtx ^. dirTempDir
            vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
            SharedImageName snStr = sn
        vmI --> hnd
        addAction hnd $ lift $
            do srcFile' <- getRealPath srcFile
               let imgFile = vmDestDir </> "0.raw"
                   sizeFile = vmDestDir </> "0.size"
                   versionFile = vmDestDir </> "VERSION"
               mkDir vmDestDir
               if srcType /= Raw
                   then convertVmImage srcFile' srcType imgFile Raw
                   else copy srcFile' imgFile
               size <- B9.B9IO.readFileSize imgFile
               renderContentToFile
                   sizeFile
                   (FromString (show size))
                   (Environment [])
               bId <- B9.B9IO.getBuildId
               bT <- B9.B9IO.getBuildDate
               renderContentToFile
                   versionFile
                   (FromString (printf "%s-%s" bId bT))
                   (Environment [])
        return ()
    runAdd hnde sa _src =
        fail $ printf "Add %s -> %s: Not Yet Implemented" (show sa) (show hnde)
    -- Export
    runExport hnd@(Handle SCloudInit _) () = do
        Just ciCtxInitial <- use $ ci . at hnd
        let mH = ciCtxInitial ^. metaDataH
            uH = ciCtxInitial ^. userDataH
        return (mH, uH)
    runExport hnd@(Handle SLocalDirectory _) mDestDir = do
        destDir <- lift $ maybe (mkTempDir "tmp-dir") return mDestDir
        Just tmpDirCtx <- use $ localDirs . at hnd
        let tmpDir = tmpDirCtx ^. dirTempDir
        destDirH <- runCreate SLocalDirectory ()
        hnd --> destDirH
        addAction destDirH $
            do destDir' <- lift $ ensureParentDir destDir
               lift $ moveDir tmpDir destDir'
        return destDirH
    runExport hnd@(Handle SFileSystemImage _) (mDestFile,mDestSize) = do
        Just fileSys <- use $ fileSystems . at hnd
        let srcH = fileSys ^. fsFileH
            srcT = fileSys ^. fsType
        tmpH    -- TODO inspect where getRealPath is called, and where not
             <-
            case mDestSize    -- TODO inspect carefully all '-->'
                  of
                Nothing -> return srcH
                Just destSize -> do
                    tmpFile <- lift $ mkTemp "file-system-resize"
                    tmpH <- runExport srcH (Just tmpFile)
                    addAction tmpH $ lift $
                        resizeFileSystem tmpFile destSize srcT
                    return tmpH
        runExport tmpH mDestFile
    runExport hnd@(Handle SReadOnlyFile _) mDestFile = do
        destFile <- lift $ maybe (mkTemp "tmp-file") return mDestFile
        destH <- runCreate SReadOnlyFile destFile
        hnd --> destH
        addAction destH $ copyReadOnlyFile hnd destFile
        return destH
    runExport hnd@(Handle SGeneratedContent _) mDestFile = do
        Just genCntCtx <- use $ generatedContent . at hnd
        let tmpH = genCntCtx ^. cFileH
        runExport tmpH mDestFile
    runExport hnd@(Handle SVmImage _) (mDestFile,mDestType,mDestSize) = do
        -- Convert the image from the source file to a temp file:
        Just (VmImgCtx srcFileH srcType) <- use $ vmImages . at hnd
        convertedH <-
            case mDestType of
                Just destType
                  | destType /= srcType -> do
                      Just (RofCtx srcFile) <- use $ roFiles . at srcFileH
                      convertedFile <- lift $ mkTemp "converted-img-file"
                      convertedH <- runCreate SReadOnlyFile convertedFile
                      srcFileH --> convertedH
                      addAction convertedH $ lift $
                          do srcFile' <- getRealPath srcFile
                             convertedFile' <- ensureParentDir convertedFile
                             convertVmImage
                                 srcFile'
                                 srcType
                                 convertedFile'
                                 destType
                      return convertedH
                _ -> runExport srcFileH Nothing
        -- Resize the temp file and move it to the destination:
        sequence_ $
            do (ImageSize destSize destSizeU) <- mDestSize
               return $ addAction convertedH $
                   do Just (RofCtx convertedFile) <-
                          use $ roFiles . at convertedH
                      lift $
                          do convertedFile' <- ensureParentDir convertedFile
                             resizeVmImage
                                 convertedFile'
                                 destSize
                                 destSizeU
                                 (fromMaybe srcType mDestType)
        -- Move the temp file to the destination:
        destFile <- lift $ maybe (mkTemp "resized-img-file") return mDestFile
        addAction convertedH $
            do Just (RofCtx convertedFile) <- use $ roFiles . at convertedH
               lift $
                   do convertedFile' <- getRealPath convertedFile
                      destFile' <- ensureParentDir destFile
                      moveFile convertedFile' destFile'
        destH <- runCreate SReadOnlyFile destFile
        convertedH --> destH
        -- TODO should I: destH --> hnd ?
        return
            destH
    runExport hnd@(Handle SPartitionedVmImage _) (mDestFile,partSpec) = do
        destFile <-
            lift $
            maybe (mkTemp $ "extracted-" ++ show partSpec) return mDestFile
        destH <- runCreate SReadOnlyFile destFile
        hnd --> destH
        Just srcFileH <- use $ partitionedImgs . at hnd
        Just (RofCtx srcFileName) <- use $ roFiles . at srcFileH
        addAction hnd $ lift $
            do src <- getRealPath srcFileName
               dst <- ensureParentDir destFile
               extractPartition partSpec src dst
        return destH
    runExport (Handle SImageRepository _) sharedImgName = do
        (sharedImgInfo,cachedImage) <- lift $ imageRepoLookup sharedImgName
        cachedH <- runCreate SReadOnlyFile cachedImage
        runCreate SVmImage (cachedH, siImgType sharedImgInfo)
    runExport hnd _dest =
        fail $ printf "Export %s: Not Yet Implemented" (show hnd)

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
copyReadOnlyFile :: Handle 'ReadOnlyFile -> FilePath -> IoCompiler ()
copyReadOnlyFile srcH dst = do
    Just (RofCtx src) <- use $ roFiles . at srcH
    src' <- lift $ getRealPath src
    dst' <- lift $ ensureParentDir dst
    lift $ copy src' dst'

-- | Copy a 'ReadOnlyFile' to a directory and 'FileSpec' and create
--   missing intermediate directories.
copyReadOnlyFile' :: Handle 'ReadOnlyFile
                  -> FileSpec
                  -> FilePath
                  -> IoCompiler ()
copyReadOnlyFile' srcH dstSpec dstDir =
    copyReadOnlyFile srcH (dstDir </> (dstSpec ^. fileSpecPath))

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

-- | Add a build action to a handle
addAction :: Handle a -> IoCompiler () -> IoCompiler ()
addAction h a = do
  Just v <- lookupVertex h
  actions . at v . traverse <>= [a]

-- | Return the vertex of a handle.
lookupVertex :: Handle a -> IoCompiler (Maybe Graph.Vertex)
lookupVertex h = use $ hToV . at (SomeHandle h)
