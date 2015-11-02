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
        SizeUnit(..), Mounted, MountPoint(..), FileSystemCreation(..),
        VmImageSpec(..), vmImgResize, vmImgType, vmImgFileSystem)
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
    , _actions :: Map.Map Graph.Vertex [IoCompiler ()]
    , _hToV :: Map.Map SomeHandle Graph.Vertex
    , _vToH :: Map.Map Graph.Vertex SomeHandle
    , _dependencies :: [Graph.Edge]
    }

instance Default Ctx where
    def = Ctx 0 def def def def def def def def def def []

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
    } deriving (Show)

-- | Context of a 'SReadOnlyFile'
data RofCtx = RofCtx
    { _rofFileName :: FilePath
    } deriving (Show)

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFileHandle :: Handle 'ReadOnlyFile
    , _vmiSpec :: VmImageSpec
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
        result <- interpret p
        runAllActions
        return result

-- | Compile a 'Program' but run no actions, instead just print out information
-- about the program using 'logTrace'
inspect :: Show a => Program a -> IoProgram String
inspect p = evalStateT compileSt def
  where
    compileSt = do
        res <- interpret $ p
        mG <- dependencyGraph
        case mG of
            Just g -> do
                handles <- use vToH
                return $ printDependencyGraph g handles
            Nothing -> do
                return $ "No artifacts." ++ show res

-- | Run all actions in correct order according to the dependency graph.
runAllActions :: IoCompiler ()
runAllActions = do
    mG <- dependencyGraph
    case mG of
        Just g -> do
            lift $ logTrace "Artifact dependency forest:"
            handles <- use vToH
            lift $ logTrace $ printDependencyGraph g handles
            forM_ (Graph.topSort g) runActionForVertex
        Nothing -> do
            lift $ logTrace "No artifacts."
            return ()
  where
    runActionForVertex vertex = do
        actionsForVertex <- use $ actions . at vertex . to fromJust
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
    "Dependency forest:" :
    Tree.drawForest
        (fmap (printSomeHandle . flip Map.lookup handles) <$> Graph.dff g) :
    "Build order:" :
    ((printSomeHandle . flip Map.lookup handles) <$> Graph.topSort g)
  where
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
                def & metaData .~ (ASTObj [("instance-id", ASTString iid)]) &
                userData .~
                (ASTObj []) &
                metaDataH .~
                mh &
                userDataH .~
                uh
        ci . at hnd ?= ciCtx
        hnd --> mh
        hnd --> uh
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
               cCtx <- use $ generatedContent . at hnd . to fromJust
               lift $ renderContentToFile destFile' (cCtx ^. cContent) env
        return hnd
    runCreate SLocalDirectory () = do
        tmp <- lift $ mkTempDir "local-dir"
        (hnd,_) <- allocHandle SLocalDirectory tmp
        localDirs . at hnd ?= (def & dirTempDir .~ tmp)
        return hnd
    runCreate SFileSystemImage fsCreate@(FileSystemCreation _ fsLabel _ _) = do
        (hnd,_) <- allocHandle SFileSystemImage fsLabel
        out <- lift $ mkTemp $ intercalate "-" ["file-system-image", fsLabel]
        fH <- runCreate SReadOnlyFile out
        hnd --> fH
        tmpDir <- lift $ mkTempDir "file-system-content"
        fileSystems . at hnd ?= FsCtx [] tmpDir fH
        addAction fH $
            do files <- use $ fileSystems . at hnd . to fromJust . fsFiles
               lift $ createFileSystem out fsCreate tmpDir files
        return hnd
    runCreate SReadOnlyFile fn = do
        (hnd,_) <- allocHandle SReadOnlyFile fn
        roFiles . at hnd ?= RofCtx fn
        return hnd
    runCreate SVmImage (fileHnd,vmSpec) = do
        (hnd,_) <- allocHandle SVmImage "vm-image"
        -- If input file resize is necessary, export the file handle and attach
        -- a resize action.
        if (vmSpec ^. vmImgResize /= KeepSize)
            then do
                resizedSrcFile <- lift $ mkTemp "resized-src-img-file"
                resizedSrcFileH <- runExport fileHnd $ Just resizedSrcFile
                resizedSrcFileH --> hnd
                addAction resizedSrcFileH $ lift $
                    do resizedSrcFile' <- getRealPath resizedSrcFile
                       let i =
                               Image
                                   resizedSrcFile'
                                   (vmSpec ^. vmImgType)
                                   (vmSpec ^. vmImgFileSystem)
                       resizeVmImage i (vmSpec ^. vmImgResize)
                vmImages . at hnd ?= VmImgCtx resizedSrcFileH vmSpec
            else do
                fileHnd --> hnd
                vmImages . at hnd ?= VmImgCtx fileHnd vmSpec
        return hnd
    runCreate sa _src = return $ singletonHandle sa
    -- Update
    runUpdate hnd@(Handle SGeneratedContent _) c = do
        generatedContent . at hnd . traverse . cContent %=
            \cOld ->
                 Concat [cOld, c]
        return ()
    runUpdate _hnd _src = return ()
    -- Add
    runAdd _ SDocumentation str = lift $ logTrace str
    runAdd _ STemplateVariable (k,v) = vars . at k ?= v
    runAdd dirH@(Handle SLocalDirectory _) SReadOnlyFile (fSpec,fH) = do
        fH --> dirH
        tmpDir <- use $ localDirs . at dirH . to fromJust . dirTempDir
        addAction dirH $ do copyReadOnlyFile' fH fSpec tmpDir
    runAdd fsH@(Handle SFileSystemImage _) SReadOnlyFile (fSpec,fH) = do
        fH --> fsH
        fileSystems . at fsH . traverse . fsFiles <>= [fSpec]
        tmpDir <- use $ fileSystems . at fsH . to fromJust . fsTempDir
        addAction fsH $ do copyReadOnlyFile' fH fSpec tmpDir
    runAdd hnd@(Handle SCloudInit _) SCloudInitMetaData ast =
        ci . at hnd . traverse . metaData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SCloudInitUserData ast =
        ci . at hnd . traverse . userData %= (`astMerge` ast)
    runAdd hnd@(Handle SCloudInit _) SReadOnlyFile (fspec,fH) = do
        fH --> hnd
        fName <- use $ roFiles . at fH . to fromJust . rofFileName
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))
    runAdd hnd@(Handle SCloudInit _) SExecutableScript scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)
    runAdd _hnde _sa _src = return ()
    -- Export
    runExport hnd@(Handle SCloudInit _) () = do
        mH <- use (ci . at hnd . to fromJust . metaDataH)
        uH <- use (ci . at hnd . to fromJust . userDataH)
        addAction mH $
            do ciCtx <- use (ci . at hnd . to fromJust)
               let mC = ciCtx ^. metaData
               interpret $ appendContent mH (RenderYaml mC)
        addAction uH $
            do ciCtx <- use (ci . at hnd . to fromJust)
               let uC = ciCtx ^. userData
               interpret $ appendContent uH (RenderYaml uC)
        return (mH, uH)
    runExport hnd@(Handle SLocalDirectory _) mDestDir = do
        destDir <- lift $ maybe (mkTempDir "tmp-dir") return mDestDir
        tmpDir <- use $ localDirs . at hnd . to fromJust . dirTempDir
        destDirH <- runCreate SLocalDirectory ()
        hnd --> destDirH
        addAction destDirH $
            do destDir' <- lift $ ensureParentDir destDir
               lift $ moveDir tmpDir destDir'
        return destDirH
    runExport hnd@(Handle SFileSystemImage _) mDestFile = do
        tmpH <- use $ fileSystems . at hnd . to fromJust . fsFileH
        runExport tmpH mDestFile
    runExport hnd@(Handle SReadOnlyFile _) mDestFile = do
        destFile <- lift $ maybe (mkTemp "tmp-file") return mDestFile
        destH <- runCreate SReadOnlyFile destFile
        hnd --> destH
        addAction destH $ copyReadOnlyFile hnd destFile
        return destH
    runExport hnd@(Handle SGeneratedContent _) mDestFile = do
        tmpH <- use $ generatedContent . at hnd . to fromJust . cFileH
        runExport tmpH mDestFile
    runExport hnd@(Handle SVmImage _) (mDestFile,mDestSpec) = do
        -- NOTE: This step is done on every export since it is expected that the
        -- typical use case is to export an image only once. This enables us to
        -- use a move instruction instead of a copy, which takes more time and
        -- disk space.
        -- Convert the image from the source file to a temp file:
        VmImgCtx srcFileH srcSpec <- use $ vmImages . at hnd . to fromJust
        let destSpec = fromMaybe (srcSpec & vmImgResize .~ KeepSize) mDestSpec
        when (srcSpec ^. vmImgFileSystem /= destSpec ^. vmImgFileSystem) $ fail $
            printf
                "file system conversion not supported: %s -> %s"
                (show $ srcSpec ^. vmImgFileSystem)
                (show $ destSpec ^. vmImgFileSystem)
        tmpFile <- lift $ mkTemp "converted-img-file"
        tmpH <- runCreate SReadOnlyFile tmpFile
        hnd --> tmpH
        srcFile <- use $ roFiles . at srcFileH . to fromJust . rofFileName
        addAction tmpH $ lift $
            do srcFile' <- getRealPath srcFile
               tmpFile' <- ensureParentDir tmpFile
               convertVmImage
                   srcFile'
                   (srcSpec ^. vmImgType)
                   tmpFile'
                   (destSpec ^. vmImgType)
        -- Resize the temp file and move it to the destination:
        destFile <- lift $ maybe (mkTemp "tmp-img-file") return mDestFile
        destH <- runCreate SReadOnlyFile destFile
        tmpH --> destH
        addAction destH $ lift $
            do tmpFile' <- ensureParentDir tmpFile
               let tmpImg =
                       Image
                           tmpFile'
                           (destSpec ^. vmImgType)
                           (destSpec ^. vmImgFileSystem)
                   r = destSpec ^. vmImgResize
               unless (r == KeepSize) $ resizeVmImage tmpImg r
               destFile' <- ensureParentDir destFile
               moveFile tmpFile' destFile'
        return destH
    runExport _hnd _dest = fail "Not Yet Implemented"

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
    src <- use $ roFiles . at srcH . to fromJust . rofFileName
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

-- | Create a new unique handle and store it in the state
allocHandle :: SArtifact a -> String -> IoCompiler (Handle a, SomeHandle)
allocHandle sa str = do
    v <- use nextVertex
    nextVertex += 1
    let h =
            handle sa $
            if str == ""
                then (show v)
                else (str ++ "-" ++ show v)
        h' = SomeHandle h
    hToV . at h' ?= v
    vToH . at v ?= h'
    actions . at v ?= []
    return (h, h')

-- | Add a dependency of one resource to another
(-->) :: Handle a -> Handle b -> IoCompiler ()
h --> h' = do
  Just v <- lookupVertex h
  Just v' <- lookupVertex h'
  dependencies <>= [(v,v')]

-- | Add a build action to a handle
addAction :: Handle a -> IoCompiler () -> IoCompiler ()
addAction h a = do
  Just v <- lookupVertex h
  actions . at v . traverse <>= [a]

-- | Return the vertex of a handle.
lookupVertex :: Handle a -> IoCompiler (Maybe Graph.Vertex)
lookupVertex h = use $ hToV . at (SomeHandle h)
