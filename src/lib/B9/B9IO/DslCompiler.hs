{-# LANGUAGE ScopedTypeVariables #-}
-- | Compile a 'ProgramT' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module B9.B9IO.DslCompiler where

import B9.B9IO
import B9.Dsl.Files
import B9.Dsl.Content
import B9.DSL               hiding (use)
import B9.ShellScript       (toBashOneLiner)
import Control.Lens         hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Data
import Data.Graph           as Graph
import Data.Map             as Map hiding (null)
import Data.Monoid
import Data.Singletons
import Data.Tree            as Tree
import System.FilePath
import Text.Printf          (printf)

-- | The monad used to compile a 'ProgramT' into an 'IoProgram'
type IoCompiler = StateT Ctx IoProgram

-- | An alias for 'ProgramT's over 'IoCompiler'
type Program a = ProgramT IoCompiler a

-- | This monad contains all information gathered in 'Ctx' but is
-- 'ReaderT'. This is mainly to prevent an action added with 'addAction' to be
-- able to change the state, especially by adding more actions (which would not
-- be executed).
type IoProgBuilder = ReaderT Ctx IoProgram

-- | State for artifacts required to generate the output program is
--   held in a single 'Map' containing the existential key/value types
--  'SomeHandle' -> 'SomeState'. This way the IoCompiler remains extensible.
type ArtifactStates = Map SomeHandle SomeState

-- | A existential type for holding state for artifacts
data SomeState where
        SomeState :: Typeable a => a -> SomeState
    deriving Typeable

-- | The internal state of the 'IoCompiler' monad
data Ctx = Ctx
    { _nextVertex :: Vertex
    , _actions :: Map Vertex [IoProgBuilder ()]
    , _hToV :: Map SomeHandle Vertex
    , _vToH :: Map Vertex SomeHandle
    , _dependencies :: [Edge]
    , _artifactStates :: ArtifactStates
    }

instance Default Ctx where
    def = Ctx 0 def def def def [] def

-- | Context of a 'SVmImage'
data VmImgCtx = VmImgCtx
    { _vmiFile :: Handle 'FreeFile
    , _vmiType :: ImageType
    } deriving (Show, Typeable)

makeLenses ''Ctx
makeLenses ''VmImgCtx

-- | * Artifact state accessors

unpackCast :: (Typeable b) => Maybe SomeState -> Maybe b
unpackCast x =
  case x of
    Nothing -> Nothing
    Just (SomeState s) -> cast s

useArtifactState
    :: (Typeable b)
    => Handle a -> IoCompiler (Maybe b)
useArtifactState hnd =
    unpackCast <$> use (artifactStates . at (SomeHandle hnd))

putArtifactState
    :: (Typeable b)
    => Handle a -> b -> IoCompiler ()
putArtifactState hnd st =
    artifactStates . at (SomeHandle hnd) ?= SomeState st

modifyArtifactState
    :: (Typeable b)
    => Handle a -> (Maybe b -> Maybe b) -> IoCompiler ()
modifyArtifactState hnd f =
    artifactStates . at (SomeHandle hnd) %= mstate . f . mcast
    where
      mcast (Just (SomeState b')) = cast b'
      mcast Nothing = Nothing
      mstate = fmap SomeState

getArtifactState
    :: (Typeable b)
    => Handle a -> IoProgBuilder (Maybe b)
getArtifactState hnd =
    unpackCast <$> view (artifactStates . at (SomeHandle hnd))


-- | Compile a 'Program' to an 'IoProgram'
compile :: Program a -> IoProgram a
compile p = evalStateT compileSt def
  where
    compileSt = do
        lift
            (do b <- getBuildId
                dbgL
                    "==[B9-PREPARE]=======================================================["
                    b
                    "]")
        createPredefinedHandles
        result <- interpret p
        runAllActions
        lift
            (do b <- getBuildId
                dbgL
                    "==[B9-FINISHED]======================================================["
                    b
                    "]")
        return result

-- | Compile a 'Program' but run no actions, instead just print out information
-- about the program using 'logTrace'
inspect :: Show a => Program a -> IoProgram String
inspect p = evalStateT compileSt def
  where
    compileSt = do
        createPredefinedHandles
        res <- interpret p
        mG <- dependencyGraph
        case mG of
            Just g -> do
                handles <- use vToH
                return (printDependencyGraph g handles)
            Nothing ->
                return ("No artifacts." ++ show res)

-- | Setup the predefined global handles, e.g. 'imageRepositoryH'
createPredefinedHandles :: IoCompiler ()
createPredefinedHandles = allocPredefinedHandle imageRepositoryH

-- | Setup a predefined global handle
allocPredefinedHandle
    :: (SingKind ('KProxy :: KProxy k), Show (Demote (a :: k)))
    => Handle a -> IoCompiler ()
allocPredefinedHandle h = do
    v <- addVertex
    void (storeHandle h v)
    actions . at v ?= []

-- | Run all actions in correct order according to the dependency graph.
runAllActions :: IoCompiler ()
runAllActions = do
    lift
        (do b <- getBuildId
            traceL
                "==[B9-EXECUTE]=======================================================["
                b
                "]")
    mG <- dependencyGraph
    case mG of
        Just g -> forM_ (topSort g) runActionForVertex
        Nothing -> lift (traceL "No artifacts.")
  where
    runActionForVertex vertex = do
        Just actionsForVertex <-
            use (actions . at vertex)
        runIoProgBuilder
            (sequence_ actionsForVertex)

-- | Generate a graph from the artifact dependencies in the compiler context.
dependencyGraph :: IoCompiler (Maybe Graph)
dependencyGraph = do
    maxVertex <- use nextVertex
    if maxVertex > 0
        then do
            deps <- use dependencies
            return (Just (buildG (0, maxVertex - 1) deps))
        else return Nothing

-- | Show the dependency graph from the compiler context.
printDependencyGraph :: Graph -> Map Vertex SomeHandle -> String
printDependencyGraph g handles =
    unlines
        ("digraph artifactDependencyGraph {" :
         fmap (printEdge handles) (edges g) ++
         "}" :
         "Dependency forest:" :
         Tree.drawForest (fmap (printVertex handles) <$> dff g) :
         "Build order:" : (printVertex handles <$> topSort g))

-- | Convert an edge to a formatted string
printEdge :: Map Vertex SomeHandle -> Edge -> String
printEdge handles (u,v) =
    printf
        "  %s   ->    %s"
        (show (printVertex handles u))
        (show (printVertex handles v))

-- | Convert a vertex to a formatted string
printVertex :: Map Vertex SomeHandle -> Vertex -> String
printVertex handles v =
    printf "%s(%d)" (printSomeHandle (Map.lookup v handles)) v

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle h)) = show h
printSomeHandle Nothing = "??error??"


instance CanAdd IoCompiler 'ImageRepository 'VmImage where
    runAdd _ _ (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- useArtifactState vmI
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH snStr
        vmI --> imageRepositoryH
        addAction imageRepositoryH (lift (imageRepoPublish imgFile srcType sn))

instance CanAdd IoCompiler 'LoggingOutput 'LogEvent where
    runAdd _ _ (lvl,msg) = lift $ logMsg lvl msg

instance CanAdd IoCompiler 'UpdateServerRoot 'VmImage where
    runAdd hnd _ (sn,vmI) = do
        Just (destDirH :: Handle 'LocalDirectory) <- useArtifactState hnd
        Just tmpDirCtx <- useArtifactState destDirH
        let destDir = tmpDirCtx ^. dirTempDir
            vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
            SharedImageName snStr = sn
        Just (VmImgCtx srcFileH srcType) <- useArtifactState vmI
        srcFile <- freeFileTempCopy srcFileH snStr
        vmI --> hnd
        addAction
            hnd
            (lift
                 (do let imgFile = vmDestDir </> "0.raw"
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
                         (Environment [])))

instance CanConvert IoCompiler 'ExecutionEnvironment 'VmImage where
    runConvert hnd _ (imgH,mp) = do
        rawH <- runConvert imgH SVmImage (Left Raw)
        rawH --> hnd
        rawFH <- runConvert rawH SFreeFile ()
        mntH <-
            runConvert
                rawFH
                SFreeFile
                (printf "mounted-at-%s" (printMountPoint mp))
        Just (FileCtx mnt _) <- useArtifactState mntH
        modifyArtifactState hnd $ traverse . execImages <>~
            [(Image mnt Raw Ext4, mp)]
        hnd --> mntH
        runConvert mntH SVmImage Raw


instance CanConvert IoCompiler 'FileSystemBuilder 'VmImage where
    runConvert hnd _ () = do
        Just fileSys <- useArtifactState hnd
        runConvert (fileSys ^. fsImgH) SVmImage ()

instance CanConvert IoCompiler 'FileSystemImage 'VmImage where
    runConvert hnd _ () = do
        Just (FsCtx fH _) <- useArtifactState hnd
        fH' <- runConvert fH SFreeFile "Raw-image"
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH

instance CanConvert IoCompiler 'FreeFile 'PartitionedVmImage where
    runConvert hnd@(Handle _ hndT) _ () = do
        let partVmImgHndT = hndT ++ "-partitioned-vm-image"
        (partVmImgHnd,_) <- allocHandle SPartitionedVmImage partVmImgHndT
        file <- runConvert hnd SFreeFile "partitioned-vm-image"
        putArtifactState partVmImgHnd $ file
        hnd --> partVmImgHnd
        return partVmImgHnd

instance CanConvert IoCompiler 'FreeFile 'VmImage where
    runConvert hnd _ imgT = do
        newHnd <- runConvert hnd SFreeFile (printf "vm-image-%s" (show imgT))
        createVmImage newHnd imgT

instance CanConvert IoCompiler 'ImageRepository 'VmImage where
    runConvert _ _ sharedImgName = do
        (sharedImgInfo,cachedImage) <- lift (imageRepoLookup sharedImgName)
        imgH <- runCreate SExternalFile cachedImage
        imgCopyH <- runConvert imgH SFreeFile ()
        createVmImage imgCopyH (siImgType sharedImgInfo)

instance CanConvert IoCompiler 'LocalDirectory 'UpdateServerRoot where
    runConvert destDirH _ () = do
        (hnd,_) <- allocHandle SUpdateServerRoot "update-server-root"
        hnd --> destDirH
        putArtifactState hnd destDirH
        return hnd

instance CanConvert IoCompiler 'PartitionedVmImage 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ partSpec@(MBRPartition pIndex) = do
        let dest = hndT ++ "-partition-" ++ show pIndex
        Just (srcFileH :: Handle 'FreeFile) <- useArtifactState hnd
        Just (FileCtx srcFileName _) <- useArtifactState srcFileH
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction hnd (lift (extractPartition partSpec srcFileName destFile))
        return destH

instance CanConvert IoCompiler 'VmImage 'FileSystemImage where
    runConvert hnd _ () = do
        hnd' <- runConvert hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- useArtifactState hnd'
        runConvert srcFileH SFileSystemImage Ext4

instance CanConvert IoCompiler 'VmImage 'FreeFile where
    runConvert hnd _ () = do
        Just (VmImgCtx srcFileH _srcType) <- useArtifactState hnd
        return srcFileH

instance CanConvert IoCompiler 'VmImage 'VmImage where
    runConvert hnd _ (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        destImgFileH <-
            runConvert
                srcImgFileH
                SFreeFile
                (printf "resized-%d-%s" destSize (show destSizeU))
        Just (FileCtx destImgFile _) <- useArtifactState destImgFileH
        addAction
            hnd
            (lift (resizeVmImage destImgFile destSize destSizeU srcType))
        hnd --> destImgFileH
        createVmImage destImgFileH srcType
    runConvert hnd@(Handle _ hndT) _ (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- useArtifactState hnd
        srcFileCopy <- freeFileTempCopy srcImgFileH "conversion-src"
        (destImgFileH,destImgFile) <-
            createFreeFile (hndT ++ "-converted-to-" ++ show destType)
        addAction
            hnd
            (lift (convertVmImage srcFileCopy srcType destImgFile destType))
        hnd --> destImgFileH
        createVmImage destImgFileH destType

instance CanExport IoCompiler 'VmImage where
    runExport hnd@(Handle SVmImage _) destFile = do
        Just (VmImgCtx fH _) <- useArtifactState hnd
        runExport fH destFile

-- | Create a vm image entry in the context.
createVmImage :: Handle 'FreeFile -> ImageType -> IoCompiler (Handle 'VmImage)
createVmImage srcFileH vmt = do
    (hnd,_) <- allocHandle SVmImage ("vm-image-" ++ show vmt)
    putArtifactState hnd $ VmImgCtx srcFileH vmt
    srcFileH --> hnd
    return hnd

-- * Utilities

-- | Create a new unique handle and store it in the state.
allocHandle :: (SingKind ('KProxy :: KProxy k)
               ,Show (Demote (a :: k)))
               => Sing a
               -> String
               -> IoCompiler (Handle a, SomeHandle)
allocHandle sa str = do
    v <- addVertex
    let h = formatHandle v sa str
    h' <- storeHandle h v
    actions . at v ?=
        [lift (traceL "==[B9-EXEC-ARTIFACT]==============[" h "]")]
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
formatHandle :: (SingKind ('KProxy :: KProxy k)
                ,Show (Demote (a :: k)))
                => Vertex -> Sing a -> String -> Handle a
formatHandle v sa str =
    Handle
        sa
        (if str == ""
             then show v
             else str ++ "-" ++ show v)

-- | Add a dependency of one resource to another
(-->) :: Handle a -> Handle b -> IoCompiler ()
h --> h' = do
    Just v <- lookupVertex h
    Just v' <- lookupVertex h'
    dependencies <>= [(v, v')]

-- | Return the vertex of a handle.
lookupVertex :: Handle a -> IoCompiler (Maybe Vertex)
lookupVertex h = use (hToV . at (SomeHandle h))

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
    lift (runReaderT a ctx)
