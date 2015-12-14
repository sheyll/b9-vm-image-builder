-- | Compile a 'ProgramT' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module B9.B9IO.DslCompiler where

import B9.B9IO
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

-- | A existential type for holding state for artifacts
data ArtifactState where
        ArtifactState ::
            Typeable (ArtifactCtx a)
            => Map (Handle a) (ArtifactCtx a) -> ArtifactState

data family ArtifactCtx (a :: k) :: *


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
    , _artifactStates :: Map String ArtifactState
    }

instance Default Ctx where
    def = Ctx 0 def def def def def def def def def def def def def def def [] def

-- | Context of a single cloud-init image, i.e. meta/user data content
data CiCtx = CiCtx
    { _metaDataH :: Handle 'GeneratedContent
    , _userDataH :: Handle 'GeneratedContent
    } deriving (Show)

instance Default CiCtx where
    def =
        CiCtx
            (globalHandle SGeneratedContent)
            (globalHandle SGeneratedContent)

-- | Context of a 'SLocalDirectory'
data DirCtx = DirCtx
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
    , _execOutMnt :: FilePath
    , _execOutDir :: FilePath
    , _execOutFiles :: [(FilePath,FilePath)]
    , _execEnvSpec :: ExecEnvSpec
    } deriving (Show)

instance Default ExecEnvCtx where
    def = ExecEnvCtx def def def def def def def def def

makeLenses ''Ctx
makeLenses ''CiCtx
makeLenses ''DirCtx
makeLenses ''FsBuilderCtx
makeLenses ''FsCtx
makeLenses ''FileCtx
makeLenses ''VmImgCtx
makeLenses ''ExecEnvCtx

putArtifactState
    :: Typeable (ArtifactCtx a)
    => Handle a -> ArtifactCtx a -> IoCompiler ()
putArtifactState hnd@(Handle s _) actx = do
    artifactStates . at (show (fromSing s)) ?= Map.empty
    artifactStates . at (show (fromSing s)) . at hnd ?= actx

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
  where
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

instance CanCreate IoCompiler 'CloudInit where
    runCreate _ iidPrefix = do
        buildId <- lift getBuildId
        (hnd@(Handle _ iid),_) <-
            allocHandle
                SCloudInit
                ("cloudinit-" ++ iidPrefix ++ "-" ++ buildId)
        mH <-
            runCreate
                SGeneratedContent
                ( Concat
                      [ FromString "#cloud-config\n"
                      , RenderYaml (ASTObj [("instance-id", ASTString iid)])]
                , iidPrefix ++ "-meta-data")
        hnd --> mH
        uH <-
            runCreate
                SGeneratedContent
                ( Concat [FromString "#cloud-config\n", RenderYaml (ASTObj [])]
                , iidPrefix ++ "-user-data")
        hnd --> uH
        ci . at hnd ?= CiCtx mH uH
        return hnd

instance CanCreate IoCompiler 'ExecutionEnvironment where
    runCreate _ e = do
        (hnd,_) <- allocHandle SExecutionEnvironment (e ^. execEnvTitle)
        incDir <- lift (mkTempDir "included-files")
        outDir <- lift (mkTempDir "output-files")
        buildId <- lift B9.B9IO.getBuildId
        let outMnt = outputFileContainerPath buildId
            incMnt = includedFileContainerPath buildId
        execEnvs . at hnd ?=
            (def &~
             do execEnvSpec .= e
                execIncDir .= incDir
                execOutDir .= outDir
                execOutMnt .= outMnt
                execBindMounts .=
                    [ SharedDirectoryRO incDir (MountPoint incMnt)
                    , SharedDirectory outDir (MountPoint outMnt)])
        addAction
            hnd
            (do Just es <- view (execEnvs . at hnd)
                let copyOutFileScript = foldMap cp (es ^. execOutFiles)
                      where
                        cp (guestFrom,hostOut) =
                            Run "cp" [guestFrom, toMntPath hostOut]
                        toMntPath = (es ^. execOutMnt </>) . takeFileName
                lift
                    (executeInEnv
                         (es ^. execEnvSpec)
                         (es ^. execScript <> copyOutFileScript)
                         (es ^. execBindMounts)
                         (es ^. execImages)))
        return hnd

instance CanCreate IoCompiler 'ExternalFile where
     runCreate _ fn = do
         (hnd,_) <- allocHandle SExternalFile (takeFileName fn)
         fn' <- lift (getRealPath fn)
         externalFiles . at hnd ?= fn'
         return hnd

instance CanCreate IoCompiler 'FileSystemBuilder where
     runCreate _ fsSpec@(FileSystemSpec t fsLabel _ _) = do
         let title =
                 show t ++ "-" ++
                 (if null fsLabel
                      then "image"
                      else fsLabel)
         (hnd,_) <- allocHandle SFileSystemBuilder fsLabel
         (tmpFileH,tmpFile) <- createFreeFile title
         hnd --> tmpFileH
         fH <- createFsImage tmpFileH t
         tmpDir <- lift (mkTempDir (title <.> "d"))
         fsBuilder . at hnd ?= FsBuilderCtx [] tmpDir fH
         addAction
             hnd
             (do Just fileSys <- view (fsBuilder . at hnd)
                 lift
                     (createFileSystem
                          tmpFile
                          fsSpec
                          tmpDir
                          (fileSys ^. fsFiles)))
         return hnd

instance CanCreate IoCompiler 'FreeFile where
    runCreate _ mTempName = do
        -- TODO escape tempName, allow only a-zA-Z0-9.-_:+=
        let tempName = maybe "tmp-file" takeFileName mTempName
        (hnd,_) <- createFreeFile tempName
        return hnd

instance CanCreate IoCompiler 'GeneratedContent where
    runCreate _ (c,title) = do
         (hnd,_) <- allocHandle SGeneratedContent title
         generatedContent . at hnd ?= c
         return hnd

instance CanCreate IoCompiler 'LocalDirectory where
    runCreate _ () = do
        tmp <- lift (mkTempDir "local-dir")
        (hnd,_) <- allocHandle SLocalDirectory tmp
        localDirs . at hnd ?= DirCtx tmp []
        addAction
            hnd
            (do Just (DirCtx src dests) <- view (localDirs . at hnd)
                case reverse dests of
                    [] -> lift (errorL hnd "not exported!")
                    (lastDest:firstDests) ->
                        lift
                            (do mapM_ (copyDir src) (reverse firstDests)
                                moveDir src lastDest))
        return hnd

instance CanAdd IoCompiler 'CloudInit 'CloudInitMetaData where
    runAdd hnd _ ast = do
        Just (CiCtx mH _) <- use (ci . at hnd)
        generatedContent . at mH . traverse %=
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'CloudInitUserData where
    runAdd hnd _ ast = do
        Just (CiCtx _ uH) <- use (ci . at hnd)
        generatedContent . at uH . traverse %=
            \(Concat [hdr,RenderYaml ast']) ->
                 Concat [hdr, RenderYaml (ast' `astMerge` ast)]

instance CanAdd IoCompiler 'CloudInit 'ExecutableScript where
    runAdd hnd _ scr =
        runAdd hnd SCloudInitUserData (toUserDataRunCmdAST scr)

instance CanAdd IoCompiler 'CloudInit 'FreeFile where
    runAdd hnd _ (fspec,fH) = do
        fH --> hnd
        fName <- freeFileTempCopy fH (takeFileName (fspec ^. fileSpecPath))
        runAdd
            hnd
            SCloudInitUserData
            (toUserDataWriteFilesAST fspec (FromBinaryFile fName))

instance CanAdd IoCompiler 'ExecutionEnvironment 'ExecutableScript where
    runAdd hnd _ cmds =
        (execEnvs . at hnd . traverse . execScript) <>= cmds

instance CanAdd IoCompiler 'ExecutionEnvironment 'FreeFile where
    runAdd hnd _ (destSpec,srcH) = do
        srcH --> hnd
        Just eCxt <- use (execEnvs . at hnd)
        incFile <-
            lift (mkTempInCreateParents (eCxt ^. execIncDir) "added-file")
        copyFreeFile srcH incFile
        execEnvs . at hnd . traverse . execIncFiles <>= [(incFile, destSpec)]
        bId <- lift B9.B9IO.getBuildId
        execEnvs . at hnd . traverse . execScript <>=
            incFileScript bId incFile destSpec

instance CanAdd IoCompiler 'ExecutionEnvironment 'LocalDirectory where
    runAdd hnd _ sharedDir =
        execEnvs . at hnd . traverse . execBindMounts <>= [sharedDir]

instance CanAdd IoCompiler 'FileSystemBuilder 'FreeFile where
    runAdd fsH _ (fSpec,fH) = do
        fsBuilder . at fsH . traverse . fsFiles <>= [fSpec]
        Just fileSys <- use (fsBuilder . at fsH)
        let tmpDir = fileSys ^. fsTempDir
        fH --> fsH
        copyFreeFile' fH tmpDir fSpec

instance CanAdd IoCompiler 'ImageRepository 'VmImage where
    runAdd _ _ (sn,vmI) = do
        Just (VmImgCtx imgFileH srcType) <- use (vmImages . at vmI)
        let SharedImageName snStr = sn
        imgFile <- freeFileTempCopy imgFileH snStr
        vmI --> imageRepositoryH
        addAction imageRepositoryH (lift (imageRepoPublish imgFile srcType sn))

instance CanAdd IoCompiler 'LocalDirectory 'FreeFile where
    runAdd dirH _ (fSpec,fH) = do
        Just localDir <- use (localDirs . at dirH)
        copyFreeFile' fH (localDir ^. dirTempDir) fSpec
        fH --> dirH

instance CanAdd IoCompiler 'LoggingOutput 'LogEvent where
    runAdd _ _ (lvl,msg) = lift $ logMsg lvl msg

instance CanAdd IoCompiler 'UpdateServerRoot 'VmImage where
    runAdd hnd _ (sn,vmI) = do
        Just destDirH <- use (updateServerRoots . at hnd)
        Just tmpDirCtx <- use (localDirs . at destDirH)
        let destDir = tmpDirCtx ^. dirTempDir
            vmDestDir = destDir </> "machines" </> snStr </> "disks" </> "raw"
            SharedImageName snStr = sn
        Just (VmImgCtx srcFileH srcType) <- use (vmImages . at vmI)
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

instance CanAdd IoCompiler 'VariableBindings 'TemplateVariable where
    runAdd _ _ (k,v) = vars . at k ?= v

instance CanConvert IoCompiler 'CloudInit 'CloudInitMetaData where
    runConvert hnd _ () = do
        Just (CiCtx (Handle SGeneratedContent h) _) <- use (ci . at hnd)
        return (Handle SCloudInitMetaData h)

instance CanConvert IoCompiler 'CloudInit 'CloudInitUserData where
    runConvert hnd _ () = do
        Just (CiCtx _ (Handle SGeneratedContent h)) <- use (ci . at hnd)
        return (Handle SCloudInitUserData h)

instance CanConvert IoCompiler 'CloudInitMetaData 'GeneratedContent where
    runConvert (Handle _ h) _ () =
        return (Handle SGeneratedContent h)

instance CanConvert IoCompiler 'CloudInitUserData 'GeneratedContent where
    runConvert (Handle _ h) _ () =
        return (Handle SGeneratedContent h)

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
        Just (FileCtx mnt _) <- use (localFiles . at mntH)
        (execEnvs . at hnd . traverse . execImages) <>=
            [(Image mnt Raw Ext4, mp)]
        hnd --> mntH
        runConvert mntH SVmImage Raw

instance CanConvert IoCompiler 'ExecutionEnvironment 'FreeFile where
    runConvert hnd _ src = do
        Just ec <- use (execEnvs . at hnd)
        (fh,f) <-
            createFreeFileIn
                (ec ^. execOutDir)
                (printf
                     "%s-%s"
                     (ec ^. execEnvSpec . execEnvTitle)
                     (takeFileName src))
        execEnvs . at hnd . traverse . execOutFiles <>= [(src, f)]
        hnd --> fh
        return fh

instance CanConvert IoCompiler 'ExternalFile 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ () = do
        Just externalFileName <- use (externalFiles . at hnd)
        (tmpFileH,tmpFile) <- createFreeFile (hndT ++ "-copy")
        hnd --> tmpFileH
        addAction hnd (lift (copy externalFileName tmpFile))
        return tmpFileH

instance CanConvert IoCompiler 'FileSystemBuilder 'FileSystemImage where
    runConvert hnd _ () = do
        Just fileSys <- use (fsBuilder . at hnd)
        return (fileSys ^. fsImgH)

instance CanConvert IoCompiler 'FileSystemBuilder 'FreeFile where
    runConvert hnd _ () = do
        Just fileSys <- use (fsBuilder . at hnd)
        runConvert (fileSys ^. fsImgH) SFreeFile ()

instance CanConvert IoCompiler 'FileSystemBuilder 'VmImage where
    runConvert hnd _ () = do
        Just fileSys <- use (fsBuilder . at hnd)
        runConvert (fileSys ^. fsImgH) SVmImage ()

instance CanConvert IoCompiler 'FileSystemImage 'FileSystemImage where
    runConvert hnd _ destSize = do
        Just (FsCtx inFileH fS) <- use (fsImages . at hnd)
        outFileH <- runConvert inFileH SFreeFile "resized"
        Just (FileCtx outFile _) <- use (localFiles . at outFileH)
        inFileH --> hnd
        hnd --> outFileH
        addAction hnd (lift (resizeFileSystem outFile destSize fS))
        createFsImage outFileH fS

instance CanConvert IoCompiler 'FileSystemImage 'FreeFile where
    runConvert hnd _ () = do
        Just (FsCtx fH _fS) <- use (fsImages . at hnd)
        return fH

instance CanConvert IoCompiler 'FileSystemImage 'VmImage where
    runConvert hnd _ () = do
        Just (FsCtx fH _) <- use (fsImages . at hnd)
        fH' <- runConvert fH SFreeFile "Raw-image"
        outH <- createVmImage fH' Raw
        hnd --> outH
        return outH

instance CanConvert IoCompiler 'FreeFile 'ExternalFile where
    runConvert hnd _ dest = do
        dest' <- lift (ensureParentDir dest)
        newFileH <- runCreate SExternalFile dest'
        hnd --> newFileH
        copyFreeFile hnd dest'
        return newFileH

instance CanConvert IoCompiler 'FreeFile 'FileSystemImage where
    runConvert hnd _ fs = do
        copyH <- runConvert hnd SFreeFile (show fs)
        fsImg <- createFsImage copyH fs
        copyH --> fsImg
        return fsImg

instance CanConvert IoCompiler 'FreeFile 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ dest = do
        (newFileH,newFile) <- createFreeFile (hndT ++ "-" ++ dest)
        copyFreeFile hnd newFile
        hnd --> newFileH
        return newFileH

instance CanConvert IoCompiler 'FreeFile 'PartitionedVmImage where
    runConvert hnd@(Handle _ hndT) _ () = do
        let partVmImgHndT = hndT ++ "-partitioned-vm-image"
        (partVmImgHnd,_) <- allocHandle SPartitionedVmImage partVmImgHndT
        file <- runConvert hnd SFreeFile "partitioned-vm-image"
        partitionedImgs . at partVmImgHnd ?= file
        hnd --> partVmImgHnd
        return partVmImgHnd

instance CanConvert IoCompiler 'FreeFile 'VmImage where
    runConvert hnd _ imgT = do
        newHnd <- runConvert hnd SFreeFile (printf "vm-image-%s" (show imgT))
        createVmImage newHnd imgT

instance CanConvert IoCompiler 'GeneratedContent 'FreeFile where
    runConvert hnd@(Handle _ dest) _ () = do
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction
            hnd
            (do Just content <- view (generatedContent . at hnd)
                env <- view (vars . to Map.toList . to Environment)
                lift (renderContentToFile destFile content env))
        return destH

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
        updateServerRoots . at hnd ?= destDirH
        return hnd

instance CanConvert IoCompiler 'PartitionedVmImage 'FreeFile where
    runConvert hnd@(Handle _ hndT) _ partSpec@(MBRPartition pIndex) = do
        let dest = hndT ++ "-partition-" ++ show pIndex
        Just srcFileH <- use (partitionedImgs . at hnd)
        Just (FileCtx srcFileName _) <- use (localFiles . at srcFileH)
        (destH,destFile) <- createFreeFile dest
        hnd --> destH
        addAction hnd (lift (extractPartition partSpec srcFileName destFile))
        return destH

instance CanConvert IoCompiler 'VmImage 'FileSystemImage where
    runConvert hnd _ () = do
        hnd' <- runConvert hnd SVmImage (Left Raw)
        Just (VmImgCtx srcFileH Raw) <- use (vmImages . at hnd')
        runConvert srcFileH SFileSystemImage Ext4

instance CanConvert IoCompiler 'VmImage 'FreeFile where
    runConvert hnd _ () = do
        Just (VmImgCtx srcFileH _srcType) <- use (vmImages . at hnd)
        return srcFileH

instance CanConvert IoCompiler 'VmImage 'VmImage where
    runConvert hnd _ (Right (ImageSize destSize destSizeU)) = do
        Just (VmImgCtx srcImgFileH srcType) <- use (vmImages . at hnd)
        destImgFileH <-
            runConvert
                srcImgFileH
                SFreeFile
                (printf "resized-%d-%s" destSize (show destSizeU))
        Just (FileCtx destImgFile _) <- use (localFiles . at destImgFileH)
        addAction
            hnd
            (lift (resizeVmImage destImgFile destSize destSizeU srcType))
        hnd --> destImgFileH
        createVmImage destImgFileH srcType
    runConvert hnd@(Handle SVmImage hndT) _ (Left destType) = do
        Just (VmImgCtx srcImgFileH srcType) <- use (vmImages . at hnd)
        srcFileCopy <- freeFileTempCopy srcImgFileH "conversion-src"
        (destImgFileH,destImgFile) <-
            createFreeFile (hndT ++ "-converted-to-" ++ show destType)
        addAction
            hnd
            (lift (convertVmImage srcFileCopy srcType destImgFile destType))
        hnd --> destImgFileH
        createVmImage destImgFileH destType

instance CanExport IoCompiler 'FileSystemImage where
    runExport hnd@(Handle SFileSystemImage _) destFile = do
        Just fileSys <- use (fsImages . at hnd)
        runExport (fileSys ^. fsFileH) destFile

instance CanExport IoCompiler 'FreeFile where
    runExport hnd@(Handle SFreeFile _) destFile =
        lift (ensureParentDir destFile) >>= copyFreeFile hnd

instance CanExport IoCompiler 'LocalDirectory where
    runExport hnd@(Handle SLocalDirectory _) destDir = do
        destDir' <- lift (ensureParentDir destDir)
        localDirs . at hnd . traverse . dirExports <>= [destDir']

instance CanExport IoCompiler 'VmImage where
    runExport hnd@(Handle SVmImage _) destFile = do
        Just (VmImgCtx fH _) <- use (vmImages . at hnd)
        runExport fH destFile

-- | Create and allocate a new 'FreeFile' and return the handle as well as the
-- path to the temporary file.
createFreeFile :: String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFile title = do
    src <- lift (mkTempCreateParents title)
    hnd <- asFreeFile src title
    return (hnd, src)

-- | Create and allocate a new 'FreeFile' inside a given directory and
-- return the handle as well as the path to the temporary file.
createFreeFileIn :: FilePath -> String -> IoCompiler (Handle 'FreeFile, FilePath)
createFreeFileIn parent title = do
    src <- lift (mkTempIn parent title)
    hnd <- asFreeFile src title
    return (hnd, src)

-- | Allocate a 'FreeFile' artifact for a given file with a given title.
asFreeFile :: FilePath -> String -> IoCompiler (Handle 'FreeFile)
asFreeFile src title = do
    (hnd,_) <- allocHandle SFreeFile title
    localFiles . at hnd ?=
        FileCtx src []
    addAction
        hnd
        (do Just (FileCtx _ destinations) <-
                view (localFiles . at hnd)
            lift
                (case reverse destinations of
                     (lastCopy:firstCopies) -> do
                         mapM_
                             (copy src)
                             (reverse firstCopies)
                         moveFile src lastCopy
                     [] -> dbgL "No copies of" src "required"))
    return hnd

-- | Add a new copy to a 'FreeFile' at the specified destination
copyFreeFile :: Handle 'FreeFile -> FilePath -> IoCompiler ()
copyFreeFile src dest = localFiles . at src . traverse . fCopies <>= [dest]

-- | Add a new copy to a 'FreeFile' using a unique temp file containg
-- a given string for better debugging, and return the path to the copy.
freeFileTempCopy :: Handle 'FreeFile -> String -> IoCompiler FilePath
freeFileTempCopy src name = do
    Just fileCtx <- use (localFiles . at src)
    dest <-
        lift
            (mkTempCreateParents
                 (printf
                      "%s-%s"
                      (takeFileName (fileCtx ^. fFileName))
                      (takeFileName name)))
    copyFreeFile src dest
    return dest

-- | Add a new copy to a 'FreeFile' at the
--   specified destination which is conveniently derived from path component of
--   a 'FileSpec' and a directory.
copyFreeFile' :: Handle 'FreeFile -> FilePath -> FileSpec -> IoCompiler ()
copyFreeFile' src dstDir dstSpec =
    copyFreeFile src (dstDir </> (dstSpec ^. fileSpecPath))

-- | Create a 'FsCtx' from an existing file and the file system type.
createFsImage :: Handle 'FreeFile -> FileSystem -> IoCompiler (Handle 'FileSystemImage)
createFsImage fH fs = do
    (hnd,_) <- allocHandle SFileSystemImage ( "fs-img-" ++ show fs)
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

-- | Return the mount point for files from the build host to be
-- included in the container.
includedFileContainerPath :: String -> FilePath
includedFileContainerPath buildId =
    "/" ++ buildId <.> "mnt" </> "included-files"

-- | Return the mount point for files that are copied after the build from the container
--   to the host.
outputFileContainerPath :: String -> FilePath
outputFileContainerPath buildId =
    "/" ++ buildId <.> "mnt" </> "output-files"

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
