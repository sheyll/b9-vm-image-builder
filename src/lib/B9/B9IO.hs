-- | A /pure/ abstraction off the IO related actions available in B9. This is useful
-- to enable unit testing, OS-independence and debugging.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module B9.B9IO
       (IoProgram, MonadIoProgram(..), runB9IO, Action(..), getBuildDir,
        getBuildId, getBuildDate, copy, copyDir, moveFile, moveDir, mkDir,
        readFileSize, mkTemp, mkTempCreateParents, mkTempIn,
        mkTempInCreateParents, mkTempDir, getRealPath, getParentDir,
        getFileName, ensureParentDir, renderContentToFile,
        createFileSystem, resizeFileSystem, convertVmImage, resizeVmImage,
        extractPartition, imageRepoLookup, imageRepoPublish, executeInEnv,
        traceEveryAction, dumpToStrings, dumpToResult, runPureDump,
        arbitraryIoProgram)
       where

import B9.CommonTypes
import B9.Content
import B9.DiskImages
import B9.ExecEnv
import B9.FileSystems
import B9.Logging
import B9.PartitionTable
import B9.QCUtil
import B9.Repository
import B9.ShellScript
import Control.Monad.Free
import Control.Monad.Trans.Writer.Lazy
import System.FilePath
import Test.QuickCheck
import Text.Printf

-- | Programs representing imperative, /impure/ IO actions required by B9 to
-- create, convert and install VM images or cloud init disks.  Pure 'Action's
-- are combined to a free monad. This seperation from actually doing the IO and
-- modelling the IO actions as pure data enables unit testing and debugging.
newtype IoProgram a = IoProgram
    { runIoProgram :: Free Action a
    } deriving (Functor,Applicative,Monad,MonadFree Action)

-- | Class of monads that can contain 'IoProgram's.
class MonadIoProgram m where
    liftIoProgram :: IoProgram a -> m a

-- | Execute an 'IoProgram' using a monadic interpretation function.
runB9IO
    :: Monad m
    => (forall a. Action a -> m a) -> IoProgram b -> m b
runB9IO a = foldFree a . runIoProgram

-- | Pure commands for disk image creation and conversion, file
-- IO and libvirt lxc interaction.
data Action next
    = LogMessage LogLevel
                 String
                 next
    | GetBuildDir (FilePath -> next)
    | GetBuildId (String -> next)
    | GetBuildDate (String -> next)
    | Copy FilePath
           FilePath
           next
    | CopyDir FilePath
              FilePath
              next
    | MoveFile FilePath
               FilePath
               next
    | MoveDir FilePath
              FilePath
              next
    | ReadFileSize FilePath
                   (Integer -> next)
    | MkDir FilePath
            next
    | MkTemp FilePath
             (FilePath -> next)
    | MkTempIn FilePath
               FilePath
               (FilePath -> next)
    | GetRealPath FilePath
                  (FilePath -> next)
    | GetParentDir FilePath
                   (FilePath -> next)
    | GetFileName FilePath
                  (FilePath -> next)
    | RenderContentToFile FilePath
                          Content
                          Environment
                          next
    | CreateFileSystem FilePath
                       FileSystemSpec
                       FilePath
                       [FileSpec]
                       next
    | ResizeFileSystem FilePath
                       FileSystemResize
                       FileSystem
                       next
    | ConvertVmImage FilePath
                     ImageType
                     FilePath
                     ImageType
                     next
    | ResizeVmImage FilePath
                    Int
                    SizeUnit
                    ImageType
                    next
    | ExtractPartition PartitionSpec
                       FilePath
                       FilePath
                       next
    | ImageRepoLookup SharedImageName
                      ((SharedImage, FilePath) -> next)
    | ImageRepoPublish FilePath
                       ImageType
                       SharedImageName
                       next
    | ExecuteInEnv ExecEnvSpec
                   Script
                   [SharedDirectory]
                   [Mounted Image]
                   next
    deriving (Functor)

instance Show (Action a) where
    show (LogMessage l m _) = printf "logMessage %s %s" (show l) (show m)
    show (GetBuildId _) = "getBuildId"
    show (GetBuildDate _) = "getBuildDate"
    show (GetBuildDir _) = "getBuildDir"
    show (Copy s d _) = printf "copy %s %s" s d
    show (CopyDir s d _) = printf "copyDir %s %s" s d
    show (MoveFile s d _) = printf "moveFile %s %s" s d
    show (MoveDir s d _) = printf "moveDir %s %s" s d
    show (ReadFileSize f _) = printf "readFileSize %s" f
    show (MkDir d _) = printf "mkDir %s" d
    show (MkTempIn d p _) = printf "mkTempIn %s %s" d p
    show (MkTemp p _) = printf "mkTemp %s" p
    show (GetRealPath p _) = printf "getRealPath %s" p
    show (GetParentDir p _) = printf "getParentDir %s" p
    show (GetFileName p _) = printf "getFileName %s" p
    show (RenderContentToFile f c e _) =
        printf "renderContentToFile %s %s %s" f (show c) (show e)
    show (CreateFileSystem dst fs srcDir files _) =
        printf "createFileSystem %s %s %s %s" dst (show fs) srcDir (show files)
    show (ResizeFileSystem fs fsResize fsType _) =
        printf "resizeFileSystem %s %s %s" fs (show fsResize) (show fsType)
    show (ConvertVmImage srcF srcT dstF dstT _) =
        printf "convertVmImage %s %s %s %s" srcF (show srcT) dstF (show dstT)
    show (ResizeVmImage img s u imgT _) =
        printf "resizeVmImage %s %d %s %s" img s (show u) (show imgT)
    show (ExtractPartition p s d _) =
        printf "extractPartition %s %s %s" (show p) s d
    show (ImageRepoLookup sn _) = printf "imageRepoLookup %s" (show sn)
    show (ImageRepoPublish f t n _) =
        printf "imageRepoPublish %s %s %s" f (show t) (show n)
    show (ExecuteInEnv e s d i _) =
        printf "executeInEnv %s %s %s %s" (show e) (show s) (show d) (show i)

instance LogArg (Action a)

-- | High-level logging API
instance (a ~ ()) => CanLog (IoProgram a) where
    logMsg l str = liftF $ LogMessage l str ()

-- | Get the (temporary) directory of the current b9 execution
getBuildDir :: IoProgram FilePath
getBuildDir = liftF $ GetBuildDir id

-- | Get a arbitrary random number selected when B9 starts, that serves as
-- unique id.
getBuildId :: IoProgram String
getBuildId = liftF $ GetBuildId id

-- | Get the system time at the start of the overall build formatted string,
-- such that the lexicographical order of the strings is the same as the order
-- of the correspondig time stamps.
getBuildDate :: IoProgram String
getBuildDate = liftF $ GetBuildDate id

-- | Copy a file
copy :: FilePath -> FilePath -> IoProgram ()
copy from to = liftF $ Copy from to ()

-- | Copy a directory recursively
copyDir :: FilePath -> FilePath -> IoProgram ()
copyDir from to = liftF $ CopyDir from to ()

-- | Move a file
moveFile :: FilePath -> FilePath -> IoProgram ()
moveFile from to = liftF $ MoveFile from to ()

-- | Move a directory
moveDir :: FilePath -> FilePath -> IoProgram ()
moveDir from to = liftF $ MoveDir from to ()

-- | Just like @mkdir -p@
mkDir :: FilePath -> IoProgram ()
mkDir d = liftF $ MkDir d ()

-- | Get the size of the contents of a file in bytes.
readFileSize :: FilePath -> IoProgram Integer
readFileSize f = liftF $ ReadFileSize f id

-- | Create a unique file path inside the build directory starting with a given
-- prefix and ending with a unique random token.
mkTemp :: FilePath -> IoProgram FilePath
mkTemp prefix = liftF $ MkTemp prefix id

-- | Like 'mkTemp' but also create the parent directory of the tempfile and
-- return an absolute path.
mkTempCreateParents :: FilePath -> IoProgram FilePath
mkTempCreateParents prefix = mkTemp prefix >>= ensureParentDir

-- | Create a unique file path inside a given directory starting with a given
-- prefix and ending with a unique random token.
mkTempIn :: FilePath -> FilePath -> IoProgram FilePath
mkTempIn parent prefix = liftF $ MkTempIn parent prefix id

-- | Like 'mkTempIn' but also create the parent directory of the tempfile and
-- return an absolute path.
mkTempInCreateParents :: FilePath -> FilePath -> IoProgram FilePath
mkTempInCreateParents parent prefix =
    mkTempIn parent prefix >>= ensureParentDir

-- | Combination of 'mkTemp' and 'mkDir'
mkTempDir :: FilePath -> IoProgram FilePath
mkTempDir prefix = do
    tmp <- mkTemp prefix
    mkDir tmp
    getRealPath tmp

-- | Return the canonical path of a relative path. NOTE: The file should exist!
getRealPath :: FilePath -> IoProgram FilePath
getRealPath d = liftF $ GetRealPath d id

-- | Return the parent directory of a file (or directory)
getParentDir :: FilePath -> IoProgram FilePath
getParentDir d = liftF $ GetParentDir d id

-- | Return the filename (last part) of a path.
getFileName :: FilePath -> IoProgram FilePath
getFileName f = liftF $ GetFileName f id

-- | Extract the parent directory from a path, create it if it does not exist
-- and return the canonical path
ensureParentDir :: FilePath -> IoProgram FilePath
ensureParentDir path = do
    parentDir <- getParentDir path
    mkDir parentDir
    parentDirAbs <- getRealPath parentDir
    file <- getFileName path
    return (parentDirAbs </> file)

-- | Render a given content to a file. Implementations should overwrite the file
-- if it exists.
renderContentToFile :: FilePath -> Content -> Environment -> IoProgram ()
renderContentToFile f c e = liftF $ RenderContentToFile f c e ()

-- | Create a 'FileSystem' inside a file, such that the criteria in a
-- 'FileSystemSpec' record are matched and all files listed in the third
-- parameter are copied into the file system.
createFileSystem :: FilePath
                 -> FileSystemSpec
                 -> FilePath
                 -> [FileSpec]
                 -> IoProgram ()
createFileSystem dst fs srcDir files =
    liftF $ CreateFileSystem dst fs srcDir files ()

-- | Resize a file system in a raw disk image.
resizeFileSystem :: FilePath -> FileSystemResize -> FileSystem -> IoProgram ()
resizeFileSystem f r t = liftF $ ResizeFileSystem f r t ()

-- | Convert a virtual machine disk image file into another format.
convertVmImage :: FilePath -> ImageType -> FilePath -> ImageType -> IoProgram ()
convertVmImage srcF srcT dstF dstT = liftF $ ConvertVmImage srcF srcT dstF dstT ()

-- | Resize a virtual machine disk image.
resizeVmImage :: FilePath -> Int -> SizeUnit -> ImageType -> IoProgram ()
resizeVmImage i s u t = liftF $ ResizeVmImage i s u t ()

-- | Extract a partition from a partitioned disk image file and copy it as raw
-- image into a new file.
extractPartition :: PartitionSpec -> FilePath -> FilePath -> IoProgram ()
extractPartition p s d = liftF $ ExtractPartition p s d ()

-- | Lookup the latest 'SharedImage' by with a given name.
imageRepoLookup :: SharedImageName -> IoProgram (SharedImage, FilePath)
imageRepoLookup sn =
  liftF $ ImageRepoLookup sn id

-- | Store a local vm image file in the image repository
imageRepoPublish :: FilePath
                 -> ImageType
                 -> SharedImageName
                 -> IoProgram ()
imageRepoPublish f t n = liftF $ ImageRepoPublish f t n ()

-- * Container execution

-- | Run a 'Script' in an isolated environment specified by an 'ExecEnvSpec'.
executeInEnv :: ExecEnvSpec
             -> Script
             -> [SharedDirectory]
             -> [Mounted Image]
             -> IoProgram ()
executeInEnv e s d i = liftF $ ExecuteInEnv e s d i ()

-- * Program transformation

-- | Wrap a interpreter for a 'IoProgram' such that all invokations except for
-- 'LogMessage' are logged via 'LogMessage'.
traceEveryAction :: IoProgram a -> IoProgram a
traceEveryAction = runB9IO traceAction
  where
    traceAction (LogMessage l s n) = do
        logMsg l s
        return n
    traceAction a@(GetBuildDir k) = do
        traceL a
        b <- getBuildDir
        traceL "->" b
        return $ k b
    traceAction a@(GetBuildId k) = do
        traceL a
        b <- getBuildId
        traceL "->" b
        return $ k b
    traceAction a@(GetBuildDate k) = do
        traceL a
        b <- getBuildDate
        traceL "->" b
        return $ k b
    traceAction a@(MkTemp prefix k) = do
        dbgL a
        t <- mkTemp prefix
        traceL "->" t
        return $ k t
    traceAction a@(MkTempIn parent prefix k) = do
        dbgL a
        t <- mkTempIn parent prefix
        traceL "->" t
        return $ k t
    traceAction a@(MkDir d n) = do
        dbgL a
        mkDir d
        return n
    traceAction a@(Copy s d n) = do
        dbgL a
        copy s d
        return n
    traceAction a@(CopyDir s d n) = do
        dbgL a
        copyDir s d
        return n
    traceAction a@(MoveFile s d n) = do
        dbgL a
        moveFile s d
        return n
    traceAction a@(MoveDir s d n) = do
        dbgL a
        moveDir s d
        return n
    traceAction a@(ReadFileSize f k) = do
        traceL a
        s <- readFileSize f
        traceL "->" s
        return $ k s
    traceAction a@(GetParentDir f k) = do
        traceL a
        p <- getParentDir f
        traceL "->" p
        return $ k p
    traceAction a@(GetRealPath f k) = do
        traceL a
        p <- getRealPath f
        traceL "->" p
        return $ k p
    traceAction a@(GetFileName f k) = do
        traceL a
        p <- getFileName f
        traceL "->" p
        return $ k p
    traceAction a@(RenderContentToFile f c e n) = do
        traceL a
        renderContentToFile f c e
        return n
    traceAction a@(CreateFileSystem dst fs srcDir files n) = do
        dbgL a
        createFileSystem dst fs srcDir files
        return n
    traceAction a@(ResizeFileSystem f r t n) = do
        dbgL a
        resizeFileSystem f r t
        return n
    traceAction a@(ConvertVmImage srcF srcT dstF dstT n) = do
        dbgL a
        convertVmImage srcF srcT dstF dstT
        return n
    traceAction a@(ResizeVmImage i s u t n) = do
        dbgL a
        resizeVmImage i s u t
        return n
    traceAction a@(ExtractPartition p s d n) = do
        dbgL a
        extractPartition p s d
        return n
    traceAction a@(ImageRepoLookup s k) = do
        dbgL a
        r <- imageRepoLookup s
        traceL "->" r
        return $ k r
    traceAction a@(ImageRepoPublish f t sn n) = do
        dbgL a
        imageRepoPublish f t sn
        return n
    traceAction a@(ExecuteInEnv e s d i n) = do
        dbgL a
        executeInEnv e s d i
        return n

-- * Testing support

-- | Run a program without any I/O
--   and return a list of strings, each list element is a textual representation
--   of the command and its parameters. This is useful for testing and
--   inspection.
dumpToStrings :: IoProgram a -> [String]
dumpToStrings = snd . runPureDump

-- | Run a program without any I/O
--   and return a the return value of the program. This is useful for testing
--   and inspection.
dumpToResult :: IoProgram a -> a
dumpToResult = fst . runPureDump

-- | Run a program without any I/O using a simple writer
--   monad where the output is a list of strings each representing an action of
--   the program and its paraters.  This is useful for testing and inspection.
runPureDump :: IoProgram a -> (a, [String])
runPureDump p = runWriter $ runB9IO dump p
  where
    dump :: Action a -> Writer [String] a
    dump a@(LogMessage _l _s n) = do
        tell [show a]
        return n
    dump a@(GetBuildDir k) = do
        tell [show a]
        return (k "/BUILD")
    dump a@(GetBuildId n) = do
        tell [show a]
        return (n "build-id-1234")
    dump a@(GetBuildDate n) = do
        tell [show a]
        return (n "1970-01-01 00:00:00")
    dump a@(MkTemp prefix n) = do
        tell [show a]
        return (n ("/BUILD" </> prefix ++ "-XXXX"))
    dump a@(MkTempIn parent prefix n) = do
        tell [show a]
        return (n (parent </> prefix ++ "-XXXX"))
    dump a@(MkDir _d n) = do
        tell [show a]
        return n
    dump a@(Copy _s _d n) = do
        tell [show a]
        return n
    dump a@(CopyDir _s _d n) = do
        tell [show a]
        return n
    dump a@(MoveFile _s _d n) = do
        tell [show a]
        return n
    dump a@(MoveDir _s _d n) = do
        tell [show a]
        return n
    dump a@(ReadFileSize _f n) = do
        tell [show a]
        return (n 1234)
    dump a@(GetParentDir f k) = do
        tell [show a]
        return (k (takeDirectory f))
    dump a@(GetRealPath "." k) = do
        tell [show a]
        return (k "/cwd")
    dump a@(GetRealPath f k) = do
        tell [show a]
        return (k ("/abs/path/" ++ f))
    dump a@(GetFileName f k) = do
        tell [show a]
        return (k (takeFileName f))
    dump a@(RenderContentToFile _f _c _e n) = do
        tell [show a]
        return n
    dump a@(CreateFileSystem _f _c _d _fs n) = do
        tell [show a]
        return n
    dump a@(ResizeFileSystem _f _r _t n) = do
        tell [show a]
        return n
    dump a@(ConvertVmImage _srcF _srcT _dstF _dstT n) = do
        tell [show a]
        return n
    dump a@(ResizeVmImage _i _s _u _t n) = do
        tell [show a]
        return n
    dump a@(ExtractPartition _p _s _d n) = do
        tell [show a]
        return n
    dump a@(ImageRepoLookup s k) = do
        tell [show a]
        return $
            k
                ( SharedImage
                      s
                      (SharedImageDate "01-01-1970")
                      (SharedImageBuildId "00000000")
                      QCow2
                      Ext4
                , "~/.b9/cache/xxx.qcow2")
    dump a@(ImageRepoPublish _f _t _sn n) = do
        tell [show a]
        return n
    dump a@(ExecuteInEnv _e _s _d _i n) = do
        tell [show a]
        return n

arbitraryIoProgram :: Gen (IoProgram ())
arbitraryIoProgram = IoProgram <$> arbitraryFree

instance Arbitrary a => Arbitrary (Action a) where
    arbitrary =
        oneof
            [ LogMessage LogTrace <$> smaller arbitraryNiceString <*>
              smaller arbitrary
            , GetBuildId <$> arbitrary
            , GetBuildDir <$> arbitrary
            , GetBuildDate <$> arbitrary
            , Copy <$> smaller arbitraryFilePath <*> smaller arbitraryFilePath <*>
              smaller arbitrary
            , CopyDir <$> smaller arbitraryFilePath <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary
            , MoveFile <$> smaller arbitraryFilePath <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary
            , MoveDir <$> smaller arbitraryFilePath <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary
            , ReadFileSize <$> smaller arbitraryFilePath <*> smaller arbitrary
            , MkDir <$> smaller arbitraryFilePath <*> smaller arbitrary
            , MkTemp <$> smaller arbitraryFilePath <*> smaller arbitrary
            , MkTempIn <$> smaller arbitraryFilePath <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary
            , GetRealPath <$> smaller arbitraryFilePath <*> smaller arbitrary
            , GetParentDir <$> smaller arbitraryFilePath <*> smaller arbitrary
            , GetFileName <$> smaller arbitraryFilePath <*> smaller arbitrary
            , RenderContentToFile <$> smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary
            , CreateFileSystem <$> smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitrary
            , ResizeFileSystem <$> smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary
            , ConvertVmImage <$> smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitrary
            , ResizeVmImage <$> smaller arbitraryFilePath <*> smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary
            , ExtractPartition <$> (MBRPartition <$> smaller arbitrary) <*>
              smaller arbitraryFilePath <*>
              smaller arbitraryFilePath <*>
              smaller arbitrary
            , ImageRepoLookup <$> smaller arbitrary <*> smaller arbitrary
            , ImageRepoPublish <$> smaller arbitraryFilePath <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary
            , ExecuteInEnv <$> smaller arbitrary <*> smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              smaller arbitrary]
