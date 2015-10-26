-- | A /pure/ abstraction off the IO related actions available in B9. This is useful
-- to enable unit testing, OS-independence and debugging.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module B9.B9IO where

import           B9.Content
import           B9.DiskImages
import           Control.Monad.Free
import           Control.Monad.Trans.Writer.Lazy
import qualified Data.ByteString as B
import           System.FilePath
import           Text.Printf

-- | Programs representing imperative, /impure/ IO actions required by B9 to
-- create, convert and install VM images or cloud init disks.  Pure 'Action's
-- are combined to a free monad. This seperation from actually doing the IO and
-- modelling the IO actions as pure data enables unit testing and debugging.
type IoProgram = Free Action

-- | Execute an 'IoProgram' using a monadic interpretation function.
run :: Monad m => (forall a. Action a -> m a) -> IoProgram b -> m b
run = foldFree

-- | Pure commands for disk image creation and conversion, file
-- IO and libvirt lxc interaction.
data Action next
    = LogTrace String
               next
    | GetBuildDir (FilePath -> next)
    | GetBuildId (String -> next)
    | Copy FilePath
           FilePath
           next
    | CopyDir FilePath
              FilePath
              next
    | Move FilePath
           FilePath
           next
    | MkDir FilePath
            next
    | MkTemp FilePath
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
                       FileSystemCreation
                       [(FilePath, FileSpec)]
                       next
    deriving (Functor)

-- | Log a string, but only when trace logging is enabled, e.g. when
-- debugging
logTrace :: String -> IoProgram ()
logTrace str = liftF $ LogTrace str ()

-- | Get the (temporary) directory of the current b9 execution
getBuildDir :: IoProgram FilePath
getBuildDir = liftF $ GetBuildDir id

-- | Get a arbitrary random number selected when B9 starts, that serves as
-- unique id.
getBuildId :: IoProgram String
getBuildId = liftF $ GetBuildId id

-- | Copy a file
copy :: FilePath -> FilePath -> IoProgram ()
copy from to = liftF $ Copy from to ()

-- | Make a recursive copy of an existing directory.
copyDir :: FilePath -> FilePath -> IoProgram ()
copyDir src dst = liftF $ CopyDir src dst ()

-- | Move a file or a directory
move :: FilePath -> FilePath -> IoProgram ()
move from to = liftF $ Move from to ()

-- | Just like @mkdir -p@
mkDir :: FilePath -> IoProgram ()
mkDir d = liftF $ MkDir d ()

-- | Create a unique file path inside the build directory starting with a given
-- prefix and ending with a unique random token.
mkTemp :: FilePath -> IoProgram FilePath
mkTemp prefix = liftF $ MkTemp prefix id

-- | Combination of 'mkTemp' and 'mkDir'
mkTempDir :: FilePath -> IoProgram FilePath
mkTempDir prefix = do
    tmp <- mkTemp prefix
    mkDir tmp
    return tmp

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

-- |

-- | Testing support
dumpToStrings :: IoProgram a -> [String]
dumpToStrings = snd . runPureDump

dumpToResult :: IoProgram a -> a
dumpToResult = fst . runPureDump

runPureDump :: IoProgram a -> (a, [String])
runPureDump p = runWriter $ run dump p
  where
    dump :: Action a -> Writer [String] a
    dump (LogTrace s n) = do
        tell ["logTrace " ++ s]
        return n
    dump (GetBuildDir k) = do
        tell ["getBuildDir"]
        return (k "/BUILD")
    dump (GetBuildId n) = do
        tell ["getBuildId"]
        return (n "build-id-1234")
    dump (MkTemp prefix n) = do
        tell ["mkTemp " ++ prefix]
        return (n ("/BUILD" </> prefix ++ "-XXXX"))
    dump (MkDir d n) = do
        tell ["mkDir " ++ d]
        return n
    dump (CopyDir s d n) = do
        tell [printf "copyDir %s %s" s d]
        return n
    dump (Copy s d n) = do
        tell [printf "copy %s %s" s d]
        return n
    dump (Move s d n) = do
        tell [printf "move %s %s" s d]
        return n
    dump (GetParentDir f k) = do
        tell [printf "getParentDir %s" f]
        return (k (takeDirectory f))
    dump (GetRealPath "." k) = do
        tell [printf "getRealPath ."]
        return (k ("/cwd"))
    dump (GetRealPath f k) = do
        tell [printf "getRealPath %s" f]
        return (k ("/abs/path/" ++ f))
    dump (GetFileName f k) = do
        tell [printf "getFileName %s" f]
        return (k (takeFileName f))
    dump (RenderContentToFile f c e n) = do
        tell [printf "renderContentToFile %s %s %s" f (show c) (show e)]
        return n
