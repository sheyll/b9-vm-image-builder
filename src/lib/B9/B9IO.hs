-- | A /pure/ abstraction off the IO related actions available in B9. This is useful
-- to enable unit testing, OS-independence and debugging.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module B9.B9IO where

import B9.Content (Content(..), Environment(..))
import B9.DiskImages
import Control.Monad.Free
import Control.Monad.Trans.Writer.Lazy
import System.FilePath
import Text.Printf

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
    | MkTemp FilePath
             (FilePath -> next)
    | MkDir FilePath
            next
    | CopyDirectory FilePath
                    FilePath
                    next
    | RenderContentToFile FilePath
                          Content
                          Environment
                          next
    | ConvertImageTo Bool
                     ImageSource
                     ImageDestination
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

-- | Create a unique file path inside the build directory starting with a given
-- prefix and ending with a unique random token.
mkTemp :: FilePath -> IoProgram FilePath
mkTemp prefix = liftF $ MkTemp prefix id

-- | Just like @mkdir -p@
mkDir :: FilePath -> IoProgram ()
mkDir d = liftF $ MkDir d ()

-- | Combination of 'mkTemp' and 'mkDir'
mkTempDir :: FilePath -> IoProgram FilePath
mkTempDir prefix = do
    tmp <- mkTemp prefix
    mkDir tmp
    return tmp

-- | Make a recursive copy of an existing directory.
copyDirectory :: FilePath -> FilePath -> IoProgram ()
copyDirectory src dst = liftF $ CopyDirectory src dst ()

-- | Move/Copy/Upload/Convert/Create a disk-image from an 'ImageSource' to an
--   'ImageDestination' If the first parameter is 'True' the source is regarded
--   as destructable and might be destroyed during the operation, e.g. when an
--   image is exported as 'LocalFile' and neither size, nor image type nor
--   filesystem differ, the source image might as well simple be renamed.
convertImageTo :: Bool -> ImageSource -> ImageDestination -> IoProgram ()
convertImageTo removeSource src dst = liftF $ ConvertImageTo removeSource src dst ()

-- | Render a given content to a file. Implementations should overwrite the file
-- if it exists.
renderContentToFile :: FilePath -> Content -> Environment -> IoProgram ()
renderContentToFile f c e = liftF $ RenderContentToFile f c e ()

-- | Testing support
dumpToStrings :: IoProgram a -> [String]
dumpToStrings = snd . runPureDump

dumpToResult :: IoProgram a -> a
dumpToResult = fst . runPureDump

runPureDump :: IoProgram a -> (a, [String])
runPureDump p = runWriter (run dump p)
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
    dump (CopyDirectory s d n) = do
        tell [printf "copyDirectory %s %s" s d]
        return n
    dump (RenderContentToFile f c e n) = do
        tell [printf "renderContentToFile %s %s %s" f (show c) (show e)]
        return n
    dump (ConvertImageTo r i d n) = do
        tell [printf "convertImageTo %s %s %s" (show r) (show i) (show d)]
        return n
