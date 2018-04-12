-- | Some utilities to deal with IO in B9.
module System.IO.B9Extras (SystemPath (..)
                                , resolve
                                , ensureDir
                                , getDirectoryFiles
                                , prettyPrintToFile
                                , consult
                                , ConsultException(..)
                                , randomUUID
                                 , UUID()
                                ) where

import Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad.Except
import System.Directory
import Text.Read ( readEither )
import System.Random ( randomIO )
import Data.Word ( Word16, Word32 )
import System.FilePath
import Text.Printf
import Data.Data
import Text.Show.Pretty (ppShow)

-- * Relative Paths

-- | A data type encapsulating different kinds of relative or absolute paths.
data SystemPath = Path FilePath  -- ^ A path that will just be passed through
                | InHomeDir FilePath -- ^ A OS specific path relative to
                                      -- the home directory of a user.
                | InB9UserDir FilePath -- ^ A path relative to the @b9@ sub of
                                       -- the users application configuration
                                       -- directory 'getAppUserDataDirectory'
                | InTempDir FilePath -- ^ A path relative to the systems
                                     -- temporary directory.
  deriving (Eq, Read, Show, Typeable, Data)

-- | Convert a 'SystemPath' to a 'FilePath'.
resolve :: MonadIO m => SystemPath -> m FilePath
resolve (Path p) = return p
resolve (InHomeDir p) = liftIO $ do
  d <- getHomeDirectory
  return $ d </> p
resolve (InB9UserDir p) = liftIO $ do
  d <- getAppUserDataDirectory "b9"
  return $ d </> p
resolve (InTempDir p) = liftIO $ do
  d <- getTemporaryDirectory
  return $ d </> p

-- * File System Directory Utilities

-- | Get all files from 'dir' that is get ONLY files not directories
getDirectoryFiles :: MonadIO m => FilePath -> m [FilePath]
getDirectoryFiles dir = do
  entries <- liftIO (getDirectoryContents dir)
  fileEntries <- mapM (liftIO . doesFileExist . (dir </>)) entries
  return (snd <$> filter fst (fileEntries `zip` entries))

-- | Create all missing parent directories of a file path.
-- Note that the file path is assumed to be of a regular file, and
-- 'takeDirectory' is applied before creating the directory.
ensureDir :: MonadIO m => FilePath -> m ()
ensureDir p = liftIO (createDirectoryIfMissing True $ takeDirectory p)

-- * Reading and Writing from/to Files

-- | Write a value of a type that is an instance of 'Show' to file.
-- This function uses 'ppShow' instead of the given 'Show' instance.
prettyPrintToFile :: (MonadIO m, Show a) => FilePath -> a -> m ()
prettyPrintToFile f x = do
  ensureDir f
  liftIO (writeFile f (ppShow x))

-- | Read a value of a type that is an instance of 'Read' from a file.
-- This function throws a 'ConsultException' when the read the file failed.
consult :: (MonadIO m, Read a) => FilePath -> m a
consult f = liftIO $ do
  c <- readFile f
  case readEither c of
    Left e ->
      throwIO $ ConsultException f e
    Right a ->
      return a

-- | An 'Exception' thrown by 'consult' to indicate the file does not
-- contain a 'read'able String
data ConsultException = ConsultException FilePath String
  deriving (Show, Typeable)

instance Exception ConsultException

-- * Unique Random IDs

-- | A bunch of numbers, enough to make globally unique IDs. Create one of these
-- using 'randomUUID'.
newtype UUID = UUID (Word32, Word16, Word16, Word16, Word32, Word16)
             deriving (Read, Show, Eq, Ord)

instance PrintfArg UUID where
  formatArg (UUID (a, b, c, d, e, f)) fmt
    | fmtChar (vFmt 'U' fmt) == 'U' =
        let str = (printf "%08x-%04x-%04x-%04x-%08x%04x" a b c d e f :: String)
        in formatString str (fmt { fmtChar = 's', fmtPrecision = Nothing })
    | otherwise = errorBadFormat $ fmtChar fmt

-- | Generate a random 'UUID'.
randomUUID :: MonadIO m => m UUID
randomUUID = liftIO
               (UUID <$> ((,,,,,) <$> randomIO
                                  <*> randomIO
                                  <*> randomIO
                                  <*> randomIO
                                  <*> randomIO
                                  <*> randomIO))
