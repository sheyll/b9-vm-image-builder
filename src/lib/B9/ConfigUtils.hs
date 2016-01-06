{-# Language DeriveDataTypeable #-}
{-| Extensions to 'Data.ConfigFile' and utility functions for dealing with
    configuration in general and reading/writing files. -}
module B9.ConfigUtils
       (allOn, lastOn, SystemPath(..), resolve, ensureDir, readIniFile,
        getOptionM, getOption, getOptionOr, IniFileException(..),
        UUID(..), randomUUID, tell, consult,
        getDirectoryFiles, maybeConsult, maybeConsultSystemPath)
       where

import Control.Exception
import Control.Monad.IO.Class
import Data.ConfigFile as CF
import Data.Data
import Data.Function ( on )
import Data.Monoid
import Data.Word ( Word16, Word32 )
import System.Directory
import System.FilePath
import System.Random ( randomIO )
import Text.Printf
import Text.Read ( readEither )
import Text.Show.Pretty (ppShow)

allOn :: (a -> Maybe Bool) -> a -> a -> Maybe Bool
allOn getter x y = getAll <$> on mappend (fmap All . getter) x y

lastOn :: (a -> Maybe b) -> a -> a -> Maybe b
lastOn getter x y = getLast $ on mappend (Last . getter) x y

data SystemPath
    = Path FilePath
    | InHomeDir FilePath
    | InB9UserDir FilePath
    | InTempDir FilePath
    deriving (Eq,Read,Show,Typeable,Data)

resolve
    :: MonadIO m
    => SystemPath -> m FilePath
resolve (Path p) = return p
resolve (InHomeDir p) =
    liftIO $
    do d <- getHomeDirectory
       return $ d </> p
resolve (InB9UserDir p) =
    liftIO $
    do d <- getAppUserDataDirectory "b9"
       return $ d </> p
resolve (InTempDir p) =
    liftIO $
    do d <- getTemporaryDirectory
       return $ d </> p

-- | Get all files from 'dir' that is get ONLY files not directories
getDirectoryFiles
    :: MonadIO m
    => FilePath -> m [FilePath]
getDirectoryFiles dir = do
    entries <- liftIO (getDirectoryContents dir)
    fileEntries <- mapM (liftIO . doesFileExist . (dir </>)) entries
    return (snd <$> filter fst (fileEntries `zip` entries))

ensureDir
    :: MonadIO m
    => FilePath -> m ()
ensureDir p = liftIO (createDirectoryIfMissing True $ takeDirectory p)

data ReaderException =
    ReaderException FilePath
                    String
    deriving (Show,Typeable)
instance Exception ReaderException

tell
    :: (MonadIO m, Show a)
    => FilePath -> a -> m ()
tell f x = do
    ensureDir f
    liftIO (writeFile f (ppShow x))

consult
    :: (MonadIO m, Read a)
    => FilePath -> m a
consult f =
    liftIO $
    do c <- readFile f
       case readEither c of
           Left e -> throwIO $ ReaderException f e
           Right a -> return a

maybeConsult
    :: (MonadIO m, Read a)
    => Maybe FilePath -> a -> m a
maybeConsult Nothing defaultArg = return defaultArg
maybeConsult (Just f) defaultArg =
    liftIO $
    do exists <- doesFileExist f
       if exists
           then consult f
           else return defaultArg

maybeConsultSystemPath
    :: (MonadIO m, Read a)
    => Maybe SystemPath -> a -> m a
maybeConsultSystemPath Nothing defaultArg = return defaultArg
maybeConsultSystemPath (Just f) defaultArg =
    liftIO $
    do f' <- resolve f
       exists <- doesFileExist f'
       if exists
           then consult f'
           else return defaultArg

data IniFileException =
    IniFileException FilePath
                     CPError
    deriving (Show,Typeable)

instance Exception IniFileException

readIniFile
    :: MonadIO m
    => SystemPath -> m ConfigParser
readIniFile cfgFile' = do
    cfgFile <- resolve cfgFile'
    cp' <- liftIO $ readfile emptyCP cfgFile
    case cp' of
        Left e -> liftIO $ throwIO (IniFileException cfgFile e)
        Right cp -> return cp

getOption
    :: (Get_C a, Monoid a)
    => ConfigParser -> SectionSpec -> OptionSpec -> a
getOption cp sec key = either (const mempty) id $ CF.get cp sec key

getOptionM
    :: (Get_C a, Read a)
    => ConfigParser -> SectionSpec -> OptionSpec -> Maybe a
getOptionM cp sec key = either (const Nothing) id $ CF.get cp sec key

getOptionOr
    :: (Get_C a, Read a)
    => ConfigParser -> SectionSpec -> OptionSpec -> a -> a
getOptionOr cp sec key dv = either (const dv) id $ CF.get cp sec key

-- TODO refactor/move UUID to a better place?
newtype UUID =
    UUID (Word32, Word16, Word16, Word16, Word32, Word16)
    deriving (Read,Show,Eq,Ord)

-- TODO refactor/move UUID to a better place?
instance PrintfArg UUID where
    formatArg (UUID (a,b,c,d,e,f)) fmt
      | fmtChar (vFmt 'U' fmt) == 'U' =
          let str =
                  (printf "%08x-%04x-%04x-%04x-%08x%04x" a b c d e f :: String)
          in formatString
                 str
                 (fmt
                  { fmtChar = 's'
                  , fmtPrecision = Nothing
                  })
      | otherwise = errorBadFormat $ fmtChar fmt

-- TODO refactor/move randomUUID to a better place?
randomUUID
    :: MonadIO m
    => m UUID
randomUUID =
    liftIO
        (UUID <$>
         ((,,,,,) <$> randomIO <*> randomIO <*> randomIO <*> randomIO <*>
          randomIO <*>
          randomIO))
