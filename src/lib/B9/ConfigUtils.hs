{-# Language DeriveDataTypeable #-}
module B9.ConfigUtils ( allOn
                      , lastOn
                      , SystemPath (..)
                      , resolve
                      , ensureDir
                      , UUID (..)
                      , randomUUID
                      , consult
                      , maybeConsult
                      , maybeConsultSystemPath
                      ) where

import Data.Monoid
import Data.Function ( on )
import Data.Maybe
import Data.Typeable
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import System.Directory
import Text.Read ( readEither )
import System.Random ( randomIO )
import Data.Word ( Word16, Word32 )
import System.FilePath
import System.Directory
import Text.Printf

allOn :: (a -> Maybe Bool) -> a -> a -> Maybe Bool
allOn getter x y = getAll <$> on mappend (fmap All . getter) x y

lastOn :: (a -> Maybe b) -> a -> a -> Maybe b
lastOn getter x y = getLast $ on mappend (Last . getter) x y

data SystemPath = Path FilePath
                | InHomeDir FilePath
                | InB9UserDir FilePath
                | InTempDir FilePath
                  deriving (Eq, Read, Show)

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

ensureDir :: MonadIO m => FilePath -> m ()
ensureDir p = liftIO (createDirectoryIfMissing True $ takeDirectory p)

data ReaderException = ReaderException { fileToRead :: FilePath
                                       , errorMessage :: String
                                       } deriving (Show, Typeable)
instance Exception ReaderException


consult :: (MonadIO m, Read a) => FilePath -> m a
consult f = liftIO $ do
  c <- readFile f
  case readEither c of
   Left e ->
     throwIO $ ReaderException f e
   Right a ->
     return a

maybeConsult :: (MonadIO m, Read a) => Maybe FilePath -> a -> m a
maybeConsult Nothing defaultArg = return defaultArg
maybeConsult (Just f) defaultArg = liftIO $ do
  exists <- doesFileExist f
  if exists
    then do consult f
    else return defaultArg

maybeConsultSystemPath :: (MonadIO m, Read a) => Maybe SystemPath -> a -> m a
maybeConsultSystemPath Nothing defaultArg = return defaultArg
maybeConsultSystemPath (Just f) defaultArg = liftIO $ do
  f' <- resolve f
  exists <- doesFileExist f'
  if exists
    then do consult f'
    else return defaultArg

newtype UUID = UUID (Word32, Word16, Word16, Word16, Word32, Word16)
             deriving (Read, Show, Eq, Ord)

instance PrintfArg UUID where
  formatArg (UUID (a, b, c, d, e, f)) fmt
    | fmtChar (vFmt 'U' fmt) == 'U' =
        let str = (printf "%08x-%04x-%04x-%04x-%08x%04x" a b c d e f :: String)
        in formatString str (fmt { fmtChar = 's', fmtPrecision = Nothing })
    | otherwise = errorBadFormat $ fmtChar fmt


randomUUID :: MonadIO m => m UUID
randomUUID = liftIO (UUID <$> ((,,,,,)
                               <$> randomIO
                               <*> randomIO
                               <*> randomIO
                               <*> randomIO
                               <*> randomIO
                               <*> randomIO))
