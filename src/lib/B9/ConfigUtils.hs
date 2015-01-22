{-# Language DeriveDataTypeable #-}
module B9.ConfigUtils ( allOn
                      , lastOn
                      , SystemPath (..)
                      , resolve
                      , ensureDir
                      , readIniFile
                      , getOptionM
                      , getOption
                      , getOptionOr
                      , IniFileException(..)
                      , module Data.ConfigFile
                      , UUID (..)
                      , randomUUID
                      , tell
                      , consult
                      , getDirectoryFiles
                      , maybeConsult
                      , maybeConsultSystemPath
                      , subst
                      , substFile
                      , substPath
                      ) where

import Data.Monoid
import Data.Function ( on )
import Data.Typeable
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.IO.Class
import System.Directory
import Text.Read ( readEither )
import System.Random ( randomIO )
import Data.Word ( Word16, Word32 )
import System.FilePath
import Text.Printf
import Data.ConfigFile
import Data.Text.Template (render,templateSafe)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Text.Encoding as E
import Data.Text.Lazy.Encoding as LE
import Data.Data
import Text.Show.Pretty (ppShow)

allOn :: (a -> Maybe Bool) -> a -> a -> Maybe Bool
allOn getter x y = getAll <$> on mappend (fmap All . getter) x y

lastOn :: (a -> Maybe b) -> a -> a -> Maybe b
lastOn getter x y = getLast $ on mappend (Last . getter) x y

data SystemPath = Path FilePath
                | InHomeDir FilePath
                | InB9UserDir FilePath
                | InTempDir FilePath
                  deriving (Eq, Read, Show, Typeable, Data)

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

-- | Get all files from 'dir' that is get ONLY files not directories
getDirectoryFiles :: MonadIO m => FilePath -> m [FilePath]
getDirectoryFiles dir = do
  entries <- liftIO (getDirectoryContents dir)
  fileEntries <- mapM (liftIO . doesFileExist . (dir </>)) entries
  return (snd <$> filter fst (fileEntries `zip` entries))

ensureDir :: MonadIO m => FilePath -> m ()
ensureDir p = liftIO (createDirectoryIfMissing True $ takeDirectory p)

data ReaderException = ReaderException FilePath String
  deriving (Show, Typeable)
instance Exception ReaderException

tell :: (MonadIO m, Show a) => FilePath -> a -> m ()
tell f x = do
  ensureDir f
  liftIO (writeFile f (ppShow x))

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

data IniFileException = IniFileException FilePath CPError
                      deriving (Show, Typeable)
instance Exception IniFileException

readIniFile :: MonadIO m => SystemPath -> m ConfigParser
readIniFile cfgFile' = do
  cfgFile <- resolve cfgFile'
  cp' <- liftIO $ readfile emptyCP cfgFile
  case cp' of
     Left e -> liftIO $ throwIO (IniFileException cfgFile e)
     Right cp -> return cp

getOption :: (Get_C a, Monoid a) => ConfigParser -> SectionSpec -> OptionSpec -> a
getOption cp sec key = either (const mempty) id $ get cp sec key

getOptionM :: (Get_C a, Read a) => ConfigParser -> SectionSpec -> OptionSpec -> Maybe a
getOptionM cp sec key = either (const Nothing) id $ get cp sec key

getOptionOr :: (Get_C a, Read a) => ConfigParser -> SectionSpec -> OptionSpec -> a -> a
getOptionOr cp sec key dv = either (const dv) id $ get cp sec key

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

-- String template substitution via dollar
subst :: [(String,String)] -> String -> String
subst env templateStr =
  LT.unpack (render template' env')
  where env' t = case lookup (T.unpack t) env of
                   Just v -> T.pack v
                   Nothing -> error ("Invalid template parameter: \""
                                    ++ (T.unpack t) ++ "\" in: \""
                                    ++ templateStr
                                    ++ "\".\nValid variables:\n" ++ ppShow env)
        template' = case templateSafe (T.pack templateStr) of
          Left (row,col) -> error ("Invalid template, error at row: "
                                  ++ show row ++ ", col: " ++ show col
                                  ++ " in: \"" ++ templateStr)
          Right t -> t

substFile :: MonadIO m => [(String, String)] -> FilePath -> FilePath -> m ()
substFile assocs src dest = do
  templateBs <- liftIO (B.readFile src)
  let t = templateSafe (E.decodeUtf8 templateBs)
  case t of
    Left (r,c) ->
      let badLine = unlines (take r (lines (T.unpack (E.decodeUtf8 templateBs))))
          colMarker = replicate (c - 1) '-' ++ "^"
      in error (printf "Template error in file '%s' line %i:\n\n%s\n%s\n"
                       src r badLine colMarker)
    Right template' -> do
      let out = LE.encodeUtf8 (render template' envLookup)
      liftIO (LB.writeFile dest out)
      return ()
  where
    envT :: [(T.Text, T.Text)]
    envT = (T.pack *** T.pack) <$> assocs
    envLookup :: T.Text -> T.Text
    envLookup x = maybe (err x) id (lookup x envT)
    err x = error (printf "Invalid template parameter: '%s'\n\
                          \In file: '%s'\n\
                          \Valid variables:\n%s\n"
                          (T.unpack x)
                          src
                          (ppShow assocs))

substPath :: [(String, String)] -> SystemPath -> SystemPath
substPath assocs src =
          case src of
            Path p -> Path (subst assocs p)
            InHomeDir p -> InHomeDir (subst assocs p)
            InB9UserDir p -> InB9UserDir (subst assocs p)
            InTempDir p -> InTempDir (subst assocs p)
