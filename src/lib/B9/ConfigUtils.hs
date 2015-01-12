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
                      , consult
                      , maybeConsult
                      , maybeConsultSystemPath
                      , subst
                      ) where

import Data.Monoid
import Data.Function ( on )
import Data.Typeable
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import System.Directory
import Text.Read ( readEither )
import System.Random ( randomIO )
import Data.Word ( Word16, Word32 )
import System.FilePath
import Text.Printf
import Data.ConfigFile
import Data.Text.Template (render, templateSafe)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Data

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

ensureDir :: MonadIO m => FilePath -> m ()
ensureDir p = liftIO (createDirectoryIfMissing True $ takeDirectory p)

data ReaderException = ReaderException FilePath String
  deriving (Show, Typeable)
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

subst :: [(String,String)] -> String -> String
subst env template =
  LT.unpack (render template' env')
  where env' t = case lookup (T.unpack t) env of
                   Just v -> T.pack v
                   Nothing -> error ("Invalid template parameter: \""
                                    ++ (T.unpack t) ++ "\" in: \"" ++ template
                                    ++ "\". Valid entries: " ++ show env)
        template' = case templateSafe (T.pack template) of
          Left (row,col) -> error ("Invalid template, error at row: "
                                  ++ show row ++ ", col: " ++ show col
                                  ++ " in: \"" ++ template)
          Right t -> t
