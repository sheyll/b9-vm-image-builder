-- | Implement 'B9IO' to do the real-thing(tm).
module B9.B9IO.Impl where

import           B9.Common
import           B9.B9IO
import qualified B9.B9Monad             as B9Monad
import           B9.Logging
import qualified Conduit                as C
import qualified Data.ByteString        as B
import qualified Data.Conduit.Binary    as CB
import           System.IO
import           System.Random

-- | Execute a 'B9IO' Program in the 'B9' monad.
executeIoProg :: IoProgram a -> B9Monad.B9 a
executeIoProg = runB9IO go
  where
    go :: Action a -> B9Monad.B9 a
    go (LogMessage l s n) = do
        logMsg l s
        return n
    go (ExitError m c n) = do
        logMsg LogError m
        liftIO $ exitWith c
        return n
    go (GetBuildDir k) = do
        b <- B9Monad.getBuildDir
        return (k b)
    go (GetBuildId n) = do
        b <- B9Monad.getBuildId
        return (n b)
    go (GetBuildDate k) = do
        d <- B9Monad.getBuildDate
        return (k d)
    go (ReadContentFromFile f k) = do
        c <- liftIO (B.readFile f)
        return (k c)
    go (WriteContentToFile f c n) = do
        traceL "writing:" (unpackUtf8 c)
        liftIO $ B.writeFile f c
        return n
