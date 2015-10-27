-- | Implement 'B9IO' to do the real-thing(tm).
module B9.B9IOImpl where

import           B9.B9IO
import qualified B9.B9Monad as B9Monad
import           Control.Monad.IO.Class
import           System.Directory
import           System.FilePath
import           System.Random

-- | Execute a 'B9IO' Program in the 'B9' monad.
executeIoProg :: IoProgram a -> B9Monad.B9 a
executeIoProg p = run go p
  where
    go :: Action a -> B9Monad.B9 a
    go (LogTrace s n) = do
        B9Monad.traceL s
        return n
    go (GetBuildDir k) = do
        b <- B9Monad.getBuildDir
        return (k b)
    go (GetBuildId n) = do
        b <- B9Monad.getBuildId
        return (n b)
    go (MkTemp prefix k) = do
        let prefix' = takeFileName prefix
        suffix <- liftIO $ sequence $ take 10 $ repeat (randomRIO ('A', 'Z'))
        b <- B9Monad.getBuildDir
        return (k (b </> prefix' ++ "-" ++ suffix))
    go (MkDir d n) = do
        liftIO $ createDirectoryIfMissing True d
        return n
    go (Copy s d n) = do
        liftIO $ copyFile s d
        return n
    go (MoveFile s d n) = do
        return n
    go (MoveDir s d n) = do
        return n
    go (GetParentDir f k) = do
        return (k (takeDirectory f))
    go (GetRealPath "." k) = do
        return (k ("/cwd"))
    go (GetRealPath f k) = do
        return (k ("/abs/path/" ++ f))
    go (GetFileName f k) = do
        return (k (takeFileName f))
    go (RenderContentToFile _f _c _e n) = do
        return n
    go (CreateFileSystem _f _c _fs n) = do
        return n
