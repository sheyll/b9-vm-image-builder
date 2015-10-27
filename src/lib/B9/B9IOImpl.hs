-- | Implement 'B9IO' to do the real-thing(tm).
module B9.B9IOImpl where

import           B9.B9IO
import qualified B9.B9Monad as B9Monad
import           B9.Content
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           System.Directory
import           System.FilePath
import           System.Random
import           Text.Printf
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import qualified Data.ByteString as B


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
    go (CopyDir s d n) = do
        B9Monad.cmdRaw "cp" ["-r", s, d]
        return n
    go (MoveFile s d n) = do
        liftIO $ renameFile s d
        return n
    go (MoveDir s d n) = do
        liftIO $ renameDirectory s d
        return n
    go (GetParentDir f k) = do
        return $ k (takeDirectory f)
    go (GetRealPath f k) = do
        f' <- liftIO $ makeAbsolute f
        return $ k f'
    go (GetFileName f k) = do
        return $ k (takeFileName f)
    go (RenderContentToFile f c e n) = do
        result <- runReaderT (render c) e
        B9Monad.traceL $ printf "rendered: \n%s\n" (T.unpack (E.decodeUtf8 result))
        liftIO $ B.writeFile f result
        return n
    go (CreateFileSystem f c fs n) = do
        return n
