-- | Implement 'B9IO' to do the real-thing(tm).
module B9.B9IOImpl where

import           B9.B9IO
import qualified B9.B9Monad as B9Monad
import           B9.Content
import           B9.DiskImages
import           B9.ExecEnv
import           B9.FileSystems
import qualified B9.LibVirtLXC as LXC
import           B9.MBR
import           B9.QemuImg
import           B9.RepositoryIO
import           B9.ShellScript
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Random
import           Text.Printf
import qualified Conduit as C
import qualified Data.Conduit.Binary as CB

-- | Execute a 'B9IO' Program in the 'B9' monad.
executeIoProg :: IoProgram a -> B9Monad.B9 a
executeIoProg = runB9IO go
  where
    go :: Action a -> B9Monad.B9 a
    go (LogMessage l s n) = do
        B9Monad.b9Log l s
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
    go (MkTemp prefix k) = do
        b <- B9Monad.getBuildDir
        go (MkTempIn b prefix k)
    go (MkTempIn parent prefix k) = do
        let prefix' = takeFileName prefix
        suffix <- liftIO $ replicateM 10 (randomRIO ('A', 'Z'))
        return (k (parent </> prefix' ++ "-" ++ suffix))
    go (MkDir d n) = do
        liftIO $ createDirectoryIfMissing True d
        return n
    go (Copy s d n) = do
        liftIO $ copyFile s d
        return n
    go (CopyDir s d n) = do
        exists <- liftIO $ doesDirectoryExist d
        when exists (liftIO $ removeDirectoryRecursive d)
        B9Monad.cmdRaw "cp" ["-r", s, d]
        return n
    go (MoveFile s d n) = do
        B9Monad.cmdRaw "mv" [s, d]
        return n
    go (MoveDir s d n) = do
        exists <- liftIO $ doesDirectoryExist d
        when exists (liftIO $ removeDirectoryRecursive d)
        B9Monad.cmdRaw "mv" [s, d]
        return n
    go (GetParentDir f k) = return $ k (takeDirectory f)
    go (ReadFileSize f k) = do
        s <- liftIO $ withFile f ReadMode hFileSize
        return $ k s
    go (GetRealPath f k) = do
        f' <- liftIO $ makeAbsolute f
        return $ k f'
    go (GetFileName f k) = return $ k (takeFileName f)
    go (RenderContentToFile f c e n) = do
        result <- runReaderT (render c) e
        B9Monad.traceL $
            printf "rendered: \n%s\n" (T.unpack (E.decodeUtf8 result))
        liftIO $ B.writeFile f result
        return n
    go (CreateFileSystem dst fs srcDir files n) = do
        createFSWithFiles dst fs srcDir files
        return n
    go (ResizeFileSystem f r t n) = do
        resizeFS r f t
        return n
    go (ConvertVmImage s st d dt n) = do
        convertImageType s st d dt
        return n
    go (ResizeVmImage i s u t n) = do
        resizeImage_ (ImageSize s u) i t
        return n
    go (ExtractPartition (MBRPartition partIndex) s d n) = do
        (start,len) <- liftIO $ B9.MBR.getPartition partIndex s
        B9Monad.traceL
            (printf
                 "extracting MBR partition %d starting at %d with a length of %d (bytes) from %s to %s"
                 partIndex
                 start
                 len
                 s
                 d)
        liftIO
            (C.runResourceT
                 ((CB.sourceFileRange
                       s
                       (Just (fromIntegral start))
                       (Just (fromIntegral len))) C.$$
                  (CB.sinkFile d)))
        return n
    go (ImageRepoLookup s k) = do
        si <- getLatestSharedImageByNameFromCache s
        src <- getSharedImageCachedFilePath si
        return $ k (si, src)
    go (ImageRepoPublish f t sn n) = do
        let i = Image f t Ext4 -- TODO the file system should not be a parameter
        void $ shareImage i sn
        return n
    go (ExecuteInEnv e s d i n) = do
        let env = ExecEnv (e ^. execEnvTitle) i d (e ^. execEnvLimits)
        res <- LXC.runInEnvironment env s
        when
            (not res)
            (fail $
             printf
                 "CONTAINER EXECUTION ERROR!\n== Failed to execute this script: == \n================================================================================\nIn that environment: %s\n"
                 (toBash $ toCmds s)
                 (show env))
        return n
