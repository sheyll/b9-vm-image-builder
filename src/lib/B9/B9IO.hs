-- | A /pure/ abstraction off the IO related actions available in B9. This is useful
-- to enable unit testing, OS-independence and debugging.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module B9.B9IO
       (IoProgram, MonadIoProgram(..), runB9IO, Action(..), getBuildDir,
        getBuildId, getBuildDate, exitError, newUUID, nextInteger,
        CommandSpec(..), runCommand, getConfigEntry, putConfigEntry,
        readContentFromFile, writeContentToFile, traceEveryAction,
        dumpToStrings, dumpToResult, runPureDump, arbitraryIoProgram)
       where

import B9.Common
import B9.Logging
import B9.QCUtil
import Control.Monad.Free
import Control.Monad.Trans.Writer.Lazy
import Test.QuickCheck
import System.Posix.Types
import Data.UUID

-- | Programs representing imperative, /impure/ IO actions required by B9 to
-- create, convert and install VM images or cloud init disks.  Pure 'Action's
-- are combined to a free monad. This seperation from actually doing the IO and
-- modelling the IO actions as pure data enables unit testing and debugging.
newtype IoProgram a =
  IoProgram {runIoProgram :: Free Action a}
  deriving (Functor,Applicative,Monad,MonadFree Action)

-- | Class of monads that can contain 'IoProgram's.
class MonadIoProgram m  where
  liftIoProgram :: IoProgram a -> m a

-- | Execute an 'IoProgram' using a monadic interpretation function.
runB9IO
  :: Monad m
  => (forall a. Action a -> m a) -> IoProgram b -> m b
runB9IO a = foldFree a . runIoProgram

-- | Basic IO actions that are easy to implement on any platform. There are no
-- platform specific commands like user creation, these would have to be
-- implemented using 'RunCommand'. To create an multiplatform version of B9
-- there would be different implementation on top of 'Action' - not below.
data Action next
  = LogMessage LogLevel
               String
               next
  | ExitError String
              ExitCode
              next
  | GetBuildDir (FilePath -> next)
  | GetBuildId (String -> next)
  | GetBuildDate (String -> next)
  | NewUUID (UUID -> next)
  | NextInteger (Integer -> next)
  | GetConfigEntry String
                   ([String] -> next)
  | PutConfigEntry String
                   [String]
                   next
  | RunCommand CommandSpec
               ((ExitCode,ByteString,ByteString) -> next)
  |
  | ReadContentFromFile FilePath
                        (ByteString -> next)
  | WriteContentToFile FilePath
                       ByteString
                       next
  deriving (Typeable,Functor)

-- | Define the command to run.
data CommandSpec =
  CommandSpec {csCmdExe :: FilePath
              ,csCmdArgs :: [String]
              ,csCwd :: Maybe FilePath
              ,csEnv :: Maybe [(String,String)]
              ,csDelegateCtlC :: Bool
              ,csChildGroup :: Maybe GroupID
              ,csChildUser :: Maybe UserID}
  deriving (Read,Show,Typeable)

instance Show (Action a) where
  show (LogMessage l m _) =
    printf "logMessage %s %s"
           (show l)
           (show m)
  show (ExitError msg code _) =
    printf "exitError (%s) %s"
           (show msg)
           (show code)
  show (GetBuildId _) = "getBuildId"
  show (GetBuildDate _) = "getBuildDate"
  show (GetBuildDir _) = "getBuildDir"
  show (NewUUID _) = "newUUID"
  show (NextInteger _) = "nextInteger"
  show (GetConfigEntry k _) = printf "getConfigEntry %s" (show k)
  show (PutConfigEntry k vs _) =
    printf "putConfigEntry %s %s"
           (show k)
           (show vs)
  show (RunCommand cs _) = printf "runCommand %s" (show cs)
  show (ReadContentFromFile p _) = printf "readContentFromFile %s" p
  show (WriteContentToFile f c _) =
    printf "writeContentToFile %s %s" f (show c)

instance LogArg (Action a)

-- | High-level logging API
instance (a ~ ()) => CanLog (IoProgram a) where
  logMsg l str = liftF $ LogMessage l str ()

-- | Exit with error.
exitError :: String -> ExitCode -> IoProgram ()
exitError msg code = liftF $ ExitError msg code ()

-- | Get the (temporary) directory of the current b9 execution
getBuildDir :: IoProgram FilePath
getBuildDir = liftF $ GetBuildDir id

-- | Get a arbitrary random number selected when B9 starts, that serves as
-- unique id.
getBuildId :: IoProgram String
getBuildId = liftF $ GetBuildId id

-- | Get the system time at the start of the overall build formatted string,
-- such that the lexicographical order of the strings is the same as the order
-- of the correspondig time stamps.
getBuildDate :: IoProgram String
getBuildDate = liftF $ GetBuildDate id

-- | Create a new 'UUID'.
newUUID :: IoProgram UUID
newUUID = liftF $ NewUUID id

-- | Return the next of a monotonically increasing series of positive naturals.
nextInteger :: IoProgram Integer
nextInteger = liftF $ NextInteger id

-- | Retreive all values for a given configuration key from the b9 config.
getConfigEntry :: String -> IoProgram [String]
getConfigEntry k = liftF $ GetConfigEntry k id

-- | Overwrite the values for a given configuration key.
putConfigEntry
  :: String -> [String] -> IoProgram ()
putConfigEntry k vs = liftF $ PutConfigEntry k vs ()

-- | Execute an external command identified by a 'CommandSpec'.
runCommand
  :: CommandSpec -> IoProgram (ExitCode,ByteString,ByteString)
runCommand cs = liftF $ RunCommand cs id

-- | Read contents from a file.
readContentFromFile
  :: FilePath -> IoProgram ByteString
readContentFromFile p = liftF $ ReadContentFromFile p id

-- | Render a given content to a file. Implementations should overwrite the file
-- if it exists.
writeContentToFile
  :: FilePath -> ByteString -> IoProgram ()
writeContentToFile f c = liftF $ WriteContentToFile f c ()

-- * Program transformation
-- | Wrap a interpreter for a 'IoProgram' such that all invokations except for
-- 'LogMessage' are logged via 'LogMessage'.
traceEveryAction :: IoProgram a -> IoProgram a
traceEveryAction = runB9IO traceAction
  where traceAction (LogMessage l s n) =
          do logMsg l s
             return n
        traceAction e@(ExitError m c n) =
          do traceL e
             exitError m c
             return n
        traceAction a@(GetBuildDir k) =
          do traceL a
             b <- getBuildDir
             traceL "->" b
             return $ k b
        traceAction a@(GetBuildId k) =
          do traceL a
             b <- getBuildId
             traceL "->" b
             return $ k b
        traceAction a@(GetBuildDate k) =
          do traceL a
             b <- getBuildDate
             traceL "->" b
             return $ k b
        traceAction a@(NewUUID k) =
          do traceL a
             b <- newUUID
             traceL "->" (toString b)
             return $ k b
        traceAction a@(NextInteger k) =
          do traceL a
             b <- nextInteger
             traceL "->" b
             return $ k b
        traceAction a@(GetConfigEntry key k) =
          do traceL a
             vs <- getConfigEntry key
             traceL "->" (show vs)
             return $ k vs
        traceAction a@(PutConfigEntry key vs n) =
          do traceL a
             putConfigEntry key vs
             return n
        traceAction a@(RunCommand spec k) =
          do traceL a
             res <- runCommand spec
             traceL "->" (show res)
             return (k res)
        traceAction a@(ReadContentFromFile f k) =
          do traceL a
             p <- readContentFromFile f
             traceL "->"
                    (if lengthB p < 1024
                        then unlines ["read file contents:"
                                     ,"------8<-----"
                                     ,unpackUtf8 p
                                     ,"------8<-----"
                                     ,""]
                        else (printf "read %d bytes from file" (lengthB p)))
             return $ k p
        traceAction a@(WriteContentToFile f c n) =
          do traceL a
             writeContentToFile f c
             return n

-- * Testing support
-- | Run a program without any I/O
--   and return a list of strings, each list element is a textual representation
--   of the command and its parameters. This is useful for testing and
--   inspection.
dumpToStrings :: IoProgram a -> [String]
dumpToStrings = snd . runPureDump

-- | Run a program without any I/O
--   and return a the return value of the program. This is useful for testing
--   and inspection.
dumpToResult :: IoProgram a -> a
dumpToResult = fst . runPureDump

-- | Run a program without any I/O using a simple writer
--   monad where the output is a list of strings each representing an action of
--   the program and its paraters.  This is useful for testing and inspection.
runPureDump :: IoProgram a -> (a,[String])
runPureDump p = runWriter $ runB9IO dump p
  where dump :: Action a -> Writer [String] a
        dump a@(LogMessage _l _s n) =
          do tell [show a]
             return n
        dump a@(ExitError _m _c n) =
          do tell [show a]
             return n
        dump a@(GetBuildDir k) =
          do tell [show a]
             return (k "/BUILD")
        dump a@(GetBuildId n) =
          do tell [show a]
             return (n "build-id-1234")
        dump a@(GetBuildDate n) =
          do tell [show a]
             return (n "1970-01-01 00:00:00")
        dump a@(NewUUID k) =
          do tell [show a]
             return (k $
                     fromJust $
                     fromString "c2cf10e1-52d6-4b6f-b899-38d97a112d8c")
        dump a@(NextInteger k) =
          do tell [show a]
             return (k 123)
        dump a@(GetConfigEntry key k) =
          do tell [show a]
             return (k [key ++ "-value"])
        dump a@(PutConfigEntry _key _vs n) =
          do tell [show a]
             return n
        dump a@(RunCommand _c k) =
          do tell [show a]
             return (k (ExitFailure 1,packB "stdout",packB "stderr"))
        dump a@(ReadContentFromFile _f k) =
          do tell [show a]
             return (k (packB "some contents"))
        dump a@(WriteContentToFile _f _c n) =
          do tell [show a]
             return n

arbitraryIoProgram :: Gen (IoProgram ())
arbitraryIoProgram = IoProgram <$> arbitraryFree

instance Arbitrary a => Arbitrary (Action a) where
  arbitrary =
    oneof [LogMessage LogTrace <$> smaller arbitraryNiceString <*>
           smaller arbitrary
          ,ExitError <$> smaller arbitrary <*>
           smaller (ExitFailure <$> arbitrary) <*>
           smaller arbitrary
          ,GetBuildId <$> arbitrary
          ,GetBuildDir <$> arbitrary
          ,GetBuildDate <$> arbitrary
          ,NewUUID <$> ((. toString) <$> arbitrary)
          ,NextInteger <$> arbitrary
          ,GetConfigEntry <$> smaller arbitrary <*> smaller arbitrary
          ,PutConfigEntry <$> smaller arbitrary <*> smaller arbitrary <*>
           smaller arbitrary
          ,RunCommand <$> smaller arbitrary <*>
           smaller ((. (\(ExitFailure c,stdo,stde) ->
                          (c,unpackUtf8 stdo,unpackUtf8 stde))) <$>
                    arbitrary)
          ,ReadContentFromFile <$> smaller arbitraryFilePath <*>
           smaller ((. unpackUtf8) <$> arbitrary)
          ,WriteContentToFile <$> smaller arbitraryFilePath <*>
           (packB <$> smaller arbitrary) <*>
           smaller arbitrary]

--  f (g <*> h <*> j)
instance Arbitrary CommandSpec where
  arbitrary =
    CommandSpec <$> smaller arbitraryFilePath <*>
    smaller (listOf arbitraryNiceString) <*>
    smaller arbitrary <*>
    smaller (oneof [pure Nothing
                   ,Just <$>
                    listOf ((,) <$> arbitraryNiceString <*> arbitraryNiceString)]) <*>
    smaller arbitrary <*>
    smaller (oneof [Just . CGid <$> arbitrary,pure Nothing]) <*>
    smaller (oneof [Just . CUid <$> arbitrary,pure Nothing])
