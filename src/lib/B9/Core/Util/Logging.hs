-- | General logging API used throughout B9, with instances ranging from pure
-- 'String's and 'IO' upto 'Program', 'B9IO' and 'B9'. This API uses the
-- 'PrintfArgs' /trick/ for variadic functions.
module B9.Core.Util.Logging where

import Control.Arrow
import Data.Data
import Data.Int
import Data.Word
import GHC.Generics (Generic)
import Text.Show.Pretty (ppShow)

-- | Generate a 'LogTrace' log event with a message formed by concatenating
-- instances of 'LogArg' using the popular 'PrintfType' pattern.
-- Example:
--
-- > traceL "the file" fileName "was not found in" dirName
--
traceL :: CanLog m
       => m
traceL = logMsg LogTrace ""

-- | Generate a 'LogDebug' log event, similar to 'traceL'.
dbgL :: CanLog m
     => m
dbgL = logMsg LogDebug ""

-- | Generate a 'LogInfo' log event, similar to 'traceL'.
infoL :: CanLog m
      => m
infoL = logMsg LogInfo ""

-- | Generate a 'LogError' log event, similar to 'traceL'.
errorL :: CanLog m
       => m
errorL = logMsg LogError ""

-- | The log-levels
data LogLevel
  = LogTrace
  | LogDebug
  | LogInfo
  | LogError
  | LogNothing
  deriving (Eq,Ord,Generic,Data,Typeable)

instance Read LogLevel where
  readsPrec _ = lex >>> fmap (first toLogLevel)
    where toLogLevel "TRACE" = LogTrace
          toLogLevel "LogTrace" = LogTrace
          toLogLevel "DEBUG" = LogDebug
          toLogLevel "LogDebug" = LogDebug
          toLogLevel "INFO" = LogInfo
          toLogLevel "LogInfo" = LogInfo
          toLogLevel "ERROR" = LogError
          toLogLevel "LogError" = LogError
          toLogLevel _ = LogNothing

instance Show LogLevel where
  show LogTrace = "TRACE"
  show LogDebug = "DEBUG"
  show LogInfo = "INFO "
  show LogError = "ERROR"
  show LogNothing = "NONE "

-- | A class of things that can be used as parameter for the logging functions
-- above, i.e. 'traceL', 'infoL', 'debugL' and 'errorL'.  If a type has a 'Show'
-- instance the 'LogArg' instance can be specified by @instance LogArg MyType@
-- thanks to the @DefaultSignatures@ extension.  The default implementation uses
-- 'ppShow' for pretty output.
class LogArg a  where
  -- | Convert the argument into a string
  toLogMsgTxt :: a -> String
  default toLogMsgTxt :: Show a => a -> String
  toLogMsgTxt = ppShow

instance LogArg [Char] where
  toLogMsgTxt = id

instance LogArg ()

instance LogArg Char

instance LogArg Float

instance LogArg Double

instance LogArg Bool

instance LogArg Int

instance LogArg Int8

instance LogArg Int16

instance LogArg Int32

instance LogArg Int64

instance LogArg Word

instance LogArg Word8

instance LogArg Word16

instance LogArg Word32

instance LogArg Word64

instance LogArg Integer

instance (Show a1,Show a2) => LogArg (a1,a2)

instance (Show a1,Show a2,Show a3) => LogArg (a1,a2,a3)

instance (Show a1,Show a2,Show a3,Show a4) => LogArg (a1,a2,a3,a4)

-- | Class of types that can consume log messages into a value.
class CanLog a  where
  logMsg :: LogLevel -> String -> a

instance (LogArg x,CanLog r) => CanLog (x -> r) where
  logMsg l "" x = logMsg l (toLogMsgTxt x)
  logMsg l s x = logMsg l (unwords [s,toLogMsgTxt x])

instance (a ~ ()) => CanLog (IO a) where
  logMsg l s =
    do putStrLn (show l ++ ": " ++ s)
       return (error "CanLog l (IO a): result should not be used.")

instance CanLog [Char] where
  logMsg l s = show l ++ " " ++ s
