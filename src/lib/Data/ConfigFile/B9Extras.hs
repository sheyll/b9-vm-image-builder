{-# Language DeriveDataTypeable, ConstraintKinds #-}
{-| Extensions to 'Data.ConfigFile' and utility functions for dealing with
    configuration in general and reading/writing files. -}
module Data.ConfigFile.B9Extras ( addSectionCP
                                , setShowCP
                                , setCP
                                , readCP
                                , mergeCP
                                , toStringCP
                                , sectionsCP
                                , emptyCP
                                , type CPGet
                                , type CPOptionSpec
                                , type CPSectionSpec
                                , type CPDocument
                                , CPError()
                                , readCPDocument
                                , CPReadException(..)
                                ) where

#if !MIN_VERSION_base(4,10,0)
import Data.Monoid
#endif
import Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.ConfigFile
import Control.Exception
import Control.Monad.Except
import System.IO.B9Extras

-- * Aliases for functions and types from 'ConfigParser' in 'Data.ConfigFile'

-- | An alias for  'ConfigParser'
type CPDocument = ConfigParser

-- | An alias for 'SectionSpec'.
type CPSectionSpec = SectionSpec

-- | An alias for 'OptionSpec'
type CPOptionSpec = OptionSpec

-- | An alias for 'setshow'.
setShowCP :: (Show a, MonadError CPError m) => CPDocument -> CPSectionSpec -> CPOptionSpec -> a -> m CPDocument
setShowCP = setshow

-- | An alias for 'set'.
setCP :: (MonadError CPError m) => CPDocument -> CPSectionSpec -> CPOptionSpec -> String -> m CPDocument
setCP = set

-- | An alias for 'get'.
readCP :: (CPGet a, MonadError CPError m) => CPDocument -> CPSectionSpec -> CPOptionSpec -> m a
readCP = get

-- | An alias for 'Get_C'
type CPGet a = Get_C a

-- | An alias for 'add_section'.
addSectionCP :: MonadError CPError m => CPDocument -> CPSectionSpec -> m CPDocument
addSectionCP = add_section

-- | An alias for 'merge'.
mergeCP :: CPDocument -> CPDocument -> CPDocument
mergeCP = merge

-- | An alias for 'to_string'
toStringCP :: CPDocument -> String
toStringCP = to_string

-- | An alias for 'sections'.
sectionsCP :: CPDocument -> [SectionSpec]
sectionsCP = sections

-- * Reading a 'CPDocument' from a 'SystemPath'

-- | Read a file and try to parse the contents as a 'CPDocument', if something
-- goes wrong throw a 'CPReadException'
readCPDocument :: MonadIO m => SystemPath -> m CPDocument
readCPDocument cfgFile' = do
  cfgFilePath <- resolve cfgFile'
  liftIO $ do
    res <- readfile emptyCP cfgFilePath
    case res of
      Left e -> throwIO (CPReadException cfgFilePath e)
      Right cp -> return cp

-- | An exception thrown by 'readCPDocument'.
data CPReadException = CPReadException FilePath CPError
                      deriving (Show, Typeable)
instance Exception CPReadException
