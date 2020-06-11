{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module enables debugging all 'ByteString' to 'Text' to 'String' conversions.
-- This is an internal module.
--
-- @since 0.5.67
module B9.Text
  ( Text,
    LazyText,
    ByteString,
    LazyByteString,
    Textual (..),
    writeTextFile,
    unsafeRenderToText,
    unsafeParseFromText,
    parseFromTextWithErrorMessage,
    encodeAsUtf8LazyByteString,
  )
where

import Control.Exception (displayException)
-- import qualified Data.ByteString               as Strict

-- import qualified Data.Text.Encoding.Error      as Text
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import GHC.Stack

-- | Lazy byte strings.
--
-- A type alias to 'Lazy.ByteString' that can be used everywhere such that
-- references don't need to be qualified with the complete module name everywere.
--
-- @since 0.5.67
type LazyByteString = LazyByteString.ByteString

-- | Lazy texts.
--
-- A type alias to 'LazyText.Text' that can be used everywhere such that
-- references don't need to be qualified with the complete module name everywere.
--
-- @since 0.5.67
type LazyText = LazyText.Text

-- | A class for values that can be converted to/from 'Text'.
--
-- @since 0.5.67
class Textual a where
  -- | Convert a 'String' to 'Text'
  -- If an error occured, return 'Left' with the error message.
  --
  -- @since 0.5.67
  renderToText :: HasCallStack => a -> Either String Text

  -- | Convert a 'Text' to 'String'
  --
  -- @since 0.5.67
  parseFromText :: HasCallStack => Text -> Either String a

instance Textual Text where
  renderToText = Right
  parseFromText = Right

instance Textual String where
  renderToText = Right . Text.pack
  parseFromText = Right . Text.unpack

-- | Convert a 'ByteString' with UTF-8 encoded string to 'Text'
--
-- @since 0.5.67
instance Textual ByteString where
  renderToText x = case Text.decodeUtf8' x of
    Left u ->
      Left
        ( "renderToText of the ByteString failed: "
            ++ displayException u
            ++ " "
            ++ show x
            ++ "\nat:\n"
            ++ prettyCallStack callStack
        )
    Right t -> Right t
  parseFromText = Right . Text.encodeUtf8

-- | Convert a 'LazyByteString' with UTF-8 encoded string to 'Text'
--
-- @since 0.5.67
instance Textual LazyByteString where
  renderToText x = case LazyText.decodeUtf8' x of
    Left u ->
      Left
        ( "renderToText of the LazyByteString failed: "
            ++ displayException u
            ++ " "
            ++ show x
            ++ "\nat:\n"
            ++ prettyCallStack callStack
        )
    Right t -> Right (LazyText.toStrict t)
  parseFromText = Right . LazyByteString.fromStrict . Text.encodeUtf8

-- | Render a 'Text' to a file.
--
-- @since 0.5.67
writeTextFile :: (HasCallStack, MonadIO m) => FilePath -> Text -> m ()
writeTextFile f = liftIO . Text.writeFile f

-- | Render a 'Text' via 'renderToText' and throw a runtime exception when rendering fails.
--
-- @since 0.5.67
unsafeRenderToText :: (Textual a, HasCallStack) => a -> Text
unsafeRenderToText = either error id . renderToText

-- | Parse a 'Text' via 'parseFromText' and throw a runtime exception when parsing fails.
--
-- @since 0.5.67
unsafeParseFromText :: (Textual a, HasCallStack) => Text -> a
unsafeParseFromText = either error id . parseFromText

-- | Encode a 'String' as UTF-8 encoded into a 'LazyByteString'.
--
-- @since 0.5.67
encodeAsUtf8LazyByteString :: HasCallStack => String -> LazyByteString
encodeAsUtf8LazyByteString =
  LazyByteString.fromStrict . Text.encodeUtf8 . Text.pack

-- | Parse the given 'Text'. \
-- Return @Left errorMessage@ or @Right a@.
--
-- error message.
--
-- @since 0.5.67
parseFromTextWithErrorMessage ::
  (HasCallStack, Textual a) =>
  -- | An arbitrary string for error messages
  String ->
  Text ->
  Either String a
parseFromTextWithErrorMessage errorMessage b = case parseFromText b of
  Left e -> Left (unwords [errorMessage, e])
  Right a -> Right a
