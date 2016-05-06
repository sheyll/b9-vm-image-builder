{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The internal core definitions for the DSL of B9.
--  The approach tries to overcome the _expression problem_ as defined by
-- P. Wadler, by using a GADT 'BuildStep' with basic CRUD like contructors, as
-- well as a type class for each constructor with associated types for
-- parameters and/or return values. In that way the library can be extended by
-- adding new instances of the typeclasses.
module B9.Dsl.Core (module B9.Dsl.Core, module X) where

import B9.Dsl.Handle as X
import Control.Monad.Free (Free(..), liftF, foldFree)

-- ---------------------------------------------------------

-- | A program is a free monad of 'BuildStep's over a type constructor @m@.
type ProgramT m = Free (BuildStep m)

-- | The abstract _CRUD_-like GADT underlying all 'ProgramT's.
data BuildStep m next where
    Create ::
        (CanCreate m a) =>
        p a -> CreateSpec m a -> (Handle a -> next) -> BuildStep m next
    Add ::
        (CanAdd m a b) =>
        Handle a ->
          p b -> AddSpec m a b -> next -> BuildStep m next
    Extract ::
        (CanExtract m a b) =>
        Handle a ->
          p b -> ExtractionArg m a b -> (Handle b -> next) -> BuildStep m next
    Export ::
        (CanExport m a) =>
        Handle a ->
          ExportSpec m a -> next -> BuildStep m next

-- | The requirement to create a free monad of a type is a functor instance.
instance Functor (BuildStep m) where
    fmap f (Create sa src k)            = Create sa src (f . k)
    fmap f (Add hndEnv sa addSpec next) = Add hndEnv sa addSpec (f next)
    fmap f (Extract hA sB conv k)       = Extract hA sB conv (f . k)
    fmap f (Export hnd out next)        = Export hnd out (f next)

-- * Artifact Creation

-- | Class of artifacts that can be created.
class CanCreate m (a :: k)  where
    -- | Contruction parameters required to create the artifact
    type family CreateSpec m (a :: k) :: *
    type CreateSpec m (a :: k) = ()
    -- | Execute the creation yielding a handle for the created artifact
    runCreate :: p a -> CreateSpec m a -> m (Handle a) -- TODO rename to runCreateHandle??

    -- | An artifact contains stuff. This could be 'Text' for text content
    -- artifacts, or if the artifact is a 'File' the contents could be a
    -- 'ByteString', and a 'CanExtract' instance for 'File' could yield
    -- 'UniqueFileCopy's, which could have content of type
    -- >
    -- > data UFC = UFC { originalFile :: FilePath
    -- >                , uniqueCopies ::[FilePath]
    -- >                }

    -- | Create the initial content of an Artifact.


-- | Declare or define an artifact that implements 'CanCreate' in terms of
--   artifact specific parameters.
create
    :: (CanCreate m a)
    => p a -> CreateSpec m a -> ProgramT m (Handle a)
create sa src = liftF $ Create sa src id

-- * Side-effects based artifact addition/extension

-- | Class of artifacts that can be extended by adding elements to it.
-- No new artifact is created in the process, and the result is in the
-- side-effects. NOTE: Side-effects do not compose very well, use with special
-- care.
class CanAdd m (a1 :: k1) (a2 :: k2)  where
    -- | Type of parameters required to describe how to add an @a2@ to an @a1@.
    type family AddSpec m (a1 :: k1) (a2 :: k2) :: *
    type AddSpec m (a1 :: k1) (a2 :: k2) = ()

    -- | Execute the addition, return no results, since this is only for
    -- side-effects.
    runAdd :: Handle a1 -> p a2 -> AddSpec m a1 a2 -> m ()

-- | Add an artifact to another artifact.
add :: (CanAdd m a b)
    => Handle a -> p b -> AddSpec m a b -> ProgramT m ()
add hndA sB addSpec = liftF $
    Add hndA sB addSpec ()

-- * Artifact to Artifact copy/clone/conversion

-- | Class of artifacts that can be copied or converted into a possibly
-- different type of new artifact, for which a handle is returned. The old
-- artifact does not change.
class CanExtract m (a :: k1) (b :: k2)  where
    -- | Conversion parameter type
    type family ExtractionArg m (a :: k1) (b :: k2) :: *
    type ExtractionArg m (a :: k1) (b :: k2) = ()
    -- | Execute the conversion yielding a handle for the converted artifact
    runExtract :: Handle a -> p b -> ExtractionArg m a b -> m (Handle b)

-- | Extract an artifact referenced by a handle to a different kind
--  of artifact and return the handle of the new artifact.
extract :: (CanExtract m a b)
        => Handle a -> p b -> ExtractionArg m a b -> ProgramT m (Handle b)
extract hndA sB convSpec = liftF $
    Extract hndA sB convSpec id

-- * Artifact output generation

-- | Class of artifacts that have a export funtion to render an actual
-- persistent output. Only this class can be relied upon to indicate actual
-- build output generation.
class CanExport m (a :: k)  where
    -- | Type of the parameter that can be specified when exporting an artifact.
    type family ExportSpec m (a :: k) :: *
    type ExportSpec m (a :: k) = ()
    -- | Export the artifact to the given destination.
    runExport :: Handle a -> ExportSpec m a -> m ()

-- | Export an artifact referenced by a handle a to a /real/ output,
-- i.e. something that is not necessarily referenced to by 'Handle'.
export :: (CanExport m a)
       => Handle a -> ExportSpec m a -> ProgramT m ()
export hnd out = liftF $
    Export hnd out ()

-- * DSL Interpreter

-- | Interpret a 'ProgramT' into a sequence of actions of a monad @m@.
interpret :: forall m a. Monad m => ProgramT m a -> m a
interpret = foldFree go
  where
    go :: BuildStep m x -> m x
    go (Create sa src k) = do
        hnd <- runCreate sa src
        return (k hnd)
    go (Add hndA sB addSpec next) = do
        runAdd hndA sB addSpec
        return next
    go (Extract hndA sB convSpec k) = do
        hnd <- runExtract hndA sB convSpec
        return (k hnd)
    go (Export hnd exportSpec next) = do
        runExport hnd exportSpec
        return next

-- * Logging

-- | The internal data type to represent the log output to the type checker.
data LoggingArtifact = LoggingOutput | LogEvent deriving Show

-- | A Global handle repesenting the (symbolic) logging output. Currently, the
-- logging output is determined by b9 conifiguration rather than explicit code.
loggingH :: Handle 'LoggingOutput
loggingH = globalHandleP (Proxy :: Proxy 'LoggingOutput)

-- | Log messages using the logging API defined in 'B9.Logging'
instance (CanAdd m 'LoggingOutput 'LogEvent, a ~ ())
         => CanLog (ProgramT m a) where
    logMsg l msg = add loggingH (Proxy :: Proxy 'LogEvent) (l, msg)

instance (CanLog (m ())) => CanAdd m 'LoggingOutput 'LogEvent where
    type AddSpec m 'LoggingOutput 'LogEvent = (LogLevel, String)
    runAdd _ _ (lvl,msg) = logMsg lvl msg
