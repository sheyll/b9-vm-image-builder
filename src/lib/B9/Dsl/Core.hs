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
        (Show (CreateSpec a), CanCreate m a) =>
        p a -> CreateSpec a -> (Handle a -> next) -> BuildStep m next
    Add ::
        (Show (AddSpec a b), CanAdd m a b) =>
        Handle a ->
          p b -> AddSpec a b -> next -> BuildStep m next
    Convert ::
        (Show (ConvSpec a b), CanConvert m a b) =>
        Handle a ->
          p b -> ConvSpec a b -> (Handle b -> next) -> BuildStep m next
    Export ::
        (Show (ExportSpec a), CanExport m a) =>
        Handle a ->
          ExportSpec a -> next -> BuildStep m next

-- | The requirement to create a free monad of a type is a functor instance.
instance Functor (BuildStep m) where
    fmap f (Create sa src k)            = Create sa src (f . k)
    fmap f (Add hndEnv sa addSpec next) = Add hndEnv sa addSpec (f next)
    fmap f (Convert hA sB conv k)       = Convert hA sB conv (f . k)
    fmap f (Export hnd out next)        = Export hnd out (f next)

-- * Artifact Creation

-- | Contruction parameters required to create the artifact
type family CreateSpec (a :: k) :: *

-- | Class of artifacts that can be created.
class CanCreate m (a :: k)  where
    -- | Execute the creation yielding a handle for the created artifact
    runCreate :: p a -> CreateSpec a -> m (Handle a)

-- | Declare or define an artifact that implements 'CanCreate' in terms of
--   artifact specific parameters.
create
    :: (Show (CreateSpec a), CanCreate m a)
    => p a -> CreateSpec a -> ProgramT m (Handle a)
create sa src = liftF $ Create sa src id

-- * Side-effects based artifact addition/extension

-- | Type of parameters required to describe how to add an @a2@ to an @a1@.
type family AddSpec (a1 :: k1) (a2 :: k2) :: *

-- | Class of artifacts that can be extended by adding elements to it.
-- No new artifact is created in the process, and the result is in the
-- side-effects. NOTE: Side-effects do not compose very well, use with special
-- care.
class CanAdd m (a1 :: k1) (a2 :: k2)  where
    -- | Execute the addition, return no results, since this is only for
    -- side-effects.
    runAdd :: Handle a1 -> p a2 -> AddSpec a1 a2 -> m ()

-- | Add an artifact to another artifact.
add :: (Show (AddSpec a b),CanAdd m a b)
    => Handle a -> p b -> AddSpec a b -> ProgramT m ()
add hndA sB addSpec = liftF $
    Add hndA sB addSpec ()

-- * Artifact to Artifact copy/clone/conversion

-- | Conversion parameter type
type family ConvSpec (a :: k1) (b :: k2) :: *

-- | Class of artifacts that can be copied or converted into a possibly
-- different type of new artifact, for which a handle is returned. The old
-- artifact does not change.
class CanConvert m (a :: k1) (b :: k2)  where
    -- | Execute the conversion yielding a handle for the converted artifact
    runConvert :: Handle a -> p b -> ConvSpec a b -> m (Handle b)

-- | Convert an artifact referenced by a handle to a different kind
--  of artifact and return the handle of the new artifact.
convert :: (Show (ConvSpec a b),CanConvert m a b)
        => Handle a -> p b -> ConvSpec a b -> ProgramT m (Handle b)
convert hndA sB convSpec = liftF $
    Convert hndA sB convSpec id

-- * Artifact output generation

-- | Type of the parameter that can be specified when exporting an artifact.
type family ExportSpec (a :: k) :: *

-- | Class of artifacts that have a export funtion to render an actual
-- persistent output. Only this class can be relied upon to indicate actual
-- build output generation.
class CanExport m (a :: k)  where
    runExport :: Handle a -> ExportSpec a -> m ()

-- | Export an artifact referenced by a handle a to a /real/ output,
-- i.e. something that is not necessarily referenced to by 'Handle'.
export :: (Show (ExportSpec a),CanExport m a)
       => Handle a -> ExportSpec a -> ProgramT m ()
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
    go (Convert hndA sB convSpec k) = do
        hnd <- runConvert hndA sB convSpec
        return (k hnd)
    go (Export hnd exportSpec next) = do
        runExport hnd exportSpec
        return next

-- * Logging

-- | The internal data type to represent the log output to the type checker.
data LoggingArtifact = LoggingOutput | LogEvent deriving Show

type instance AddSpec 'LoggingOutput 'LogEvent = (LogLevel, String)

-- | A Global handle repesenting the (symbolic) logging output. Currently, the
-- logging output is determined by b9 conifiguration rather than explicit code.
loggingH :: Handle 'LoggingOutput
loggingH = globalHandle (Proxy :: Proxy 'LoggingOutput)

-- | Log messages using the logging API defined in 'B9.Logging'
instance (CanAdd m 'LoggingOutput 'LogEvent, a ~ ())
         => CanLog (ProgramT m a) where
    logMsg l msg = add loggingH (Proxy :: Proxy 'LogEvent) (l, msg)

instance (CanLog (m ())) => CanAdd m 'LoggingOutput 'LogEvent where
    runAdd _ _ (lvl,msg) = logMsg lvl msg
