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
module B9.Dsl.Core where

import B9.Logging
import Control.Monad.Free (Free(..), liftF, foldFree)
import Data.Function (on)
import Data.Singletons.TH

-- ---------------------------------------------------------

-- | A program is a free monad of 'BuildStep's over a type constructor @m@.
type ProgramT m = Free (BuildStep m)

-- | The abstract _CRUD_-like GADT underlying all 'ProgramT's.
data BuildStep m next where
    Create ::
        (Show (CreateSpec a), CanCreate m a) =>
        Sing a -> CreateSpec a -> (Handle a -> next) -> BuildStep m next
    Add ::
        (Show (AddSpec a b), CanAdd m a b) =>
        Handle a ->
          Sing b -> AddSpec a b -> next -> BuildStep m next
    Convert ::
        (Show (ConvSpec a b), CanConvert m a b) =>
        Handle a ->
          Sing b -> ConvSpec a b -> (Handle b -> next) -> BuildStep m next
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

-- * Handle for artifacts.

-- | In order to indicate references and dependencies between artifacts, e.g.
--   disk images and lxc execution, inside the (pure and declaretive) 'ProgramT',
-- _handles_ are used, instead of actual artifact specific values (like
-- e.g. file handles, network sockets,...). Handles put an additional layer of
-- indirection and proctection around an actual artifact representation. A
-- handle is a proxy for an artifact singleton type paired with a string
-- identifieing the runtime value referenced to by the handle.
data Handle (a :: k) where
    -- | Create a 'Handle' that contains a string.
    Handle :: (SingKind ('KProxy :: KProxy k),
                Show (Demote (a :: k))) =>
               Sing a -> String -> Handle a
instance Eq (Handle (a :: k)) where
    (Handle _ t) == (Handle _ t') = t == t'
instance Ord (Handle (a :: k)) where
    compare (Handle _ t) (Handle _ t') = compare t t'
instance Show (Handle a) where
    show (Handle p t) = show (fromSing p) ++ "//" ++ t
instance LogArg (Handle a)

-- | Make a handle for some global/static pseudo artifact, like e.g. logging.
-- Specifically, create a handle for types of artifacts that are only ever
-- inhabited by a single value, by simply @show@ing the singleton.
globalHandle :: (SingKind ('KProxy :: KProxy k),
                 Show (Demote (a :: k))) => Sing a -> Handle a
globalHandle s = Handle s (show (fromSing s))

-- | A wrapper around 'Handle' with existential quantification over the
-- artifact type.
data SomeHandle where
        SomeHandle :: Handle a -> SomeHandle

instance Show SomeHandle where
    show (SomeHandle h) = "<" ++ show h ++ ">"

instance Eq SomeHandle where
    (==) = (==) `on` show

instance Ord SomeHandle where
    compare = compare `on` show

-- * Artifact Creation

-- | Contruction parameters required to create the artifact
type family CreateSpec (a :: k) :: *

-- | Class of artifacts that can be created.
class CanCreate m (a :: k)  where
    -- | Execute the creation yielding a handle for the created artifact
    runCreate :: Sing a -> CreateSpec a -> m (Handle a)

-- | Declare or define an artifact that implements 'CanCreate' in terms of
--   artifact specific parameters.
create
    :: (Show (CreateSpec a), CanCreate m a)
    => Sing a -> CreateSpec a -> ProgramT m (Handle a)
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
    runAdd :: Handle a1 -> Sing a2 -> AddSpec a1 a2 -> m ()

-- | Add an artifact to another artifact.
add :: (Show (AddSpec a b),CanAdd m a b)
    => Handle a -> Sing b -> AddSpec a b -> ProgramT m ()
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
    runConvert :: Handle a -> Sing b -> ConvSpec a b -> m (Handle b)

-- | Convert an artifact referenced by a handle to a different kind
--  of artifact and return the handle of the new artifact.
convert :: (Show (ConvSpec a b),CanConvert m a b)
        => Handle a -> Sing b -> ConvSpec a b -> ProgramT m (Handle b)
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
$(singletons [d|
   data LoggingArtifact = LoggingOutput | LogEvent deriving Show
 |])

type instance AddSpec 'LoggingOutput 'LogEvent = (LogLevel, String)

-- | A Global handle repesenting the (symbolic) logging output. Currently, the
-- logging output is determined by b9 conifiguration rather than explicit code.
loggingH :: Handle 'LoggingOutput
loggingH = globalHandle SLoggingOutput

-- | Log messages using the logging API defined in 'B9.Logging'
instance (CanAdd m 'LoggingOutput 'LogEvent, a ~ ())
         => CanLog (ProgramT m a) where
    logMsg l msg = add loggingH SLogEvent (l, msg)

instance (CanLog (m ())) => CanAdd m 'LoggingOutput 'LogEvent where
    runAdd _ _ (lvl,msg) = logMsg lvl msg
