module B9.B9Monad
  ( runB9
  , B9
  , B9Eff
  , IsB9
  ) where

import B9.B9Config
import B9.B9Error
import B9.B9Logging
import B9.BuildInfo
import B9.Environment
import B9.Repository
import Control.Eff
import Data.Functor ()

-- | Definition of the B9 monad. See 'B9Eff'.
--
-- This module is used by the _effectful_ functions in this library.
--
-- @since 0.5.65
type B9 a = Eff B9Eff a

-- | Definition of the B9 effect list. It encapsulates logging,
-- a reader for the "B9.B9Config" and access to the
-- current build id, the current build directory and the artifact to build.
--
-- This monad is used by the _effectful_ functions in this library.
--
-- @since 0.5.65
type B9Eff
   = '[ SelectedRemoteRepoReader, RepoCacheReader, BuildInfoReader, LoggerReader, B9ConfigReader, EnvironmentReader, ExcB9, Lift IO]

-- | A constraint that contains all effects of 'B9Eff'
--
-- @since 0.5.65
type IsB9 e = (Lifted IO e, CommandIO e, B9Eff <:: e)

-- | Execute a 'B9' effect and return an action that needs
-- the 'B9Config'.
--
-- @since 0.5.65
runB9 :: B9 a -> B9ConfigAction a
runB9 action = do
  cfg <- getB9Config
  env <- askEnvironment
  lift
    (runLift .
     errorOnException .
     runEnvironmentReader env .
     runB9ConfigReader cfg . withLogger . withBuildInfo . withRemoteRepos . withSelectedRemoteRepo $
     action)