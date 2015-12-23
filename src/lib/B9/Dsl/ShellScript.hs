module B9.Dsl.ShellScript where

import B9.Dsl.Core
import B9.ShellScript     (Script(..))
import Data.Singletons.TH

$(singletons
  [d|
    data ExecutableScriptArtifact
     = ExecutableScript
     deriving (Show)
   |])

-- * Script Execution (inside a container)

-- | Run a command in an environment.
runCommand
    :: (Show (AddSpec a 'ExecutableScript), CanAdd m a 'ExecutableScript)
    => Handle a -> AddSpec a 'ExecutableScript -> ProgramT m ()
runCommand hnd = add hnd SExecutableScript

-- | Execute a string in a shell inside an environment.
sh
    :: (AddSpec a 'ExecutableScript ~ Script, CanAdd m a 'ExecutableScript)
    => Handle a -> String -> ProgramT m ()
sh e s = runCommand e (Run s [])
