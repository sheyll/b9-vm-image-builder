-- | Convenient Shake 'Action's for 'B9' rules.
module B9.Shake.Actions
  ( b9InvocationAction
  , buildB9File
  ) where

import           B9
import           Control.Lens               ((?~))
import           Development.Shake

-- | Convert a 'B9Invocation' action into a Shake 'Action'. This is just
-- an alias for 'runB9ConfigActionWithOverrides' since 'Action' is an instance of 'MonadIO'
-- and 'runB9ConfigActionWithOverrides' work on any .
b9InvocationAction :: B9ConfigAction a -> B9ConfigOverride -> Action a
b9InvocationAction x y = liftIO (runB9ConfigActionWithOverrides x y)

-- | An action that does the equivalent of
-- @b9c build -f <b9file> -- (args !! 0) (args !! 1) ... (args !! (length args - 1))@
-- with the current working directory changed to @b9Root@.
-- The return value is the buildid, see 'getBuildId'
buildB9File :: FilePath -> FilePath -> [String] -> Action String
buildB9File b9Root b9File args = do
  let f = b9Root </> b9File
  need [f]
  liftIO
    (runB9ConfigAction
       (addLocalPositionalArguments args (localB9Config (projectRoot ?~ b9Root) (runBuildArtifacts [f]))))
