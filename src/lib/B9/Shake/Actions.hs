-- | Convenient Shake 'Action's for 'B9' rules.
module B9.Shake.Actions (b9InvokationAction, buildB9File) where

import Development.Shake
import Development.Shake.FilePath
import B9

-- | Convert a 'B9Invokation' action into a Shake 'Action'.
b9InvokationAction :: B9Invokation -> ReaderT B9Config IO a -> Action a
b9InvokationAction invokation act = liftIO (invokeB9 invokation act)

-- | An action that does the equivalent of
-- @b9c build -f <b9file> -- (args !! 0) (args !! 1) ... (args !! (length args - 1))@
-- with the current working directory changed to @b9Root@.
-- The return value is the buildid, see 'getBuildId'
buildB9File :: FilePath -> FilePath -> [String] -> Action String
buildB9File b9Root b9File args = do
    let f = b9Root </> b9File
    need [f]
    b9InvokationAction
        ( defaultB9Invokation
        & runtimeInvokationConfig %~ appendPositionalArguments args
        & overrideWorkingDirectory b9Root
        )
        (runBuildArtifacts [f])