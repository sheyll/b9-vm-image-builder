-- | Convenient Shake 'Action's for 'B9' rules.
module B9.Shake.Actions (b9InvokationAction, buildB9File) where

import Development.Shake
import Development.Shake.FilePath
import B9

-- | Convert a 'B9Invokation' action into a Shake 'Action'. This is just
-- an alias for 'execB9ConfigAction'.
b9InvokationAction :: B9ConfigAction Action a -> B9ConfigOverride -> Action a
b9InvokationAction = execB9ConfigAction

-- | An action that does the equivalent of
-- @b9c build -f <b9file> -- (args !! 0) (args !! 1) ... (args !! (length args - 1))@
-- with the current working directory changed to @b9Root@.
-- The return value is the buildid, see 'getBuildId'
buildB9File :: FilePath -> FilePath -> [String] -> Action String
buildB9File b9Root b9File args = do
    let f = b9Root </> b9File
    need [f]
    invokeB9
        ( localRuntimeConfig
            (appendPositionalArguments args . (buildDirRoot .~ Just b9Root))
            (runBuildArtifacts [f])
        )