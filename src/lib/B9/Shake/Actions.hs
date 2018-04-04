-- | Convenient Shake 'Action's for 'B9' rules.
module B9.Shake.Actions (buildB9File) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary as Binary
import B9

-- | An action that does the equivalent of
-- @b9c build -f <b9file> -- (args !! 0) (args !! 1) ... (args !! (length args - 1))@
-- with the current working directory changed to @b9Root@.
buildB9File :: FilePath -> FilePath -> [String] -> Action ()
buildB9File b9Root b9File args = do
  let f = b9Root </> b9File
  need [f]
  success <- liftIO $ runB9 $ defaultB9RunParameters $ do
    modifyInvokationConfig (appendPositionalArguments args)
    overrideWorkingDirectory b9Root
    runBuildArtifacts [f]
  if success then
    return ()
   else
    fail $ "ERROR: Build failed: " ++ f
  -- cmd Shell (Cwd b9Root) "b9c" "-v" "build" "-f" b9File "--" b9ExtraArgs
