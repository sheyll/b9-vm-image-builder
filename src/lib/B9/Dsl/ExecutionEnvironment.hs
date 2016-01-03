module B9.Dsl.ExecutionEnvironment where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.DiskImages
import B9.Dsl.Content
import B9.Dsl.Core
import B9.ExecEnv
import B9.ShellScript
import B9.ShellScript       (toBashOneLiner)
import Control.Lens         hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Data
import Data.Default
import Data.Graph           as Graph
import Data.Map             as Map hiding (null)
import Data.Monoid
import Data.Singletons
import Data.Singletons.TH
import Data.Tree            as Tree
import System.FilePath
import Text.Printf          (printf)

-- | Singletons for the execuition environment library.
$(singletons
  [d|
    data ExecutionEnvironment
     = ExecutionEnvironment
      deriving (Show)
   |])

-- | Context of a 'ExecutionEnvironment'
data ExecEnvCtx = ExecEnvCtx
    { _execImages :: [Mounted Image]
    , _execBindMounts :: [SharedDirectory]
    , _execScript :: Script
    , _execIncFiles :: [(FilePath, FileSpec)]
    , _execIncDir :: FilePath
    , _execOutMnt :: FilePath
    , _execOutDir :: FilePath
    , _execOutFiles :: [(FilePath,FilePath)]
    , _execEnvSpec :: ExecEnvSpec
    } deriving (Show, Typeable)

instance Default ExecEnvCtx where
    def = ExecEnvCtx def def def def def def def def def

makeLenses ''ExecEnvCtx

instance CanCreate IoCompiler 'ExecutionEnvironment where
    runCreate _ e = do
        (hnd,_) <- allocHandle SExecutionEnvironment (e ^. execEnvTitle)
        incDir <- lift (mkTempDir "included-files")
        outDir <- lift (mkTempDir "output-files")
        buildId <- lift B9.B9IO.getBuildId
        let outMnt = outputFileContainerPath buildId
            incMnt = includedFileContainerPath buildId
        putArtifactState
            hnd
            (def &~
             do execEnvSpec .= e
                execIncDir .= incDir
                execOutDir .= outDir
                execOutMnt .= outMnt
                execBindMounts .=
                    [ SharedDirectoryRO incDir (MountPoint incMnt)
                    , SharedDirectory outDir (MountPoint outMnt)])
        addAction
            hnd
            (do Just es <- getArtifactState hnd
                let copyOutFileScript = foldMap cp (es ^. execOutFiles)
                      where
                        cp (guestFrom,hostOut) =
                            Run "cp" [guestFrom, toMntPath hostOut]
                        toMntPath = (es ^. execOutMnt </>) . takeFileName
                lift
                    (executeInEnv
                         (es ^. execEnvSpec)
                         (es ^. execScript <> copyOutFileScript)
                         (es ^. execBindMounts)
                         (es ^. execImages)))
        return hnd

instance CanAdd IoCompiler 'ExecutionEnvironment 'ExecutableScript where
    runAdd hnd _ cmds =
        modifyArtifactState hnd $ traverse . execScript <>~ cmds

instance CanAdd IoCompiler 'ExecutionEnvironment 'FreeFile where
    runAdd hnd _ (destSpec,srcH) = do
        srcH --> hnd
        Just eCxt <- useArtifactState hnd
        incFile <-
            lift (mkTempInCreateParents (eCxt ^. execIncDir) "added-file")
        copyFreeFile srcH incFile
        modifyArtifactState hnd $ traverse . execIncFiles <>~
            [(incFile, destSpec)]
        bId <- lift B9.B9IO.getBuildId
        modifyArtifactState hnd $ traverse . execScript <>~
            incFileScript bId incFile destSpec

instance CanAdd IoCompiler 'ExecutionEnvironment 'LocalDirectory where
    runAdd hnd _ sharedDir =
        modifyArtifactState hnd $ traverse . execBindMounts <>~ [sharedDir]

instance CanConvert IoCompiler 'ExecutionEnvironment 'FreeFile where
    runConvert hnd _ src = do
        Just ec <- useArtifactState hnd
        (fh,f) <-
            createFreeFileIn
                (ec ^. execOutDir)
                (printf
                     "%s-%s"
                     (ec ^. execEnvSpec . execEnvTitle)
                     (takeFileName src))
        modifyArtifactState hnd $ traverse . execOutFiles <>~ [(src, f)]
        hnd --> fh
        return fh

-- | Return the mount point for files from the build host to be
-- included in the container.
includedFileContainerPath :: String -> FilePath
includedFileContainerPath buildId =
    "/" ++ buildId <.> "mnt" </> "included-files"

-- | Return the mount point for files that are copied after the build from the container
--   to the host.
outputFileContainerPath :: String -> FilePath
outputFileContainerPath buildId =
    "/" ++ buildId <.> "mnt" </> "output-files"
