{-| Data types describing the execution environment of virtual machine builds.
'ExecEnv', 'Resources' and 'SharedDirectory' describe how e.g. "LibVirtLXC"
should configure and execute scripts.
    -}
module B9.Dsl.ExecutionEnvironment where

import B9.B9IO
import B9.B9IO.IoCompiler
import B9.Content
import B9.DiskImages
import B9.Dsl.Core
import B9.Dsl.File
import B9.Dsl.VmImage
import B9.ExecEnv
import B9.FileSystems
import B9.ShellScript (Script(..))
import Data.Singletons.TH

-- | Singletons for the execuition environment library.
$(singletons
    [d|

  data ExecutionEnvironment = ExecutionEnvironment
                            | ExecutableScript
                            deriving Show
  |])

instance Show (Sing 'ExecutionEnvironment) where show _ = "ExecutionEnvironment"
instance Show (Sing 'ExecutableScript) where show _ = "ExecutableScript"

-- | Context of a 'ExecutionEnvironment'
data ExecEnvCtx =
  ExecEnvCtx {_execImages :: [Mounted Image]
             ,_execBindMounts :: [SharedDirectory]
             ,_execScript :: Script
             ,_execIncFiles :: [(FilePath,FileSpec)]
             ,_execIncDir :: FilePath
             ,_execOutMnt :: FilePath
             ,_execOutDir :: FilePath
             ,_execOutFiles :: [(FilePath,FilePath)]
             ,_execEnvSpec :: ExecEnvSpec}
  deriving (Show,Typeable)

type instance IoCompilerArtifactState 'ExecutionEnvironment =
     ExecEnvCtx

instance Default ExecEnvCtx where
  def = ExecEnvCtx def def def def def def def def def

makeLenses ''ExecEnvCtx

instance CanCreate IoCompiler 'ExecutionEnvironment where
  type CreateSpec IoCompiler 'ExecutionEnvironment = ExecEnvSpec
  runCreate _ e =
    do (hnd,_) <-
         allocHandle SExecutionEnvironment
                     (e ^. execEnvTitle)
       incDir <- liftIoProgram (mkTempDir "included-files")
       outDir <- liftIoProgram (mkTempDir "output-files")
       buildId <- liftIoProgram B9.B9IO.getBuildId
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
               [SharedDirectoryRO incDir
                                  (MountPoint incMnt)
               ,SharedDirectory outDir
                                (MountPoint outMnt)])
       addAction hnd
                 (do Just es <- getArtifactState hnd
                     let copyOutFileScript = foldMap cp (es ^. execOutFiles)
                           where cp (guestFrom,hostOut) =
                                   Run "cp" [guestFrom,toMntPath hostOut]
                                 toMntPath =
                                   (es ^. execOutMnt </>) . takeFileName
                     liftIoProgram
                       (executeInEnv (es ^. execEnvSpec)
                                     (es ^. execScript <> copyOutFileScript)
                                     (es ^. execBindMounts)
                                     (es ^. execImages)))
       return hnd

instance CanAdd IoCompiler 'ExecutionEnvironment 'ExecutableScript where
  type AddSpec IoCompiler 'ExecutionEnvironment 'ExecutableScript = Script
  runAdd hnd _ cmds = modifyArtifactState hnd $ traverse . execScript <>~ cmds

instance CanAdd IoCompiler 'ExecutionEnvironment 'FreeFile where
  type AddSpec IoCompiler 'ExecutionEnvironment 'FreeFile = (FileSpec, Handle 'FreeFile)
  runAdd hnd _ (destSpec,srcH) =
    do srcH --> hnd
       Just eCxt <- useArtifactState hnd
       incFile <- liftIoProgram (mkTempIn (eCxt ^. execIncDir) "added-file")
       copyFreeFile srcH incFile
       modifyArtifactState hnd $ traverse . execIncFiles <>~
         [(incFile,destSpec)]
       bId <- liftIoProgram B9.B9IO.getBuildId
       modifyArtifactState hnd $ traverse . execScript <>~
         incFileScript bId incFile destSpec

instance CanAdd IoCompiler 'ExecutionEnvironment 'LocalDirectory where
  type AddSpec IoCompiler 'ExecutionEnvironment 'LocalDirectory = SharedDirectory
  runAdd hnd _ sharedDir =
    modifyArtifactState hnd $ traverse . execBindMounts <>~ [sharedDir]

instance CanExtract IoCompiler 'ExecutionEnvironment 'FreeFile where
  type ExtractionArg IoCompiler 'ExecutionEnvironment 'FreeFile = FilePath
  runExtract hnd _ src =
    do Just ec <- useArtifactState hnd
       (fh,f) <-
         createFreeFileIn
           (ec ^. execOutDir)
           (printf "%s-%s"
                   (ec ^. execEnvSpec . execEnvTitle)
                   (takeFileName src))
       modifyArtifactState hnd $ traverse . execOutFiles <>~ [(src,f)]
       hnd --> fh
       return fh

instance CanExtract IoCompiler 'ExecutionEnvironment 'VmImage where
  type ExtractionArg IoCompiler 'ExecutionEnvironment 'VmImage = (Handle 'VmImage, MountPoint)
  runExtract hnd _ (imgH,mp) =
    do rawH <-
         runExtract imgH
                    SVmImage
                    (Left Raw)
       rawH --> hnd
       rawFH <- runExtract rawH SFreeFile ()
       mntH <-
         runExtract rawFH
                    SFreeFile
                    (Just (printf "mounted-at-%s" (printMountPoint mp)))
       Just (FileCtx mnt _) <- useArtifactState mntH
       modifyArtifactState hnd $ traverse . execImages <>~
         [(Image mnt Raw Ext4,mp)]
       hnd --> mntH
       runExtract mntH SVmImage Raw

-- | Generate a 'Script' that copies an included file in a
-- container from the mounted directory to the actual destination.
incFileScript
  :: String -> FilePath -> FileSpec -> Script
incFileScript buildId tmpIncFile fSpec =
  Begin [Run "cp" [srcPath,destPath]
        ,Run "chmod" [printf "%d%d%d%d" s u g o,destPath]
        ,Run "chown" [printf "%s:%s" userName groupName,destPath]]
  where (FileSpec destPath (s,u,g,o) userName groupName) = fSpec
        srcPath = includedFileContainerPath buildId </> incFile
        incFile = takeFileName tmpIncFile

-- | Return the mount point for files from the build host to be
-- included in the container.
includedFileContainerPath :: String -> FilePath
includedFileContainerPath buildId =
  "/" ++ buildId <.> "mnt" </> "included-files"

-- | Return the mount point for files that are copied after the build from the container
--   to the host.
outputFileContainerPath :: String -> FilePath
outputFileContainerPath buildId = "/" ++ buildId <.> "mnt" </> "output-files"
