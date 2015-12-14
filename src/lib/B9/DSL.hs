{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Install and deploy VM-images using a simple DSL.
module B9.DSL
       (module X, BuiltInArtifact(..), SBuiltInArtifact, Sing(..),
        imageRepositoryH, externalFile, use, fromFile, outputFile,
        variableBindings, ($=), addFile, addExe, addFileP, addTemplate,
        addTemplateP, addTemplateExe, addFileFull, addFileFromContent,
        createContent, appendContent, newDirectory, exportDir,
        newCloudInit, addMetaData, addUserData, writeCloudInitDir,
        writeCloudInitDir', writeCloudInit, addCloudInitToArtifact,
        fromShared, sharedAs, boot, lxc, lxc32, mountDir, mountDirRW,
        mount, runCommand, sh, rootImage, dataImage,
        mountAndShareSharedImage, mountAndShareNewImage)
       where

import B9.CommonTypes     as X
import B9.Content         as X
import B9.DslCore         as X
import B9.DiskImages      as X
import B9.ExecEnv         as X
import B9.FileSystems     as X
import B9.Logging         as X
import B9.PartitionTable  as X
import B9.Repository      as X
import B9.ShellScript     as X (Script(..))
import Control.Lens       hiding ((#), from, use, (<.>), uncons)
import Data.Binary
import Data.Functor       (void)
import Data.Singletons.TH
import System.FilePath
import Text.Printf        (printf)

-- * Pre-defined, artifacts

-- | Singletons for the artifacts provided by B9 and used througout this DSL.
$(singletons
  [d|
    data BuiltInArtifact
     = VmImage
     | UpdateServerRoot
     | PartitionedVmImage
     | CloudInit
     | CloudInitMetaData
     | CloudInitUserData
     | ExecutionEnvironment
     | TemplateVariable
     | ExecutableScript
     | GeneratedContent
     | VariableBindings
     | LocalDirectory
     | ExternalFile
     | FreeFile
     | FileSystemBuilder
     | FileSystemImage
     | ImageRepository
     deriving (Show)
   |])

-- * Creation type families

-- | Artifact specific parameters for 'create'
type instance CreateSpec 'CloudInit            = String
type instance CreateSpec 'ExecutionEnvironment = ExecEnvSpec
type instance CreateSpec 'ExternalFile         = FilePath
type instance CreateSpec 'FileSystemBuilder    = FileSystemSpec
type instance CreateSpec 'FreeFile             = Maybe String
type instance CreateSpec 'GeneratedContent     = (Content, String)
type instance CreateSpec 'LocalDirectory       = ()

-- | Parameters for 'add'. They may depend on both artifact types.
type instance AddSpec 'CloudInit 'CloudInitMetaData           = AST Content YamlObject
type instance AddSpec 'CloudInit 'CloudInitUserData           = AST Content YamlObject
type instance AddSpec 'CloudInit 'ExecutableScript            = Script
type instance AddSpec 'CloudInit 'FreeFile                    = (FileSpec, Handle 'FreeFile)
type instance AddSpec 'ExecutionEnvironment 'ExecutableScript = Script
type instance AddSpec 'ExecutionEnvironment 'FreeFile         = (FileSpec, Handle 'FreeFile)
type instance AddSpec 'ExecutionEnvironment 'LocalDirectory   = SharedDirectory
type instance AddSpec 'FileSystemBuilder 'FreeFile            = (FileSpec, Handle 'FreeFile)
type instance AddSpec 'GeneratedContent 'GeneratedContent     = Content
type instance AddSpec 'ImageRepository 'VmImage               = (SharedImageName, Handle 'VmImage)
type instance AddSpec 'LocalDirectory 'FreeFile               = (FileSpec, Handle 'FreeFile)
type instance AddSpec 'UpdateServerRoot 'VmImage              = (SharedImageName, Handle 'VmImage)
type instance AddSpec 'VariableBindings 'TemplateVariable     = (String, String)

-- | Parameters for 'convert'. They may depend on both artifact types.
type instance ConvSpec 'CloudInit 'CloudInitMetaData        = ()
type instance ConvSpec 'CloudInit 'CloudInitUserData        = ()
type instance ConvSpec 'CloudInitMetaData 'GeneratedContent = ()
type instance ConvSpec 'CloudInitUserData 'GeneratedContent = ()
type instance ConvSpec 'ExecutionEnvironment 'VmImage       = (Handle 'VmImage, MountPoint)
type instance ConvSpec 'ExecutionEnvironment 'FreeFile      = FilePath
type instance ConvSpec 'ExternalFile 'FreeFile              = ()
type instance ConvSpec 'FileSystemBuilder 'FileSystemImage  = ()
type instance ConvSpec 'FileSystemBuilder 'FreeFile         = ()
type instance ConvSpec 'FileSystemBuilder 'VmImage          = ()
type instance ConvSpec 'FileSystemImage 'FileSystemImage    = FileSystemResize
type instance ConvSpec 'FileSystemImage 'FreeFile           = ()
type instance ConvSpec 'FileSystemImage 'VmImage            = ()
type instance ConvSpec 'FreeFile 'ExternalFile              = FilePath
type instance ConvSpec 'FreeFile 'FileSystemImage           = FileSystem
type instance ConvSpec 'FreeFile 'FreeFile                  = String
type instance ConvSpec 'FreeFile 'PartitionedVmImage        = ()
type instance ConvSpec 'FreeFile 'VmImage                   = ImageType
type instance ConvSpec 'GeneratedContent 'FreeFile          = ()
type instance ConvSpec 'ImageRepository 'VmImage            = SharedImageName
type instance ConvSpec 'LocalDirectory 'UpdateServerRoot    = ()
type instance ConvSpec 'PartitionedVmImage 'FreeFile        = PartitionSpec
type instance ConvSpec 'VmImage 'FileSystemImage            = ()
type instance ConvSpec 'VmImage 'FreeFile                   = ()
type instance ConvSpec 'VmImage 'VmImage                    = Either ImageType ImageSize

-- | Artifact export parameters.
type instance ExportSpec 'FileSystemImage = FilePath
type instance ExportSpec 'FreeFile        = FilePath
type instance ExportSpec 'LocalDirectory  = FilePath
type instance ExportSpec 'VmImage         = FilePath

-- * Incorporating external files

-- | Reference an external file. An external file will never be modified.
externalFile :: CanCreate m 'ExternalFile => FilePath -> ProgramT m (Handle 'ExternalFile)
externalFile = create SExternalFile

-- | Get a reference to a copy of an external file inside the build root that
-- can be modified.
use :: (CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile)
       => FilePath -> ProgramT m (Handle 'FreeFile)
use f = do
    extH <- externalFile f
    convert extH SFreeFile ()

-- | 'use' an external file to introduce an artifact with the help of a
--  artifact dependent extra arguement and return the artifacts handle.
fromFile
    :: (Show (ConvSpec 'FreeFile b),
        CanCreate m 'ExternalFile,
        CanConvert m 'FreeFile b,
        CanConvert m 'ExternalFile 'FreeFile)
       => FilePath -> Sing b -> ConvSpec 'FreeFile b -> ProgramT m (Handle b)
fromFile f a conversionArg = do
    h <- use f
    convert h a conversionArg

-- | Given an artifact that support extraction or conversion to a file
-- create/write a file to a given output path.
outputFile
    :: (Show (ConvSpec a 'FreeFile), CanConvert m a 'FreeFile,
        CanExport m 'FreeFile)
       => Handle a -> ConvSpec a 'FreeFile -> FilePath -> ProgramT m ()
outputFile e src dst = do
    outF <- convert e SFreeFile src
    export outF dst

-- * Template support

-- | A handle representing the environment holding all template variable
-- bindings.
variableBindings :: Handle 'VariableBindings
variableBindings = globalHandle SVariableBindings

-- | Create a template variable binding. The bindings play a role in generated
-- 'Content' and in the 'addTemplate' (and similar) functions.
($=) :: CanAdd m 'VariableBindings 'TemplateVariable
        => String -> String -> ProgramT m ()
var $= val = add variableBindings STemplateVariable (var, val)

-- * File Inclusion, File-Templating and Script Rendering

-- | Add an existing file to an artifact.
-- Strip the directories from the path, e.g. @/etc/blub.conf@ will be
-- @blob.conf@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addFile
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addFile d f = addFileP d f (0, 6, 4, 4)

-- | Same as 'addFile' but set the destination file permissions to @0755@
-- (executable for all).
addExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addExe d f = addFileP d f (0, 7, 5, 5)

-- | Same as 'addFile' but with an extra output file permission parameter.
addFileP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile)
       => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> ProgramT m ()
addFileP dstH f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
    origH <- create SExternalFile f
    tmpH <- convert origH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpH)

-- | Generate a file to an artifact from a local file template.
-- All occurences of @${var}@ will be replaced by the contents of @var@, which
-- is the last values assigned to @var@ using @"var" $= "123"@. The directory
-- part is stripped from the output file name, e.g. @template/blah/foo.cfg@ will
-- be @foo.cfg@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addTemplate
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addTemplate d f = addTemplateP d f (0,6,4,4)

-- | Same as 'addTemplate' but set the destination file permissions to @0755@
-- (executable for all).
addTemplateExe
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addTemplateExe d f = addTemplateP d f (0, 6, 4, 4)

-- | Same as 'addTemplate' but with an extra output file permission parameter.
addTemplateP
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile)
       => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> ProgramT m ()
addTemplateP dstH f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
        srcFile = Source ExpandVariables f
    addFileFromContent dstH (FromTextFile srcFile) dstSpec

-- | Add an existing file from the file system, optionally with template
-- variable expansion to an artifact at a 'FileSpec'.
addFileFull
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanCreate m 'ExternalFile
       ,CanConvert m 'ExternalFile 'FreeFile
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent)
       => Handle e -> SourceFile -> FileSpec -> ProgramT m ()
addFileFull dstH srcFile dstSpec =
    case srcFile of
        (Source ExpandVariables _) ->
            addFileFromContent dstH (FromTextFile srcFile) dstSpec
        (Source NoConversion f) -> do
            origH <- create SExternalFile f
            tmpH <- convert origH SFreeFile ()
            add dstH SFreeFile (dstSpec, tmpH)

-- | Generate a file with a content and add that file to an artifact at a
-- 'FileSpec'.
addFileFromContent
    :: (AddSpec e 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m e 'FreeFile
       ,CanCreate m 'GeneratedContent)
       => Handle e -> Content -> FileSpec -> ProgramT m ()
addFileFromContent dstH content dstSpec = do
    cH <-
        createContent
            content
            (printf
                 "contents-of-%s"
                 (dstSpec ^. fileSpecPath . to takeFileName))
    tmpFileH <- convert cH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpFileH)

-- * /Low-level/ 'Content' generation functions

-- | Create a handle for accumulating 'Content' with an initial 'Content'.
createContent :: (CanCreate m 'GeneratedContent)
                 => Content -> String -> ProgramT m (Handle 'GeneratedContent)
createContent c title = create SGeneratedContent (c, title)

-- | Accumulate/Append more 'Content' to the 'GeneratedContent'
--   handle obtained by e.g. 'createContent'
appendContent :: (CanAdd m 'GeneratedContent 'GeneratedContent)
                 => Handle 'GeneratedContent -> Content -> ProgramT m ()
appendContent hnd = add hnd SGeneratedContent

-- * directories

-- | Create a temp directory
newDirectory :: (CanCreate m 'LocalDirectory)
                => ProgramT m (Handle 'LocalDirectory)
newDirectory = create SLocalDirectory ()

-- | Render the directory to the actual destination (which must not exist)
exportDir :: (CanExport m 'LocalDirectory)
             => Handle 'LocalDirectory -> FilePath -> ProgramT m ()
exportDir = export

-- * cloud init

newCloudInit :: (CanCreate m 'CloudInit)
                => String -> ProgramT m (Handle 'CloudInit)
newCloudInit = create SCloudInit

addMetaData :: (CanAdd m 'CloudInit 'CloudInitMetaData)
               => Handle 'CloudInit -> AST Content YamlObject -> ProgramT m ()
addMetaData hnd = add hnd SCloudInitMetaData

addUserData :: (CanAdd m 'CloudInit 'CloudInitUserData)
               => Handle 'CloudInit -> AST Content YamlObject -> ProgramT m ()
addUserData hnd = add hnd SCloudInitUserData

writeCloudInitDir :: (CanCreate m 'LocalDirectory
                     ,CanExport m 'LocalDirectory
                     ,CanAdd m 'LocalDirectory 'FreeFile
                     ,CanConvert m 'CloudInit 'CloudInitMetaData
                     ,CanConvert m 'CloudInit 'CloudInitUserData
                     ,CanConvert m 'CloudInitMetaData 'GeneratedContent
                     ,CanConvert m 'CloudInitUserData 'GeneratedContent
                     ,CanConvert m 'GeneratedContent 'FreeFile)
                     => Handle 'CloudInit -> FilePath -> ProgramT m ()
writeCloudInitDir h = void . writeCloudInitDir' h

writeCloudInitDir' :: (CanCreate m 'LocalDirectory
                      ,CanExport m 'LocalDirectory
                      ,CanAdd m 'LocalDirectory 'FreeFile
                      ,CanConvert m 'CloudInit 'CloudInitMetaData
                      ,CanConvert m 'CloudInit 'CloudInitUserData
                      ,CanConvert m 'CloudInitMetaData 'GeneratedContent
                      ,CanConvert m 'CloudInitUserData 'GeneratedContent
                      ,CanConvert m 'GeneratedContent 'FreeFile)
                      => Handle 'CloudInit -> FilePath -> ProgramT m ()
writeCloudInitDir' h dst = do
    dirH <- newDirectory
    addCloudInitToArtifact h dirH
    export dirH dst

writeCloudInit :: (CanCreate m 'FileSystemBuilder
                  ,CanConvert m 'FileSystemBuilder 'FileSystemImage
                  ,CanExport m 'FileSystemImage
                  ,CanAdd m 'LocalDirectory 'FreeFile
                  ,CanAdd m 'FileSystemBuilder 'FreeFile
                  ,CanConvert m 'CloudInit 'CloudInitMetaData
                  ,CanConvert m 'CloudInit 'CloudInitUserData
                  ,CanConvert m 'CloudInitMetaData 'GeneratedContent
                  ,CanConvert m 'CloudInitUserData 'GeneratedContent
                  ,CanConvert m 'GeneratedContent 'FreeFile)
                  => Handle 'CloudInit
                  -> FileSystem
                  -> FilePath
                  -> ProgramT m ()
writeCloudInit h fs dst = do
    fsBuilder <- create SFileSystemBuilder (FileSystemSpec fs "cidata" 2 MB)
    fsImage <- convert fsBuilder SFileSystemImage ()
    export fsImage dst
    addCloudInitToArtifact h fsBuilder

addCloudInitToArtifact
    :: (AddSpec a 'FreeFile ~ (FileSpec, Handle 'FreeFile)
       ,CanConvert m 'CloudInit 'CloudInitMetaData
       ,CanConvert m 'CloudInit 'CloudInitUserData
       ,CanConvert m 'CloudInitMetaData 'GeneratedContent
       ,CanConvert m 'CloudInitUserData 'GeneratedContent
       ,CanConvert m 'GeneratedContent 'FreeFile
       ,CanAdd m a 'FreeFile)
    => Handle 'CloudInit -> Handle a -> ProgramT m ()
addCloudInitToArtifact chH destH = do
    metaData <- convert chH SCloudInitMetaData ()
    metaDataContent <- convert metaData SGeneratedContent ()
    metaDataFile <- convert metaDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "meta-data", metaDataFile)
    userData <- convert chH SCloudInitUserData ()
    userDataContent <- convert userData SGeneratedContent ()
    userDataFile <- convert userDataContent SFreeFile ()
    add destH SFreeFile (fileSpec "user-data", userDataFile)

-- * Image import

-- | A Global handle repesenting the (local) share image repository.
imageRepositoryH :: Handle 'ImageRepository
imageRepositoryH = globalHandle SImageRepository

fromShared :: (CanConvert m 'ImageRepository 'VmImage)
              => String -> ProgramT m (Handle 'VmImage)
fromShared sharedImgName = convert
        imageRepositoryH
        SVmImage
        (SharedImageName sharedImgName)

-- * Image export

-- | Store an image in the local cache with a name as key for lookups, e.g. from
-- 'fromShared'
sharedAs :: (CanAdd m 'ImageRepository 'VmImage)
            => Handle 'VmImage -> String -> ProgramT m ()
sharedAs hnd name = add imageRepositoryH SVmImage (SharedImageName name, hnd)

-- * Execution environment

boot :: (CanCreate m 'ExecutionEnvironment)
        => ExecEnvSpec -> ProgramT m (Handle 'ExecutionEnvironment)
boot = create SExecutionEnvironment

lxc :: (CanCreate m 'ExecutionEnvironment)
       => String -> ProgramT m (Handle 'ExecutionEnvironment)
lxc name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 X86_64)

lxc32 :: (CanCreate m 'ExecutionEnvironment)
         => String -> ProgramT m (Handle 'ExecutionEnvironment)
lxc32 name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 I386)

-- * Mounting

mountDir :: (CanAdd m 'ExecutionEnvironment 'LocalDirectory)
            => Handle 'ExecutionEnvironment
            -> FilePath
            -> FilePath
            -> ProgramT m ()
mountDir e hostDir dest =
    add e SLocalDirectory (SharedDirectoryRO hostDir (MountPoint dest))

mountDirRW :: (CanAdd m 'ExecutionEnvironment 'LocalDirectory)
              => Handle 'ExecutionEnvironment
              -> FilePath
              -> FilePath
              -> ProgramT m ()
mountDirRW e hostDir dest =
    add e SLocalDirectory (SharedDirectory hostDir (MountPoint dest))

mount
    :: (CanConvert m 'ExecutionEnvironment 'VmImage)
       => Handle 'ExecutionEnvironment
       -> Handle 'VmImage
       -> FilePath
       -> ProgramT m (Handle 'VmImage)
mount e imgHnd dest = convert e SVmImage (imgHnd, MountPoint dest)

-- * Script Execution (inside a container)

runCommand
    :: (Show (AddSpec a 'ExecutableScript)
       ,CanAdd m a 'ExecutableScript)
       => Handle a -> AddSpec a 'ExecutableScript -> ProgramT m ()
runCommand hnd = add hnd SExecutableScript

sh
    :: (AddSpec a 'ExecutableScript ~ Script
       ,CanAdd m a 'ExecutableScript)
    => Handle a -> String -> ProgramT m ()
sh e s = runCommand e (Run s [])

-- * Some utility vm builder lego

rootImage :: (CanConvert m 'ImageRepository 'VmImage
             ,CanConvert m 'ExecutionEnvironment 'VmImage
             ,CanAdd m 'ImageRepository 'VmImage)
             => String
             -> String
             -> Handle 'ExecutionEnvironment
             -> ProgramT m ()
rootImage nameFrom nameExport env =
    void $ mountAndShareSharedImage nameFrom nameExport "/" env

dataImage :: (CanConvert m 'GeneratedContent 'FreeFile
             ,CanCreate m 'FileSystemBuilder
             ,CanConvert m 'FileSystemBuilder 'FileSystemImage
             ,CanConvert m 'FileSystemImage 'VmImage
             ,CanAdd m 'ImageRepository 'VmImage
             ,CanConvert m 'ExecutionEnvironment 'VmImage)
             => String -> Handle 'ExecutionEnvironment -> ProgramT m ()
dataImage nameExport env =
    void $ mountAndShareNewImage "data" 64 nameExport "/data" env

mountAndShareSharedImage :: (CanConvert m 'ImageRepository 'VmImage
                            ,CanConvert m 'ExecutionEnvironment 'VmImage
                            ,CanAdd m 'ImageRepository 'VmImage)
                            => String
                            -> String
                            -> String
                            -> Handle 'ExecutionEnvironment
                            -> ProgramT m ()
mountAndShareSharedImage nameFrom nameTo mountPoint env = do
    i <- fromShared nameFrom
    i' <- mount env i mountPoint
    i' `sharedAs` nameTo

mountAndShareNewImage
    :: (CanConvert m 'GeneratedContent 'FreeFile
       ,CanCreate m 'FileSystemBuilder
       ,CanConvert m 'FileSystemBuilder 'FileSystemImage
       ,CanConvert m 'FileSystemImage 'VmImage
       ,CanAdd m 'ImageRepository 'VmImage
       ,CanConvert m 'ExecutionEnvironment 'VmImage)
    => String
    -> Int
    -> String
    -> FilePath
    -> Handle 'ExecutionEnvironment
    -> ProgramT m ()
mountAndShareNewImage label sizeGB nameTo mountPoint env = do
    fs <-
        create SFileSystemBuilder (FileSystemSpec Ext4 label sizeGB GB)
    fi <- convert fs SFileSystemImage ()
    i <- convert fi SVmImage ()
    i' <- mount env i mountPoint
    i' `sharedAs` nameTo
