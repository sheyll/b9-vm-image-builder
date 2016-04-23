{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Install and deploy VM-images using a simple DSL.
module B9.Dsl
       (module X,
        externalFile, externalFileTempCopy, fromFile, outputFile, addFile,
        addExe, addFileP, newDirectory, exportDir, createContent,
        appendContent, addTemplate, addTemplateP, addTemplateExe,
        addFileFull, addFileFromContent, runCommand, sh, boot, lxc, lxc32,
        mount, mountDir, mountDirRW, fromShared, sharedAs, rootImage,
        dataImage, mountAndShareSharedImage, mountAndShareNewImage)
       where

import B9.CommonTypes              as X
import B9.Content                  as X
import B9.DiskImages               as X
import B9.Dsl.CloudInit            as X
import B9.Dsl.Content              as X
import B9.Dsl.Core                 as X
import B9.Dsl.ExecutionEnvironment as X
import B9.Dsl.File                 as X
import B9.Dsl.FileSystem           as X
import B9.Dsl.ImageRepository      as X
import B9.Dsl.Logging              as X
import B9.Dsl.PartitionedVmImage   as X
import B9.Dsl.UpdateServerRoot     as X
import B9.Dsl.VmImage              as X
import B9.ExecEnv                  as X
import B9.FileSystems              as X
import B9.Logging                  as X
import B9.PartitionTable           as X
import B9.Repository               as X
import B9.ShellScript              as X (Script(..))
import Control.Lens
import Data.Functor                (void)
import Data.Singletons             (Sing)
import Data.Word
import System.FilePath
import Text.Printf

-- * Cloud-init
-- TODO

-- * Adding Files to an Artifact

-- | Reference an external file. An external file will never be modified.
externalFile
    :: CanCreate m 'ExternalFile
    => FilePath -> ProgramT m (Handle 'ExternalFile)
externalFile = create SExternalFile

-- | Get a reference to a copy of an external file inside the build root that
-- can be modified.
externalFileTempCopy
    :: (CanCreate m 'ExternalFile, CanConvert m 'ExternalFile 'FreeFile)
    => FilePath -> ProgramT m (Handle 'FreeFile)
externalFileTempCopy f = do
    extH <- externalFile f
    convert extH SFreeFile ()

-- | 'use' an external file to introduce an artifact with the help of a
--  artifact dependent extra arguement and return the artifacts handle.
fromFile
    :: (Show (ConvSpec 'FreeFile b), CanCreate m 'ExternalFile, CanConvert m 'FreeFile b, CanConvert m 'ExternalFile 'FreeFile)
    => FilePath -> Sing b -> ConvSpec 'FreeFile b -> ProgramT m (Handle b)
fromFile f a conversionArg = do
    h <- externalFileTempCopy f
    convert h a conversionArg

-- | Given an artifact that support extraction or conversion to a file
-- create/write a file to a given output path.
outputFile
    :: (Show (ConvSpec a 'FreeFile), CanConvert m a 'FreeFile, CanExport m 'FreeFile)
    => Handle a -> ConvSpec a 'FreeFile -> FilePath -> ProgramT m ()
outputFile e src dst = do
    outF <- convert e SFreeFile src
    export outF dst

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

-- * Directories

-- | Create a temp directory
newDirectory :: (CanCreate m 'LocalDirectory)
                => ProgramT m (Handle 'LocalDirectory)
newDirectory = create SLocalDirectory ()

-- | Render the directory to the actual destination (which must not exist)
exportDir :: (CanExport m 'LocalDirectory)
             => Handle 'LocalDirectory -> FilePath -> ProgramT m ()
exportDir = export

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

-- * Template variable definitions

-- * 'Content' to file rendering functions

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
    undefined
    -- TODO addFileFromContent dstH (FromTextFile srcFile) dstSpec

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
            -- TODO addFileFromContent dstH (FromTextFile srcFile) dstSpec
            undefined
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
    tmpFileH <- convert cH SFreeFile (Environment [])
    add dstH SFreeFile (dstSpec, tmpFileH)

-- * Execution environment

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

-- * Some utility vm builder lego


-- * Image import

-- | Load the newest version of the shared image from the local cache.
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
