{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Install and deploy VM-images using a simple DSL.
module B9.Dsl
       (module X,
        externalFile, externalFileTempCopy, fromFile, outputFile, addFile,
        addExe, addFileP, newDirectory, exportDir, createContent,
        appendContent, addFileFull, addFileFromContent, runCommand, sh,
        boot, lxc, lxc32,
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
import B9.Dsl.PartitionedVmImage   as X
import B9.Dsl.UpdateServerRoot     as X
import B9.Dsl.VmImage              as X
import B9.ExecEnv                  as X
import B9.FileSystems              as X
import B9.PartitionTable           as X
import B9.Repository               as X
import B9.ShellScript              as X (Script(..))

-- * Cloud-init
-- TODO

-- * Adding Files to an Artifact

-- | Reference an external file. An external file will never be modified.
externalFile
    :: (IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath)
    => FilePath -> ProgramT m (Handle ExternalFile)
externalFile = create SExternalFile

-- | Get a reference to a copy of an external file inside the build root that
-- can be modified.
externalFileTempCopy
    :: (IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ())
    => FilePath -> ProgramT m (Handle FreeFile)
externalFileTempCopy f = do
    extH <- externalFile f
    extract extH SFreeFile ()

-- | 'use' an external file to introduce an artifact with the help of a
--  artifact dependent extra arguement and return the artifacts handle.
fromFile
    :: (IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m FreeFile b
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ())
    => FilePath -> proxy b -> ExtractionArg m FreeFile b -> ProgramT m (Handle b)
fromFile f a conversionArg = do
    h <- externalFileTempCopy f
    extract h a conversionArg

-- | Given an artifact that support extraction or conversion to a file
-- create/write a file to a given output path.
outputFile
    :: (CanExtract m a FreeFile
       ,CanExport m FreeFile
       ,ExportSpec m FreeFile ~ FilePath)
    => Handle a -> ExtractionArg m a FreeFile -> FilePath -> ProgramT m ()
outputFile e src dst = do
    outF <- extract e SFreeFile src
    export outF dst

-- * File Inclusion, File-Templating and Script Rendering

-- | Add an existing file to an artifact.
-- Strip the directories from the path, e.g. @/etc/blub.conf@ will be
-- @blob.conf@ in the artifact. The file will be world readable and not
-- executable. The source file must not be a directory.
addFile
    :: (AddSpec m e FreeFile ~ (FileSpec, Handle FreeFile)
       ,IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ()
       ,CanAdd m e FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addFile d f = addFileP d f (0, 6, 4, 4)

-- | Same as 'addFile' but set the destination file permissions to @0755@
-- (executable for all).
addExe
    :: (AddSpec m e FreeFile ~ (FileSpec, Handle FreeFile)
       ,IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ()
       ,CanAdd m e FreeFile)
       => Handle e -> FilePath -> ProgramT m ()
addExe d f = addFileP d f (0, 7, 5, 5)

-- | Same as 'addFile' but with an extra output file permission parameter.
addFileP
    :: (AddSpec m e FreeFile ~ (FileSpec, Handle FreeFile)
       ,IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ()
       ,CanAdd m e FreeFile)
       => Handle e -> FilePath -> (Word8, Word8, Word8, Word8) -> ProgramT m ()
addFileP dstH f p = do
    let dstSpec = fileSpec (takeFileName f) & fileSpecPermissions .~ p
    origH <- create SExternalFile f
    tmpH <- extract origH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpH)

-- * Directories

-- | Create a temp directory
newDirectory :: (IsBuilder m LocalDirectory
                ,InitArgs m LocalDirectory ~ ())
                => ProgramT m (Handle LocalDirectory)
newDirectory = create SLocalDirectory ()

-- | Render the directory to the actual destination (which must not exist)
exportDir :: (CanExport m LocalDirectory
             ,ExportSpec m LocalDirectory ~ FilePath)
             => Handle LocalDirectory -> FilePath -> ProgramT m ()
exportDir = export

-- * /Low-level/ 'Content' generation functions

-- | Create a handle for accumulating 'Content' with an initial 'Content'.
createContent :: (IsCnt c
                 ,Show c
                 ,IsBuilder m (Cnt c)
                 ,InitArgs m (Cnt c) ~ (c,String))
                 => c -> String -> ProgramT m (Handle (Cnt c))
createContent c title = create (cntProxy c) (c, title)

-- | Accumulate/Append more 'Content' to the 'GeneratedContent'
--   handle obtained by e.g. 'createContent'
appendContent :: (Show c
                 ,IsCnt c
                 ,CanAdd m (Cnt c) (Cnt c)
                 ,AddSpec m (Cnt c) (Cnt c) ~ c)
                 => Handle (Cnt c) -> c -> ProgramT m ()
appendContent hnd c = add hnd (cntProxy c) c

-- * Template variable definitions

-- * 'Content' to file rendering functions

-- | Add an existing file from the file system to an artifact at a 'FileSpec'.
addFileFull
    :: (AddSpec m e FreeFile ~ (FileSpec, Handle FreeFile)
       ,IsBuilder m ExternalFile
       ,InitArgs m ExternalFile ~ FilePath
       ,CanExtract m ExternalFile FreeFile
       ,ExtractionArg m ExternalFile FreeFile ~ ()
       ,CanAdd m e FreeFile)
       => Handle e -> FilePath -> FileSpec -> ProgramT m (Handle FreeFile)
addFileFull dstH srcFile dstSpec = do
    origH <- create SExternalFile srcFile
    tmpH <- extract origH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpH)
    return tmpH

-- | Generate a file with a content and add that file to an artifact at a
-- 'FileSpec'.
addFileFromContent
    :: (Show c
       ,IsCnt c
       ,IsBuilder m (Cnt c)
       ,InitArgs m (Cnt c) ~ (c, String)
       ,CanExtract m (Cnt c) FreeFile
       ,ExtractionArg m (Cnt c) FreeFile ~ ()
       ,AddSpec m e FreeFile ~ (FileSpec, Handle FreeFile)
       ,CanAdd m e FreeFile)
       => Handle e -> c -> FileSpec -> ProgramT m (Handle (Cnt c))
addFileFromContent dstH content dstSpec = do
    cH <-
        createContent
            content
            (printf
                 "contents-of-%s"
                 (dstSpec ^. fileSpecPath . to takeFileName))
    tmpFileH <- extract cH SFreeFile ()
    add dstH SFreeFile (dstSpec, tmpFileH)
    return cH

-- * Execution environment

-- | Run a command in an environment.
runCommand
    :: (CanAdd m a 'ExecutableScript)
    => Handle a -> AddSpec m a 'ExecutableScript -> ProgramT m ()
runCommand hnd = add hnd SExecutableScript

-- | Execute a string in a shell inside an environment.
sh
    :: (AddSpec m a 'ExecutableScript ~ Script
       ,CanAdd m a 'ExecutableScript)
    => Handle a -> String -> ProgramT m ()
sh e s = runCommand e (Run s [])

boot :: (IsBuilder m 'ExecutionEnvironment
        ,InitArgs m 'ExecutionEnvironment ~ ExecEnvSpec)
        => ExecEnvSpec -> ProgramT m (Handle 'ExecutionEnvironment)
boot = create SExecutionEnvironment

lxc :: (IsBuilder m 'ExecutionEnvironment
       ,InitArgs m 'ExecutionEnvironment ~ ExecEnvSpec)
       => String -> ProgramT m (Handle 'ExecutionEnvironment)
lxc name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 X86_64)

lxc32 :: (IsBuilder m 'ExecutionEnvironment
         ,InitArgs m 'ExecutionEnvironment ~ ExecEnvSpec)
         => String -> ProgramT m (Handle 'ExecutionEnvironment)
lxc32 name = boot $ ExecEnvSpec name LibVirtLXC (Resources AutomaticRamSize 2 I386)

-- * Mounting

mountDir :: (CanAdd m 'ExecutionEnvironment LocalDirectory
            ,AddSpec m 'ExecutionEnvironment LocalDirectory ~ SharedDirectory)
            => Handle 'ExecutionEnvironment
            -> FilePath
            -> FilePath
            -> ProgramT m ()
mountDir e hostDir dest =
    add e SLocalDirectory (SharedDirectoryRO hostDir (MountPoint dest))

mountDirRW :: (CanAdd m 'ExecutionEnvironment LocalDirectory
              ,AddSpec m 'ExecutionEnvironment LocalDirectory ~ SharedDirectory)
              => Handle 'ExecutionEnvironment
              -> FilePath
              -> FilePath
              -> ProgramT m ()
mountDirRW e hostDir dest =
    add e SLocalDirectory (SharedDirectory hostDir (MountPoint dest))

mount
    :: (CanExtract m 'ExecutionEnvironment 'VmImage
       ,ExtractionArg m 'ExecutionEnvironment 'VmImage ~ (Handle 'VmImage, MountPoint))
       => Handle 'ExecutionEnvironment
       -> Handle 'VmImage
       -> FilePath
       -> ProgramT m (Handle 'VmImage)
mount e imgHnd dest = extract e SVmImage (imgHnd, MountPoint dest)

-- * Some utility vm builder lego


-- * Image import

-- | Load the newest version of the shared image from the local cache.
fromShared :: (CanExtract m 'ImageRepository 'VmImage
              ,ExtractionArg m 'ImageRepository 'VmImage ~ SharedImageName)
              => String -> ProgramT m (Handle 'VmImage)
fromShared sharedImgName = extract
        imageRepositoryH
        SVmImage
        (SharedImageName sharedImgName)

-- * Image export

-- | Store an image in the local cache with a name as key for lookups, e.g. from
-- 'fromShared'
sharedAs :: (CanAdd m 'ImageRepository 'VmImage
            ,AddSpec m 'ImageRepository 'VmImage ~ (SharedImageName, Handle 'VmImage))
            => Handle 'VmImage -> String -> ProgramT m ()
sharedAs hnd name = add imageRepositoryH SVmImage (SharedImageName name, hnd)

rootImage :: (CanExtract m 'ImageRepository 'VmImage
             ,ExtractionArg m 'ImageRepository 'VmImage ~ SharedImageName
             ,CanExtract m 'ExecutionEnvironment 'VmImage
             ,ExtractionArg m 'ExecutionEnvironment 'VmImage ~ (Handle 'VmImage, MountPoint)
             ,CanAdd m 'ImageRepository 'VmImage
             ,AddSpec m 'ImageRepository 'VmImage ~ (SharedImageName, Handle 'VmImage))
             => String
             -> String
             -> Handle 'ExecutionEnvironment
             -> ProgramT m ()
rootImage nameFrom nameExport env =
    void $ mountAndShareSharedImage nameFrom nameExport "/" env

dataImage :: (IsBuilder m 'FileSystemBuilder
             ,InitArgs m 'FileSystemBuilder ~ FileSystemSpec
             ,CanExtract m 'FileSystemBuilder 'FileSystemImage
             ,ExtractionArg m 'FileSystemBuilder 'FileSystemImage ~ ()
             ,CanExtract m 'FileSystemImage 'VmImage
             ,CanAdd m 'ImageRepository 'VmImage
             ,AddSpec m 'ImageRepository 'VmImage ~ (SharedImageName, Handle 'VmImage)
             ,CanExtract m 'ExecutionEnvironment 'VmImage
             ,ExtractionArg m 'ExecutionEnvironment 'VmImage ~ (Handle 'VmImage, MountPoint)
             ,ExtractionArg m 'FileSystemImage 'VmImage ~ ())
             => String -> Handle 'ExecutionEnvironment -> ProgramT m ()
dataImage nameExport env =
    void $ mountAndShareNewImage "data" 64 nameExport "/data" env

mountAndShareSharedImage :: (CanExtract m 'ImageRepository 'VmImage
                            ,ExtractionArg m 'ImageRepository 'VmImage ~ SharedImageName
                            ,CanExtract m 'ExecutionEnvironment 'VmImage
                            ,ExtractionArg m 'ExecutionEnvironment 'VmImage ~ (Handle 'VmImage, MountPoint)
                            ,CanAdd m 'ImageRepository 'VmImage
                            ,AddSpec m 'ImageRepository 'VmImage ~ (SharedImageName, Handle 'VmImage))
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
    :: (IsBuilder m 'FileSystemBuilder
       ,InitArgs m 'FileSystemBuilder ~ FileSystemSpec
       ,CanExtract m 'FileSystemBuilder 'FileSystemImage
       ,ExtractionArg m 'FileSystemBuilder 'FileSystemImage ~ ()
       ,CanExtract m 'FileSystemImage 'VmImage
       ,ExtractionArg m 'FileSystemImage 'VmImage ~ ()
       ,CanAdd m 'ImageRepository 'VmImage
       ,AddSpec m 'ImageRepository 'VmImage ~ (SharedImageName, Handle 'VmImage)
       ,CanExtract m 'ExecutionEnvironment 'VmImage
       ,ExtractionArg m 'ExecutionEnvironment 'VmImage ~ (Handle 'VmImage, MountPoint))
    => String
    -> Int
    -> String
    -> FilePath
    -> Handle 'ExecutionEnvironment
    -> ProgramT m ()
mountAndShareNewImage label sizeGB nameTo mountPoint env = do
    fs <-
        create SFileSystemBuilder (FileSystemSpec Ext4 label sizeGB GB)
    fi <- extract fs SFileSystemImage ()
    i <- extract fi SVmImage ()
    i' <- mount env i mountPoint
    i' `sharedAs` nameTo
