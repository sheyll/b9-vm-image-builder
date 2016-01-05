{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Install and deploy VM-images using a simple DSL.
module B9.Dsl
       (module X, runCommand, sh, boot, lxc, lxc32, mount, mountDir,
        mountDirRW)
       where

import B9.CommonTypes              as X
import B9.Content                  as X
import B9.DiskImages               as X
import B9.Dsl.Content              as X
import B9.Dsl.Core                 as X
import B9.Dsl.ExecutionEnvironment as X
import B9.Dsl.File                 as X
import B9.Dsl.FileSystem           as X
import B9.ExecEnv                  as X
import B9.FileSystems              as X
import B9.Logging                  as X
import B9.PartitionTable           as X
import B9.Repository               as X
import B9.ShellScript              as X (Script(..))
import Data.Functor                (void)

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
