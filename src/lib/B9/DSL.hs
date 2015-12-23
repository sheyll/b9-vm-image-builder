{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Install and deploy VM-images using a simple DSL.
module B9.DSL
       (module X, BuiltInArtifact(..), SBuiltInArtifact, Sing(..),
        imageRepositoryH, fromShared, sharedAs, boot, lxc, lxc32,
        mountDir, mountDirRW, mount, runCommand, sh, rootImage, dataImage,
        mountAndShareSharedImage, mountAndShareNewImage)
       where

import B9.CommonTypes     as X
import B9.Content         as X
import B9.Dsl.Core        as X
import B9.Dsl.Content     as X
import B9.Dsl.Files       as X
import B9.Dsl.ShellScript as X
import B9.DiskImages      as X
import B9.ExecEnv         as X
import B9.FileSystems     as X
import B9.Logging         as X
import B9.PartitionTable  as X
import B9.Repository      as X
import B9.ShellScript     as X (Script(..))
import Data.Functor       (void)
import Data.Singletons.TH

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
     | FileSystemBuilder
     | FileSystemImage
     | ImageRepository
     deriving (Show)
   |])

-- * Creation type families

-- | Artifact specific parameters for 'create'
type instance CreateSpec 'ExecutionEnvironment = ExecEnvSpec

-- | Parameters for 'add'. They may depend on both artifact types.
type instance AddSpec 'ExecutionEnvironment 'ExecutableScript = Script
type instance AddSpec 'ExecutionEnvironment 'FreeFile         = (FileSpec, Handle 'FreeFile)
type instance AddSpec 'ExecutionEnvironment 'LocalDirectory   = SharedDirectory
type instance AddSpec 'ImageRepository 'VmImage               = (SharedImageName, Handle 'VmImage)
type instance AddSpec 'UpdateServerRoot 'VmImage              = (SharedImageName, Handle 'VmImage)
type instance AddSpec 'VariableBindings 'TemplateVariable     = (String, String)

-- | Parameters for 'convert'. They may depend on both artifact types.
type instance ConvSpec 'ExecutionEnvironment 'VmImage       = (Handle 'VmImage, MountPoint)
type instance ConvSpec 'ExecutionEnvironment 'FreeFile      = FilePath
type instance ConvSpec 'FileSystemBuilder 'VmImage          = ()
type instance ConvSpec 'FileSystemImage 'VmImage            = ()
type instance ConvSpec 'FreeFile 'PartitionedVmImage        = ()
type instance ConvSpec 'FreeFile 'VmImage                   = ImageType
type instance ConvSpec 'ImageRepository 'VmImage            = SharedImageName
type instance ConvSpec 'LocalDirectory 'UpdateServerRoot    = ()
type instance ConvSpec 'PartitionedVmImage 'FreeFile        = PartitionSpec
type instance ConvSpec 'VmImage 'FileSystemImage            = ()
type instance ConvSpec 'VmImage 'FreeFile                   = ()
type instance ConvSpec 'VmImage 'VmImage                    = Either ImageType ImageSize

-- | Artifact export parameters.
type instance ExportSpec 'VmImage         = FilePath

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
