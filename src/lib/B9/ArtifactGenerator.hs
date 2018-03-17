{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Top-Level data types for B9 build artifacts.
-}
module B9.ArtifactGenerator
       (ArtifactGenerator(..), ArtifactSource(..), InstanceId(..),
        ArtifactTarget(..), CloudInitType(..), ArtifactAssembly(..),
        AssembledArtifact(..), AssemblyOutput(..), instanceIdKey, buildIdKey, buildDateKey,
        getAssemblyOutput, getArtifactSourceFiles)
       where

import Control.Parallel.Strategies
import Data.Binary
import Data.Data
import Data.Hashable
import GHC.Generics (Generic)
import Data.Semigroup as Sem
import System.FilePath ((<.>), (</>))

import B9.DiskImages
import B9.Vm
import B9.Content.StringTemplate
import B9.Content.Generator
import B9.QCUtil

import Test.QuickCheck

{-| Artifacts represent the things B9 can build. A generator specifies howto
generate parameterized, multiple artifacts. The general structure is:

@
   Let [ ... bindings ... ]
       [ Sources
           [ ... list all input files ... ]
           [ Artifact ...
           , Artifact ...
           , Let [ ... ] [ ... ]
           ]
       ]
@

The reasons why 'Sources' takes a list of 'ArtifactGenerator's is that

   1. this makes the value easier to read/write for humans

   2. the sources are static files used in all children (e.g. company logo image)

   3. the sources are parameterized by variables that bound to different values
      for each artifact, e.g. a template network config file which contains
      the host IP address.

To bind such variables use 'Let', 'Each', 'LetX' or 'EachT'.

String subtitution of these variables is done by "B9.Content.StringTemplate".
These variables can be used as value in nested 'Let's, in most file names/paths
and in source files added with 'B9.Content.StringTemplate.SourceFile'

-}
data ArtifactGenerator
    = Sources [ArtifactSource]
              [ArtifactGenerator]
    |
      -- ^ Add sources available to 'ArtifactAssembly's in
      -- nested artifact generators.
      Let [(String, String)]
          [ArtifactGenerator]
    |
      -- ^ Bind variables, variables are avaible in nested
      -- generators.
      LetX [(String, [String])]
           [ArtifactGenerator]
    |
      -- ^ A 'Let' where each variable is assigned to each
      -- value; the nested generator is executed for each
      -- permutation.
      --
      -- @
      --     LetX [("x", ["1","2","3"]), ("y", ["a","b"])] [..]
      -- @
      -- Is equal to:
      --
      -- @
      --     Let [] [
      --       Let [("x", "1"), ("y", "a")] [..]
      --       Let [("x", "1"), ("y", "b")] [..]
      --       Let [("x", "2"), ("y", "a")] [..]
      --       Let [("x", "2"), ("y", "b")] [..]
      --       Let [("x", "3"), ("y", "a")] [..]
      --       Let [("x", "3"), ("y", "b")] [..]
      --     ]
      -- @
      Each [(String, [String])]
           [ArtifactGenerator]
    |
      -- ^ Bind each variable to their first value, then each
      -- variable to the second value, etc ... and execute the
      -- nested generator in every step. 'LetX' represents a
      -- product of all variables, whereas 'Each' represents a
      -- sum of variable bindings - 'Each' is more like a /zip/
      -- whereas 'LetX' is more like a list comprehension.
      EachT [String]
            [[String]]
            [ArtifactGenerator]
    |
      -- ^ The transposed verison of 'Each': Bind the variables
      -- in the first list to each a set of values from the
      -- second argument; execute the nested generators for
      -- each binding
      Artifact InstanceId
               ArtifactAssembly
    |
      -- ^ Generate an artifact defined by an
      -- 'ArtifactAssembly'; the assembly can access the files
      -- created from the 'Sources' and variables bound by
      -- 'Let'ish elements. An artifact has an instance id,
      -- that is a unique, human readable string describing the
      -- artifact to assemble.
      EmptyArtifact
    deriving (Read,Show,Eq,Data,Typeable,Generic)

instance Hashable ArtifactGenerator
instance Binary ArtifactGenerator
instance NFData ArtifactGenerator

instance Sem.Semigroup ArtifactGenerator where
    (Let [] []) <> x = x
    x <> (Let [] []) = x
    x <> y = Let [] [x, y]

instance Monoid ArtifactGenerator where
    mempty = Let [] []
    mappend = (Sem.<>)

-- | Describe how input files for artifacts to build are obtained.  The general
--   structure of each constructor is __FromXXX__ /destination/ /source/
data ArtifactSource
    = FromFile FilePath
               SourceFile
    |
      -- ^ Copy a 'B9.Content.StringTemplate.SourceFile'
      -- potentially replacing variabled defined in 'Let'-like
      -- parent elements.
      FromContent FilePath
                  Content
    |
      -- ^ Create a file from some 'Content'
      SetPermissions Int
                     Int
                     Int
                     [ArtifactSource]
    |
      -- ^ Set the unix /file permissions/ to all files generated
      -- by the nested list of 'ArtifactSource's.
      FromDirectory FilePath
                    [ArtifactSource]
    |
      -- ^ Assume a local directory as starting point for all
      -- relative source files in the nested 'ArtifactSource's.
      IntoDirectory FilePath
                    [ArtifactSource]
    |
      -- ^ Specify an output directory for all the files
      -- generated by the nested 'ArtifactSource's
      Concatenation FilePath
                    [ArtifactSource]
    -- ^ __Deprecated__ Concatenate the files generated by the
    -- nested 'ArtifactSource's. The nested, generated files
    -- are not written when they are concatenated.
    deriving (Read,Show,Eq,Data,Typeable,Generic)

instance Hashable ArtifactSource
instance Binary ArtifactSource
instance NFData ArtifactSource

-- | Return all source files generated by an 'ArtifactSource'.
getArtifactSourceFiles :: ArtifactSource -> [FilePath]
getArtifactSourceFiles (Concatenation f _) = [f]
getArtifactSourceFiles (FromContent   f _) = [f]
getArtifactSourceFiles (FromFile      f _) = [f]
getArtifactSourceFiles (IntoDirectory pd as) =
    (pd</>) <$> (as >>= getArtifactSourceFiles)
getArtifactSourceFiles (FromDirectory _ as) = as >>= getArtifactSourceFiles
getArtifactSourceFiles (SetPermissions _ _ _ as) =
    as >>= getArtifactSourceFiles



-- | Identify an artifact. __Deprecated__ TODO: B9 does not check if all
-- instances IDs are unique.
newtype InstanceId =
    IID String
    deriving (Read,Show,Typeable,Data,Eq,NFData,Binary,Hashable)

-- | The variable containing the instance id. __Deprecated__
instanceIdKey :: String
instanceIdKey = "instance_id"

-- | The variable containing the buildId that identifies each execution of
-- B9. For more info about variable substitution in source files see
-- 'B9.Content.StringTemplate'
buildIdKey :: String
buildIdKey = "build_id"

-- | The variable containing the date and time a build was started. For more
-- info about variable substitution in source files see
-- 'B9.Content.StringTemplate'
buildDateKey :: String
buildDateKey = "build_date"

-- | Define an __output__ of a build. Assemblies are nested into
-- 'ArtifactGenerator's. They contain all the files defined by the 'Sources'
-- they are nested into.
data ArtifactAssembly
    = CloudInit [CloudInitType]
                FilePath
    |
      -- ^ Generate a __cloud-init__ compatible directory, ISO-
      -- or VFAT image, as specified by the list of
      -- 'CloudInitType's. Every item will use the second
      -- argument to create an appropriate /file name/,
      -- e.g. for the 'CI_ISO' type the output is @second_param.iso@.
      VmImages [ImageTarget]
               VmScript
    -- ^ a set of VM-images that were created by executing a
    -- build script on them.
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable ArtifactAssembly
instance Binary ArtifactAssembly
instance NFData ArtifactAssembly

-- | A type representing the targets assembled by
-- 'B9.ArtifactGeneratorImpl.assemble' from an 'ArtifactAssembly'. There is a
-- list of 'ArtifactTarget's because e.g. a single 'CloudInit' can produce upto
-- three output files, a directory, an ISO image and a VFAT image.
data AssembledArtifact =
    AssembledArtifact InstanceId
                      [ArtifactTarget]
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable AssembledArtifact
instance Binary AssembledArtifact
instance NFData AssembledArtifact

data ArtifactTarget
    = CloudInitTarget CloudInitType
                      FilePath
    | VmImagesTarget
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable ArtifactTarget
instance Binary ArtifactTarget
instance NFData ArtifactTarget

data CloudInitType
    = CI_ISO
    | CI_VFAT
    | CI_DIR
    deriving (Read,Show,Typeable,Data,Eq,Generic)

instance Hashable CloudInitType
instance Binary CloudInitType
instance NFData CloudInitType

-- | The output of an 'ArtifactAssembly' is either a set of generated files,
--  or it might be a directory that contains the artifacts sources.
data AssemblyOutput =
    AssemblyGeneratesOutputFiles [FilePath]
   | AssemblyCopiesSourcesToDirectory FilePath
   deriving (Read,Show,Typeable,Data,Eq,Generic)

-- | Return the files that the artifact assembly consist of.
getAssemblyOutput :: ArtifactAssembly -> [AssemblyOutput]
getAssemblyOutput (VmImages ts _) =
    AssemblyGeneratesOutputFiles . getImageDestinationOutputFiles <$> ts
getAssemblyOutput (CloudInit ts o) = getCloudInitOutputFiles o <$> ts
 where
  getCloudInitOutputFiles baseName t = case t of
      CI_ISO  -> AssemblyGeneratesOutputFiles [baseName <.> "iso"]
      CI_VFAT -> AssemblyGeneratesOutputFiles [baseName <.> "vfat"]
      CI_DIR  -> AssemblyCopiesSourcesToDirectory baseName


-- * QuickCheck instances

instance Arbitrary ArtifactGenerator where
    arbitrary =
        oneof
            [ Sources <$> halfSize arbitrary <*> halfSize arbitrary
            , Let <$> halfSize arbitraryEnv <*> halfSize arbitrary
            , halfSize arbitraryEachT <*> halfSize arbitrary
            , halfSize arbitraryEach <*> halfSize arbitrary
            , Artifact <$> smaller arbitrary <*> smaller arbitrary
            , pure EmptyArtifact]


arbitraryEachT :: Gen ([ArtifactGenerator] -> ArtifactGenerator)
arbitraryEachT =
    sized $
    \n ->
         EachT <$> vectorOf n (halfSize (listOf1 (choose ('a', 'z')))) <*>
         oneof
             [ listOf (vectorOf n (halfSize arbitrary))
             , listOf1 (listOf (halfSize arbitrary))]

arbitraryEach :: Gen ([ArtifactGenerator] -> ArtifactGenerator)
arbitraryEach =
    sized $
    \n ->
         Each <$>
         listOf
             ((,) <$> listOf1 (choose ('a', 'z')) <*>
              vectorOf n (halfSize (listOf1 (choose ('a', 'z')))))


instance Arbitrary ArtifactSource where
    arbitrary =
        oneof
            [ FromFile <$> smaller arbitraryFilePath <*> smaller arbitrary
            , FromContent <$> smaller arbitraryFilePath <*> smaller arbitrary
            , SetPermissions <$> choose (0, 7) <*> choose (0, 7) <*>
              choose (0, 7) <*>
              smaller arbitrary
            , FromDirectory <$> smaller arbitraryFilePath <*> smaller arbitrary
            , IntoDirectory <$> smaller arbitraryFilePath <*> smaller arbitrary]

instance Arbitrary InstanceId where
    arbitrary = IID <$> arbitraryFilePath

instance Arbitrary ArtifactAssembly where
    arbitrary =
        oneof
            [ CloudInit <$> arbitrary <*> arbitraryFilePath
            , VmImages <$> smaller arbitrary <*> pure NoVmScript]

instance Arbitrary CloudInitType where
    arbitrary = elements [CI_ISO, CI_VFAT, CI_DIR]
