module B9.ArtifactGenerator
  (ArtifactGenerator(..)
  ,ArtifactSource(..)
  ,InstanceId(..)
  ,ArtifactTarget(..)
  ,CloudInitType(..)
  ,ArtifactAssembly(..)
  ,AssembledArtifact(..)
  ,instanceIdKey
  ,buildIdKey
  ,buildDateKey
  ) where


import Data.Data
import Data.Monoid
import Control.Applicative

import B9.DiskImages
import B9.Vm

import Test.QuickCheck

-- | A single config generator specifies howto generate multiple output
-- files/directories. It consists of a netsted set of variable bindings that are
-- replaced inside the text files
data ArtifactGenerator =
    Sources [ArtifactSource] [ArtifactGenerator]
  | Let [(String, String)] [ArtifactGenerator]
  | Each [String] [[String]] [ArtifactGenerator]
  | Artifact InstanceId ArtifactAssembly
  | EmptyArtifact
  deriving (Read, Show, Typeable, Data, Eq)

instance Monoid ArtifactGenerator where
  mempty = Let [] []
  (Let [] []) `mappend` x = x
  x `mappend` (Let [] []) = x
  x `mappend` y = Let [] [x, y]

-- | Explicit is better than implicit: Only files that have explicitly been
-- listed will be included in any generated configuration. That's right: There's
-- no "inlcude *.*". B9 will check that *all* files in the directory specified with 'FromDir' are referred to by nested 'ArtifactSource's.
data ArtifactSource = Template FilePath
                  | Templates [FilePath]
                  | File FilePath
                  | Files [FilePath]
                  | SetPermissions Int Int Int [ArtifactSource]
  --                | SetUserGroupID Int Int [ArtifactSource]
                  | Concatenation FilePath [ArtifactSource]
                  | FromDirectory FilePath [ArtifactSource]
                  | IntoDirectory FilePath [ArtifactSource]
    deriving (Read, Show, Typeable, Data, Eq)

newtype InstanceId = IID String
  deriving (Read, Show, Typeable, Data, Eq)

instanceIdKey :: String
instanceIdKey = "instance_id"

buildIdKey :: String
buildIdKey = "build_id"

buildDateKey :: String
buildDateKey = "build_date"

data ArtifactAssembly = CloudInit [CloudInitType] FilePath
                      | VmImages [ImageTarget] VmScript
  deriving (Read, Show, Typeable, Data, Eq)

data AssembledArtifact = AssembledArtifact InstanceId [ArtifactTarget]
  deriving (Read, Show, Typeable, Data, Eq)

data ArtifactTarget = CloudInitTarget CloudInitType FilePath
                    | VmImagesTarget
  deriving (Read, Show, Typeable, Data, Eq)

data CloudInitType = CI_ISO | CI_VFAT | CI_DIR
  deriving (Read, Show, Typeable, Data, Eq)

instance Arbitrary ArtifactGenerator where
  arbitrary = oneof [ Sources <$> (halfSize arbitrary) <*> (halfSize arbitrary)
                    , Let <$> (halfSize arbitraryEnv) <*> (halfSize arbitrary)
                    , (halfSize arbitraryEach) <*> (halfSize arbitrary)
                    , Artifact <$> (smaller arbitrary)
                               <*> (smaller arbitrary)
                    , pure EmptyArtifact
                    ]

arbitraryEach = sized $ \n ->
   Each <$> vectorOf n (halfSize
                           (listOf1
                              (choose ('a', 'z'))))
        <*> oneof [ listOf (vectorOf n (halfSize arbitrary))
                  , listOf1 (listOf (halfSize arbitrary))
                  ]

arbitraryEnv = listOf ((,) <$> listOf1 (choose ('a', 'z')) <*> arbitrary)

halfSize g = sized (flip resize g . (flip div 2))

smaller g = sized (flip resize g . flip (-) 1 )

instance Arbitrary ArtifactSource where
  arbitrary = oneof [ Template <$> smaller arbitraryFilePath
                    , Templates <$> smaller (listOf arbitraryFilePath)
                    , File <$> smaller arbitraryFilePath
                    , Files <$> smaller (listOf arbitraryFilePath)
                    , SetPermissions <$> choose (0,7)
                                     <*> choose (0,7)
                                     <*> choose (0,7)
                                     <*> smaller arbitrary
                    , FromDirectory <$> smaller arbitraryFilePath <*> smaller arbitrary
                    , IntoDirectory <$> smaller arbitraryFilePath <*> smaller arbitrary
                    ]


instance Arbitrary InstanceId where
  arbitrary = IID <$> arbitraryFilePath

instance Arbitrary ArtifactAssembly where
  arbitrary = oneof [ CloudInit <$> arbitrary <*> arbitraryFilePath
                    , pure (VmImages [] NoVmScript)
                    ]

instance Arbitrary CloudInitType where
  arbitrary = elements [CI_ISO,CI_VFAT,CI_DIR]

arbitraryFilePath =
  listOf1 (frequency
             [(50, oneof [choose ('a','z')
                         ,choose ('A','Z')
                         ,choose ('0', '9')
                         ,pure '-'
                         ,pure '_'])
             ,(5, pure '/')
             ,(1, pure ' ')
             ,(1, pure '.')])
