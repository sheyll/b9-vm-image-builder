{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module B9.ArtifactGenerator
  (ArtifactGenerator(..)
  ,ArtifactSource(..)
  ,InstanceId(..)
  ,ArtifactTarget(..)
  ,CloudInitType(..)
  ,ArtifactAssembly(..)
  ,AssembledArtifact(..)
  --,YamlValue (..)
  ,instanceIdKey
  ,buildIdKey
  ,buildDateKey
  ) where


import Data.Data
import Data.Monoid -- hiding ((<>))
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Text.Printf

import B9.DiskImages
import B9.Vm

import B9.ConcatableSyntax
import B9.PropLists
import B9.ConfigUtils
import B9.ErlTerms
import B9.Content.StringTemplate
import B9.QCUtil

import qualified Data.ByteString as B

import Test.QuickCheck

-- | A single config generator specifies howto generate multiple output
-- files/directories. It consists of a netsted set of variable bindings that are
-- replaced inside the text files
data ArtifactGenerator =
    Sources [ArtifactSource] [ArtifactGenerator]
  | Let [(String, String)] [ArtifactGenerator]
  | LetX [(String, [String])] [ArtifactGenerator]
  | EachT [String] [[String]] [ArtifactGenerator]
  | Each [(String,[String])] [ArtifactGenerator]
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
data ArtifactSource = Copy FilePath SourceFile
                    | Create FilePath Content
                    | SetPermissions Int Int Int [ArtifactSource]
  --                | SetUserGroupID Int Int [ArtifactSource]
                    | FromDirectory FilePath [ArtifactSource]
                    | IntoDirectory FilePath [ArtifactSource]
                    | Concatenation FilePath [ArtifactSource]
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
                    , (halfSize arbitraryEachT) <*> (halfSize arbitrary)
                    , (halfSize arbitraryEach) <*> (halfSize arbitrary)
                    , Artifact <$> (smaller arbitrary)
                               <*> (smaller arbitrary)
                    , pure EmptyArtifact
                    ]

arbitraryEachT = sized $ \n ->
   EachT <$> vectorOf n (halfSize
                            (listOf1
                               (choose ('a', 'z'))))
         <*> oneof [listOf (vectorOf n (halfSize arbitrary))
                   ,listOf1 (listOf (halfSize arbitrary))]

arbitraryEach = sized $ \n ->
   Each <$> listOf ((,) <$> (listOf1
                               (choose ('a', 'z')))
                        <*> vectorOf n (halfSize
                                          (listOf1
                                             (choose ('a', 'z')))))


instance Arbitrary ArtifactSource where
  arbitrary = oneof [ Embed <$> smaller arbitraryFilePath
                            <*> smaller arbitrary
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
