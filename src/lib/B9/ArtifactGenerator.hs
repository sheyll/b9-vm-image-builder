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
  ,YamlValue (..)
  ,instanceIdKey
  ,buildIdKey
  ,buildDateKey
  ) where


import Data.Data
import Data.Monoid -- hiding ((<>))
import Control.Applicative
import Data.Semigroup
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
data ArtifactSource = Embed FilePath ArtifactSourceFile
                  | SetPermissions Int Int Int [ArtifactSource]
  --                | SetUserGroupID Int Int [ArtifactSource]
                  | FromDirectory FilePath [ArtifactSource]
                  | IntoDirectory FilePath [ArtifactSource]
                  | Concatenation FilePath [ArtifactSource]
                  | Put FilePath Content
    deriving (Read, Show, Typeable, Data, Eq)

data Content = RenderErlang (AST ErlangPropList)
             | RenderYaml (AST ErlangPropList)
             | FromTextFile ArtifactSourceFile
  deriving (Read, Show, Typeable, Data, Eq)

newtype Environment = Environment [(String,String)]

printContent :: (MonadIO m, MonadReader Environment m) => Content -> m B.ByteString
printContent (RenderErlang ast) = return $ encodeSyntax (fromAST ast)
printContent (RenderYaml ast) = return $ encodeSyntax (fromAST ast)
printContent (FromTextFile (Source ExpandVariables f)) = do
  c <- liftIO (B.readFile f)
  Environment env <- ask
  let (Right c') = substEB env c
  return c'

objectUnion :: [(String,AST a)] -> [(String, AST a)] -> [(String, AST a)]
objectUnion = (++)

data AST a = ASTObj [(String, AST a)]
           | ASTArr [AST a]
           | ASTMerge [AST a]
           | ASTEmbed Content
           | ASTString String
           | ASTParse ArtifactSourceFile
           | AST a
           | ASTNoOp
  deriving (Read, Show, Typeable, Data, Eq)

data ArtifactSourceFile = Source ArtifactSourceFileConversion FilePath
    deriving (Read, Show, Typeable, Data, Eq)

data ArtifactSourceFileConversion = NoConversion | ExpandVariables
  deriving (Read, Show, Typeable, Data, Eq)

newtype InstanceId = IID String
  deriving (Read, Show, Typeable, Data, Eq)

class StringFilter f where
  filterString :: String -> f String

class HasStrings a where
  applyStringFilter :: (StringFilter f) => a -> f a

class SourceReader f where
  readSource :: ArtifactSourceFile -> f B.ByteString

class (ConcatableSyntax a) => ASTish a where
  fromAST :: (Applicative m, Monad m, MonadIO m, MonadReader Environment m) => AST a -> m a
  encodeAST :: (Applicative m, Monad m, MonadIO m, MonadReader Environment m) => AST a -> m B.ByteString

instance ASTish ErlangPropList where
  fromAST ASTNoOp = pure $ ErlangPropList $ ErlList []

  fromAST (AST a) = pure a

  fromAST (ASTObj pairs) = ErlangPropList . ErlList <$> mapM makePair pairs
    where
      makePair (k, ast) = do
        (ErlangPropList second) <- fromAST ast
        return $ ErlTuple [ErlAtom k, second]

  fromAST (ASTArr xs) =
        ErlangPropList . ErlList
    <$> mapM (\x -> do (ErlangPropList x') <- fromAST x
                       return x')
             xs

  fromAST (ASTString s) = pure $ ErlangPropList $ ErlString s
  fromAST (ASTEmbed c) =
    ErlangPropList . ErlString . T.unpack . E.decodeUtf8 <$> printContent c
  fromAST (ASTMerge asts) = mconcat <$> mapM fromAST asts
  fromAST (ASTParse sourceFile) = do
    c <- artifactSourceRead sourceFile
    case decodeSyntax sourceFile c of
      Right s -> return s
      Left e -> error (printf "could not parse erlang \
                              \source file: '%s'\n%s\n"
                              sourceFile
                              e)

artifactSourceRead :: (MonadIO m, MonadReader Environment m)
                   => ArtifactSourceFile -> m B.ByteString
artifactSourceRead (Source conv f) = do
  c <- liftIO (B.readFile f)
  convert c
  where
    convert c = case conv of
                  NoConversion -> return c
                  ExpandVariables -> do
                    Environment env <- ask
                    let (Right c') = substEB env c
                    return c'

xxx = RenderYaml
        (ASTMerge
           [ASTParse
              (Source NoConversion "common/user-data")
           ,ASTObj
               [("write_files"
                ,ASTArr
                   [ASTObj
                      [("content"
                       ,ASTEmbed
                          (RenderErlang
                             (ASTMerge
                                [ASTParse (Source ExpandVariables "COMMON/xyz1")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz2")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz3")
                                ,ASTParse (Source ExpandVariables "COMMON/xyz4")])))
                      ,("path", ASTString "/usr/lib64/mrfp/runtime.config")
                      ,("owner", ASTString "voice01:voice01")]])]])

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

arbitraryEnv = listOf ((,) <$> listOf1 (choose ('a', 'z')) <*> arbitrary)

halfSize g = sized (flip resize g . (flip div 2))

smaller g = sized (flip resize g . flip (-) 1 )

instance Arbitrary ArtifactSource where
  arbitrary = oneof [ Embed <$> smaller arbitraryFilePath
                            <*> elements [NoConversion, ExpandVariables]
                            <*> smaller (listOf arbitraryFilePath)
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
