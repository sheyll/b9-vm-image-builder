{-| Types expressing some form of text content stored in files. The content can
itself be structured, i.e. in Yaml, JSON or Erlang Term format, or unstructured
raw strings or even binaries. -}
module B9.Content
       (module X, FileSpec(..), fileSpec, Content(..), fileSpecPath,
        fileSpecPermissions, fileSpecOwner, fileSpecGroup, UnixUser(..))
       where

import           B9.Content.AST              as X
import           B9.Content.ErlTerms         as X
import           B9.Content.ErlangPropList   as X
import           B9.Content.StringTemplate   as X
import           B9.Content.YamlObject       as X
import           B9.QCUtil
import           Control.Lens.TH
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString.Char8       as B
import           Data.Data
import           Data.Hashable
import           GHC.Generics                (Generic)
import           Test.QuickCheck
import           Text.Printf

-- | This contains most of the information elements necessary to specify a file
-- in some file system. This is used to specify where e.g. cloud-init or the
-- 'LibVirtLXC' builder should create a file.
data FileSpec = FileSpec
    { _fileSpecPath :: FilePath
    , _fileSpecPermissions :: (Word8,Word8, Word8, Word8)
    , _fileSpecOwner :: String
    , _fileSpecGroup :: String
    } deriving (Ord,Read,Eq,Data,Typeable,Generic)

instance Hashable FileSpec
instance Binary FileSpec
instance NFData FileSpec

instance Show FileSpec where
    show (FileSpec path (s,u,g,o) owner group) =
        printf
               "%s%s%s%s %s %s %s"
               (str' s)
               (str u)
               (str g)
               (str o)
               owner
               group
               path
      where
        str' x =
            (if (testBit x 2)
                 then 'u'
                 else '-') :
            (if (testBit x 1)
                 then 'g'
                 else '-') :
            if (testBit x 0)
                then "s"
                else "-"
        str x =
            (if (testBit x 2)
                 then 'r'
                 else '-') :
            (if (testBit x 1)
                 then 'w'
                 else '-') :
            (if (testBit x 0)
                 then "w"
                 else "-")

-- | A file spec for a file belonging to root:root with permissions 0644.
fileSpec :: FilePath -> FileSpec
fileSpec f = FileSpec f (0,6,4,4) "root" "root"

-- | A unix user or group specified by a context dependent user or group name,
-- or specified through a numerical UID or GID respectively. --TODO use or
-- discard? cloud-inits support for that is... well ... it's complicated
data UnixUser
    = UnixUserName String
    | UnixUserId Word16
    deriving (Ord,Read,Eq,Data,Typeable,Generic)

instance Hashable UnixUser
instance Binary UnixUser
instance NFData UnixUser

makeLenses ''FileSpec


{- TODO
data Content
    = ErlangContent (AST Content ErlangPropList)
    | YamlContent (AST Content YamlObject)
    | StringContent String
    | FileContent SourceFile
    deriving (Read,Show,Typeable,Eq,Data,Generic)
-}

-- | This defines the contents of a file to be created by B9.
data Content
    = RenderErlang (AST Content ErlangPropList)
    | RenderYaml (AST Content YamlObject)
    | FromString String
    | FromTextFile SourceFile
    | FromBinaryFile FilePath
    | FromBinary B.ByteString
    | Concat [Content]
    deriving (Read,Show,Typeable,Eq,Data,Generic)

instance Hashable Content
instance Binary Content
instance NFData Content

instance CanRender Content where
  render (RenderErlang ast) = encodeSyntax <$> fromAST ast
  render (RenderYaml ast) = encodeSyntax <$> fromAST ast
  render (FromString str) = return (B.pack str)
  render (FromTextFile s) = readTemplateFile s
  render (FromBinaryFile f) = liftIO (B.readFile f)
  render (FromBinary b) = return b
  render (Concat cs) = fmap mconcat $ mapM render cs

-- * QuickCheck helper

instance Arbitrary FileSpec where
    arbitrary =
        FileSpec <$> smaller arbitraryFilePath <*>
        ((,,,) <$> elements [0 .. 7] <*> elements [0 .. 7] <*>
         elements [0 .. 7] <*>
         elements [0 .. 7]) <*>
        elements ["root", "alice", "bob"] <*>
        elements ["root", "users", "wheel"]

instance Arbitrary Content where
    arbitrary =
        oneof
            [ FromTextFile <$> smaller arbitrary
            , RenderErlang <$> smaller arbitrary
            , RenderYaml <$> smaller arbitrary
            , FromString <$> smaller arbitraryNiceString
            , FromBinaryFile <$> smaller arbitraryFilePath
            , FromBinary <$> B.pack <$> smaller arbitraryNiceString
            , Concat <$> smaller arbitrary
            ]
