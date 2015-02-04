module B9.ConcatableSyntax (ConcatableSyntax(..)
                           ,SyntacticCat
                           ,decodeSyntaxFile
                           ,encodeSyntaxFile
                           ,concatSyntaxFiles) where

import qualified Data.ByteString as B
import Data.Data
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import System.Directory
import Text.Printf

-- | Image you would want to create a cloud-init 'user-data' file from a set of
-- 'user-data' snippets which each are valid 'user-data' file, i.e. they contain
-- yaml syntax and e.g. a 'writefiles' section. Now the goal is, for b9 to be
-- able to merge these snippets into one, such that all writefiles sections are
-- combined into a single writefile section.  This type class is the greatest
-- commonon denominator of types describing a syntax that can be parsed,
-- concatenated e.g. like in the above example and rendered. The actual
-- concatenation operation is the append from Monoid, i.e. like monoid but
-- without the need for an empty element.
class Semigroup a => ConcatableSyntax a where
  decodeSyntax :: B.ByteString -> Either String a
  encodeSyntax :: a -> B.ByteString

-- | Read syntax from a file and throw an exception on error.
decodeSyntaxFile :: (Functor m, ConcatableSyntax a, MonadIO m)
                 => FilePath
                 -> m (Either String a)
decodeSyntaxFile fp = do
  exists <- liftIO (doesFileExist fp)
  if exists
   then decodeSyntax <$> liftIO (B.readFile fp)
   else return (Left (printf "decoding syntax file failed:\
                             \ \'\' does not exist"))

-- | Render some syntax to a file.
encodeSyntaxFile :: (ConcatableSyntax a, MonadIO m)
                 => FilePath
                 -> a
                 -> m ()
encodeSyntaxFile fp s = do
  exists <- liftIO (doesFileExist fp)
  if exists
    then fail (printf "cannot encode syntax to file: '%s'\
         \the file already exists in the file system.")
    else liftIO (B.writeFile fp (encodeSyntax s))

-- | Read and decode the syntax in 'inputFiles', merge the syntax and write it
-- to 'outputFile' and return the merged value.
concatSyntaxFiles :: (ConcatableSyntax a, Functor m, MonadIO m)
                  => [FilePath]
                  -> FilePath
                  -> m (Either String a)
concatSyntaxFiles inputFiles outputFile = do
  inputs <- sequence <$> mapM decodeSyntaxFile inputFiles
  let concatenated = liftM (foldl1 (<>)) inputs
  either (const (return ()))
         (encodeSyntaxFile outputFile)
         concatenated
  return concatenated

data SyntacticCat a = SyntacticCat [a]
                    | RenderSyntax FilePath (SyntacticCat a)
                    deriving (Show,Read,Data,Typeable)
