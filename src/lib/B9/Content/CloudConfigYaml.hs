{-# LANGUAGE OverloadedStrings #-}

{-| This contains a 'YamlObject' for Canonicals @cloud-init@.

For some reason, cloud-config yaml documents __MUST__
contain @#cloud-config@ in the first line.

This is documented in the <https://cloudinit.readthedocs.io/en/latest/topics/format.html#cloud-config-data cloud config documentation>.

Otherwise, this is just a wrapper around 'YamlObject'.

@Since 0.5.62
-}
module B9.Content.CloudConfigYaml
  ( CloudConfigYaml(..)
  , cloudConfigFileHeader
  ) where

import           B9.Content.AST
import           B9.Content.YamlObject

import           Control.Parallel.Strategies (NFData)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.Binary.Get             as Binary
import qualified Data.ByteString.Lazy.Char8  as B
import           Data.Data                   (Data, Typeable)
import           Data.Hashable               (Hashable)
import           GHC.Generics                (Generic)
import           Test.QuickCheck             (Arbitrary)

-- | Cloud-init @meta-data@ configuration Yaml.
--
-- @cloud-config@ yaml documents contain:
 -- @#cloud-config@ as first line.
--
-- @Since 0.5.62
newtype CloudConfigYaml = MkCloudConfigYaml
  { fromCloudConfigYaml :: YamlObject
  } deriving (Hashable, NFData, Eq, Data, Typeable, Generic, Arbitrary, Read, Show, Semigroup)

-- | The header line, which must be the first line in the
-- text file containing the cloud-config Yaml document.
--
-- @Since 0.5.62
cloudConfigFileHeader :: B.ByteString
cloudConfigFileHeader = "#cloud-config\n"

instance ASTish CloudConfigYaml where
  fromAST ast = MkCloudConfigYaml <$> fromAST (fromCloudConfigYaml <$> ast)

instance Binary CloudConfigYaml where
  get
    -- skip the optional header line
   = do
    Binary.lookAheadM
      (do completeDocument <- Binary.lookAhead Binary.getRemainingLazyByteString
          if B.length completeDocument >= B.length cloudConfigFileHeader &&
                B.take (B.length cloudConfigFileHeader) completeDocument == cloudConfigFileHeader
               then do Binary.skip (fromIntegral (B.length cloudConfigFileHeader))
                       return (Just ())
               else return Nothing)
    MkCloudConfigYaml <$> Binary.get
  put (MkCloudConfigYaml y) = do
    Binary.put cloudConfigFileHeader
    Binary.put y
