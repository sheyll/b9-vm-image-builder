{-# LANGUAGE OverloadedStrings #-}

-- | This contains a 'YamlObject' for Canonicals @cloud-init@.
--
-- For some reason, cloud-config yaml documents __MUST__
-- contain @#cloud-config@ in the first line.
--
-- This is documented in the <https://cloudinit.readthedocs.io/en/latest/topics/format.html#cloud-config-data cloud config documentation>.
--
-- Otherwise, this is just a wrapper around 'YamlObject'.
--
-- @Since 0.5.62
module B9.Artifact.Content.CloudConfigYaml
  ( CloudConfigYaml (..),
    cloudConfigFileHeader,
  )
where

import B9.Artifact.Content.AST
import B9.Artifact.Content.YamlObject
import B9.Text
import Control.Parallel.Strategies (NFData)
import Data.Data
  ( Data,
    Typeable,
  )
import Data.Hashable (Hashable)
import Data.Text as Text
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)

-- | Cloud-init @meta-data@ configuration Yaml.
--
-- @cloud-config@ yaml documents contain:
-- @#cloud-config@ as first line.
--
-- @Since 0.5.62
newtype CloudConfigYaml
  = MkCloudConfigYaml
      { fromCloudConfigYaml :: YamlObject
      }
  deriving (Hashable, NFData, Eq, Data, Typeable, Generic, Arbitrary, Read, Show, Semigroup)

-- | The header line, which must be the first line in the
-- text file containing the cloud-config Yaml document.
--
-- @Since 0.5.62
cloudConfigFileHeader :: Text
cloudConfigFileHeader = "#cloud-config\n"

instance FromAST CloudConfigYaml where
  fromAST ast = MkCloudConfigYaml <$> fromAST (fromCloudConfigYaml <$> ast)

instance Textual CloudConfigYaml where
  parseFromText txt = do
    -- skip the optional header line
    let header = Text.take (Text.length cloudConfigFileHeader) txt
        txt' =
          if header == cloudConfigFileHeader
            then Text.drop (Text.length cloudConfigFileHeader) txt
            else txt
    y <- parseFromText txt'
    return (MkCloudConfigYaml y)

  renderToText (MkCloudConfigYaml y) = do
    txt <- renderToText y
    return (Text.unlines [cloudConfigFileHeader, txt])
