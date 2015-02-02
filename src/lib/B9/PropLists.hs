module B9.PropLists () where

import Data.Monoid
import Data.Yaml
import qualified Data.Yaml as YAML
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict
import Control.Applicative
import Text.Show.Pretty
import Data.Vector ((++))
import Prelude hiding ((++))

newtype PList = PList Object
  deriving (Show)

instance Monoid PList where
  mempty = PList mempty
  mappend (PList o1) (PList o2) = PList (unionWith combine o1 o2)
    where
      combine (Array a1) (Array a2) = (Array (a1 ++ a2))
      combine _ v2 = v2

instance FromJSON PList where parseJSON (Object o) = pure (PList o)

instance ToJSON PList where toJSON (PList o) = Object o





loadMrfp :: IO (Maybe PList)
loadMrfp =
  decodeFile "/home/sven/Dropbox/Work/dtag/cloud-init-mrfp/common/user-data"

loadMrfc :: IO (Maybe PList)
loadMrfc =
  decodeFile "/home/sven/Dropbox/Work/dtag/cloud-init-mrfc/user-data"


test = do
  (Just mrfp) <- loadMrfp
  (Just mrfc) <- loadMrfc
  let ee = (YAML.encode (mrfp <> mrfc))
  B.putStrLn ee
  let (Just ff) = YAML.decode ee :: Maybe PList
  let gg = YAML.encode ff
  BL.putStrLn (JSON.encode ff)
