-- Instances of 'B9.ConcatableSyntax' for some commonly used
-- syntax types.
module B9.PropLists (YamlUserData (..)
                    ,OTPSysConfig (..)) where

import Data.Yaml
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict
import Control.Applicative
import Text.Show.Pretty
import Data.Vector ((++))
import Prelude hiding ((++))
import Data.Semigroup

import B9.ErlTerms
import B9.ConcatableSyntax

-- | A wrapper type around erlang terms with a Semigroup instance useful for
-- combining sys.config files with OTP-application configurations in a list of
-- the form of a proplist.
newtype OTPSysConfig = OTPSysConfig SimpleErlangTerm
  deriving (Eq,Show)

instance Semigroup OTPSysConfig where
  (OTPSysConfig v1) <> (OTPSysConfig v2) = OTPSysConfig (combine v1 v2)
    where
      combine (ErlList pl1) (ErlList pl2) = ErlList (pl1 <> pl2)
      combine (ErlList pl1) t2 = ErlList (pl1 <> [t2])
      combine t1 (ErlList pl2) = ErlList ([t1] <> pl2)
      combine t1 t2 = ErlList [t1,t2]

instance ConcatableSyntax OTPSysConfig where
  decodeSyntax str = do
    t <- parseErlTerm "" str
    return (OTPSysConfig t)

  encodeSyntax (OTPSysConfig t) =
    renderErlTerm t

-- | A wrapper type around yaml values with a Semigroup instance useful for
-- combining yaml documents describing system configuration like e.g. user-data.
newtype YamlUserData = YamlUserData Data.Yaml.Value
  deriving (Eq,Show)

instance Semigroup YamlUserData where
  (YamlUserData v1) <> (YamlUserData v2) = YamlUserData (combine v1 v2)
    where
      combine :: Data.Yaml.Value
              -> Data.Yaml.Value
              -> Data.Yaml.Value
      combine (Object o1) (Object o2) =
        Object (unionWith combine o1 o2)
      combine (Array a1) (Array a2) =
        Array (a1 ++ a2)
      combine t1 t2 =
        array [t1,t2]

instance ConcatableSyntax YamlUserData where
  decodeSyntax str = do
    o <- decodeEither str
    return (YamlUserData o)

  encodeSyntax (YamlUserData o) = encode o
