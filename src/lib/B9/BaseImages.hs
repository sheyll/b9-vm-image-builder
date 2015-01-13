{-# LANGUAGE DeriveDataTypeable #-}
module B9.BaseImages ( BaseImage (..) ) where

import Data.Data

data BaseImage = BaseImage String
  deriving (Read, Show, Typeable, Data)
