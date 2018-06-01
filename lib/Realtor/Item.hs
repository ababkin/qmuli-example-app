{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Realtor.Item where

import           Data.Aeson
import           GHC.Generics
import           Protolude

data Item = Item {
    id              :: Text
  , bed             :: Maybe Int
  , bath            :: Maybe Text
  , lotSize         :: Int
  , price           :: Int
  , sqft            :: Int
  , address         :: Text
  , city            :: Text
  , state           :: Text
  , zip             :: Text
  , isForeclosure   :: Bool
  , propertyType    :: Text
  , isStatusPending :: Maybe Bool
  , ldpUrl          :: Text
  }
  deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item
