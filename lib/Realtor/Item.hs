{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Realtor.Item where

import           Data.Aeson   hiding ((.=))
import           Data.Csv
import           Data.Vector  (Vector)
import           GHC.Generics
import           Protolude    hiding (state, zip)

data Item = Item {
    id              :: Text
  , bed             :: Maybe Text
  , bath            :: Maybe Text
  , lotSize         :: Maybe Int
  , price           :: Maybe Int
  , sqft            :: Maybe Int
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

--instance ToField Bool where
--  toField True  = "true"
--  toField False = "false"

instance ToNamedRecord Item where
    toNamedRecord Item{..} =
      namedRecord [
          "id" .= id
        , "beds" .= bed
        , "lotSize" .= lotSize
        , "price" .= price
        , "sqft" .= sqft
        , "address" .= address
        , "city" .= city
        , "state" .= state
        , "zip" .= zip
        , "isForeclosure" .= fromBool isForeclosure
        , "propertyType" .= propertyType
        , "isStatusPending" .= fromMaybeBool isStatusPending
        , "url" .= ldpUrl
        ]

headers :: Vector ByteString
headers = [
    "id"
  , "beds"
  , "lotSize"
  , "price"
  , "sqft"
  , "address"
  , "city"
  , "state"
  , "zip"
  , "isForeclosure"
  , "propertyType"
  , "isStatusPending"
  , "url"
  ]



fromBool :: Bool -> Text
fromBool b = if b then "true" else "false"

fromMaybeBool :: Maybe Bool -> Text
fromMaybeBool = maybe "" fromBool
