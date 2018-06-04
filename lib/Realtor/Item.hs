{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Realtor.Item where

import           Data.Aeson   hiding ((.=))
import           Data.Csv
import           GHC.Generics
import           Protolude    hiding (state, zip)


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


fromBool :: Bool -> Text
fromBool b = if b then "true" else "false"

fromMaybeBool :: Maybe Bool -> Text
fromMaybeBool = maybe "" fromBool
