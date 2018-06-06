{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Realtor.Item where

import           Control.Monad.Fail   (fail)
import           Data.Aeson           hiding ((.=))
import qualified Data.Attoparsec.Text as P
import           Data.Csv
import           Data.Scientific      (Scientific)
import           Data.Vector          (Vector)
import           GHC.Generics
import           Protolude            hiding (state, zip)

exactOrMoreParser :: P.Parser ExactOrMore
exactOrMoreParser = P.try (Exact <$> P.scientific) <|> P.try (EqualOrMore <$> P.scientific <* P.char '+')



data ExactOrMore = Exact Scientific | EqualOrMore Scientific
  deriving (Eq, Show)

instance FromJSON ExactOrMore where
  parseJSON v = case v of
    Number n -> pure $ Exact n
    _ -> case v of
          String s -> either fail pure $ P.parseOnly exactOrMoreParser s

          _        -> fail "Unsupported type"


instance ToJSON ExactOrMore where
  toJSON (Exact n)       = String $ show n
  toJSON (EqualOrMore n) = String $ show n <> "+"

instance ToField ExactOrMore where
  toField (Exact n)       = show n
  toField (EqualOrMore n) = show n <> "+"



data Item = Item {
    id              :: Text
  , bed             :: Maybe ExactOrMore
  , bath            :: Maybe ExactOrMore
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
  deriving (Eq, Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance ToNamedRecord Item where
    toNamedRecord Item{..} =
      namedRecord [
          "id"      .= id
        , "beds"    .= bed
        , "baths"   .= bath
        , "lotSize" .= lotSize
        , "price"   .= price
        , "sqft"    .= sqft
        , "address" .= address
        , "city"    .= city
        , "state"   .= state
        , "zip"     .= zip
        , "isForeclosure"   .= fromBool isForeclosure
        , "propertyType"    .= propertyType
        , "isStatusPending" .= fromMaybeBool isStatusPending
        , "url"     .= ldpUrl
        ]

headers :: Vector ByteString
headers = [
    "id"
  , "beds"
  , "baths"
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
