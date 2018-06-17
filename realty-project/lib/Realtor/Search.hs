{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Realtor.Search where

import           Control.Monad.Fail  (fail)
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Proxy
import           Data.Scientific
import           GHC.Generics
import           Protolude           hiding (state)
import           Realtor.Item        (Item)


data Facets = Facets {
    beds_min         :: Maybe Text
  , beds_max         :: Maybe Text
  , baths_min        :: Maybe Text
  , baths_max        :: Maybe Text
  , price_min        :: Maybe Text
  , price_max        :: Maybe Text
  , acre_min         :: Maybe Text
  , acre_max         :: Maybe Text
  , foreclosure      :: Maybe Bool
  , days_on_market   :: Maybe Int
  , new_construction :: Maybe Bool
  }
  deriving (Show, Generic)

instance ToJSON Facets

instance Default Facets where
  def = Facets def def def def def def def def def def def

data SearchParams = SearchParams {
    search_criteria   :: Text
  , city              :: Maybe Text
  , state             :: Maybe Text
  , facets            :: Facets
  , search_type       :: Text
  , search_controller :: Text
  , types             :: [Text]
  , page_size         :: Int
  , page              :: Int
  }
  deriving (Show, Generic)

searchParams
  :: Text
  -> Text
  -> SearchParams
searchParams criteria controller =
  SearchParams {
            search_criteria = criteria
          , city = Nothing
          , state = Nothing
          , facets = def
          , search_type = "city"
          , search_controller = controller
          , types = ["property"]
          , page_size = 100
          , page = 1
          }


instance ToJSON SearchParams


data QueryResponse = QueryResponse {
    total :: Int
  , items :: [Item]
  } deriving (Show)

instance FromJSON QueryResponse where
  parseJSON = withObject "QueryResponse" $ \ qr -> do
    results <- qr .: "results"
    property <- results .: "property"
    (Object items') <- property .: "items"
    (Number total') <- property .: "total"
    case floatingOrInteger total' of
      Right (total'' :: Integer) ->
        QueryResponse
          <$> pure (fromIntegral total'')
          <*> (HM.elems <$> traverse parseJSON items')
      Left (_f :: Double) ->
        fail "encountered float in 'total' pagination field"




