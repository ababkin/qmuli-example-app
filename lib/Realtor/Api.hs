{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Realtor.Api where

import           Control.Monad.Fail  (fail)
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Proxy
import           Data.Scientific
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Protolude
import           Realtor.Item
import           Realtor.Search
import           Servant.API
import           Servant.Client


type Api =
    "pagination_result.json"
  :> Header "content-type" Text
  :> Header "accept" Text
  :> ReqBody '[JSON] SearchParams :> Post '[JSON] QueryResponse

api :: Proxy Api
api = Proxy

fetch
  :: Maybe Text
  -> Maybe Text
  -> SearchParams
  -> ClientM QueryResponse

fetch = client api

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: [Char]   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: [Char]   -- ^ path (eg "/a/b/c")
  }




{-
curl 'https://www.realtor.com/pagination_result.json' \
  -H 'content-type: application/json' \
  -H 'accept: application/json, text/javascript, */*; q=0.01' \
  --data-binary '{"search_criteria":"Madison_NJ","discovery_mode":false,"postal":null,"sort":null,"position":null,"facets":{"beds_min":"3","beds_max":"","baths_min":null,"baths_max":null,"price_min":null,"price_max":null,"prop_type":"","sqft_min":null,"sqft_max":null,"acre_min":null,"acre_max":null,"lot_unit":null,"age_max":null,"age_min":null,"radius":null,"pets":null,"days_on_market":null,"open_house":null,"show_listings":null,"pending":null,"foreclosure":null,"new_construction":null,"multi_search":{},"include_pending_contingency":false,"features_hash":[],"prop_types":[""]},"search_controller":"Search::PropertiesController","neighborhood":null,"street":null,"searchType":"city","school":null,"types":["property"],"searchFacetsToDTM":["beds_min"],"searchFeaturesToDTM":[],"page_size":80,"viewport_height":216,"pin_height":25}' \
  --compressed | jq .
-}


{-
curl 'https://www.realtor.com/search_result.json' \
  -H 'content-type: application/json' \
  -H 'accept: application/json, text/javascript, */*; q=0.01' \
  --data-binary '{"search_criteria":"Madison_NJ","city":"Madison","county":"Morris","discovery_mode":false,"state":"NH","postal":null,"sort":null,"position":null,"facets":{"beds_min":"3","beds_max":"","baths_min":null,"baths_max":null,"price_min":null,"price_max":null,"prop_type":"","sqft_min":null,"sqft_max":null,"acre_min":null,"acre_max":null,"lot_unit":null,"age_max":null,"age_min":null,"radius":null,"pets":null,"days_on_market":null,"open_house":null,"show_listings":null,"pending":null,"foreclosure":null,"new_construction":null,"multi_search":{},"include_pending_contingency":false,"features_hash":[],"prop_types":[""]},"search_controller":"Search::RecentlySoldController","neighborhood":null,"street":null,"searchType":"city","school":null,"types":["property"],"searchFacetsToDTM":["beds_min"],"searchFeaturesToDTM":[],"page_size":80,"viewport_height":216,"pin_height":25}' \
  --compressed | jq .
-}


{-
        "3717832792": {
          "agentId": null,
          "ldpUrl": "/realestateandhomes-detail/14-Dow-St_Nashua_NH_03064_M37178-32792",
          "id": "3717832792",
          "property_id": "3717832792",
          "source_id": "M",
          "plan_id": 3717832792,
          "subdivision_id": null,
          "listing_id": 3717832792,
          "isStatusPending": null,
          "isSaved": false,
          "mprId": 3717832792,
          "officeName": null,
          "plot": true,
          "position": {
            "coordinates": [
              -71.463906,
              42.76724
            ],
            "type": "Point"
          },
          "status": "recently_sold",
          "type": "property",
          "product_type": "basic",
          "search_flags": {
            "is_price_reduced": false,
            "is_pending": false,
            "is_new_listing": false,
            "is_short_sale": null,
            "is_showcase": false,
            "has_tour": false,
            "has_video": false,
            "has_realtor_logo": false,
            "is_cobroke": false,
            "is_coshow_product": false,
            "is_foreclosure": false,
            "is_new_plan": false,
            "price_excludes_land": null,
            "has_deals": null,
            "is_new": null,
            "is_costar": false,
            "is_address_suppressed": false,
            "is_suppress_map_pin": false,
            "is_suppress_map": false,
            "has_sign_rider": null,
            "is_aptlist": false
          },
          "open_house_display": null,
          "sort": 0,
          "isCoBroke": false,
          "isEnhanced": false,
          "isForeclosure": false,
          "isPriceReduced": false,
          "isAddressSuppressed": false,
          "bed": 4,
          "bath": "1+",
          "lotSize": 2614,
          "photo": null,
          "photoCount": 0,
          "price": 214933,
          "priceDisplay": "$214,933",
          "propertyType": "single_family",
          "sqft": 1888,
          "sqft_display": "1,888",
          "address": "14 Dow St",
          "city": "Nashua",
          "county": "Hillsborough",
          "state": "New Hampshire",
          "zip": "03064",
          "pinPrice": null,
          "data_source": null,
          "agent_id": null

  -}



{-

curl 'https://www.realtor.com/search_result.json' \
  -H 'content-type: application/json' \
  -H 'accept: application/json, text/javascript, */*; q=0.01' \
  --data-binary '{"search_criteria":"Madison_NJ","city":"Madison","state":"NJ","facets":{"days_on_market":null,"show_listings":null,"foreclosure":null},"search_controller":"Search::RecentlySoldController","types":["property"]}' \
  --compressed | jq .

-}


-- https://www.realtor.com/search_result.json

