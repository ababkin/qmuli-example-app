{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import qualified Data.Aeson                  as A
import qualified Data.Csv                    as Csv
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.Text                   as T
import           Protolude                   hiding (state)
import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEventsRuleProfile (ScheduledEventProfile))
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize, lpTimeoutSeconds)
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda,
                                              s3Bucket)
import           Qi.Program.Lambda.Interface (CwLambdaProgram, LambdaProgram,
                                              getS3ObjectContent, listS3Objects,
                                              putS3ObjectContent, runServant,
                                              say)
import           Qi.Util                     (success)
import qualified Realtor.Api                 as Api
import           Realtor.Item                (Item (Item))
import qualified Realtor.Item                as Item
import           Realtor.Search              (QueryResponse (..), SearchParams,
                                              searchParams)
import qualified Realtor.Search              as Search
import           Realtor.Util


data SearchLocation = SearchLocation {
    city  :: Text
  , state :: Text
  }

searchCriteriaLiteral :: SearchLocation -> Text
searchCriteriaLiteral SearchLocation{ city, state } = T.replace " " "-" $ city <> "_" <> state

searchLocations :: [SearchLocation]
searchLocations = [ SearchLocation "Madison" "NJ"
                  , SearchLocation "Chatham" "NJ"
                  , SearchLocation "Summit" "NJ"
                  , SearchLocation "Short Hills" "NJ"
                  , SearchLocation "Millburn" "NJ"
                  , SearchLocation "Maplewood" "NJ"
                  , SearchLocation "South Orange" "NJ"
                  , SearchLocation "Morristown" "NJ"
                  ]


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let fetchCron = ScheduledEventProfile "cron(* * * * ? *)" -- every minute

      recentlySoldId <- s3Bucket "recently-sold"
      void $ cwEventLambda "fetch" fetchCron (fetchLambda recentlySoldId) $ def
                                                                            & lpMemorySize .~ M512
                                                                            & lpTimeoutSeconds .~ 300

      let summaryCron = ScheduledEventProfile "cron(0 * * * ? *)" -- every minute
      summaryId <- s3Bucket "summary"

      void $ cwEventLambda "summary" summaryCron (summaryLambda recentlySoldId summaryId) $ def
                                                                            & lpMemorySize .~ M512
                                                                            & lpTimeoutSeconds .~ 300


    summaryLambda
      :: S3BucketId
      -> S3BucketId
      -> CwLambdaProgram
    summaryLambda recentlySoldBuckedId summaryBucketId _ = do
      objs <- listS3Objects recentlySoldBuckedId
      (items :: [Item]) <- catMaybes <$> traverse (map A.decode . getS3ObjectContent) objs

      let headers = [
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

      void $ putS3ObjectContent (S3Object summaryBucketId $ S3Key "summary.csv") $ Csv.encodeByName headers items
      success "success!"



    fetchLambda
      :: S3BucketId
      -> CwLambdaProgram
    fetchLambda bucketId _ = do

      for_ searchLocations $ \sl@SearchLocation{ city, state } -> do
        let criteria = searchCriteriaLiteral sl
            params = searchParams criteria "Search::RecentlySoldController"
        persistedPages <- persistPages params (persistItem bucketId) 1
        say $ "persisted " <> show persistedPages <> " pages for: '" <> criteria <> "'"

      success "success!"



