{-# LANGUAGE NamedFieldPuns      #-}
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

      let fetchRecentlySoldCron = ScheduledEventProfile "cron(1 * * * ? *)"

      recentlySoldBucketId <- s3Bucket "recently-sold"
      forSaleBucketId <- s3Bucket "for-sale"
      outputBucketId <- s3Bucket "output"

      cwEventLambda "fetchRecentlySold" fetchRecentlySoldCron (fetchRecentlySoldLambda recentlySoldBucketId) $ def
                                                                            & lpMemorySize .~ M512
                                                                            & lpTimeoutSeconds .~ 300

      let fetchForSaleCron = ScheduledEventProfile "cron(2 * * * ? *)"

      cwEventLambda "fetchForSale" fetchForSaleCron (fetchForSaleLambda forSaleBucketId) $ def
                                                                            & lpMemorySize .~ M512
                                                                            & lpTimeoutSeconds .~ 300


      let extractCron = ScheduledEventProfile "cron(0 * * * ? *)"

      void $ cwEventLambda "extractRecentlySold" extractCron (extractItemsLambda recentlySoldBucketId outputBucketId "recently-sold.csv")
              $ def
                & lpMemorySize .~ M512
                & lpTimeoutSeconds .~ 300

      void $ cwEventLambda "extractForSale" extractCron (extractItemsLambda forSaleBucketId outputBucketId "for-sale.csv")
              $ def
                & lpMemorySize .~ M512
                & lpTimeoutSeconds .~ 300


    extractItemsLambda
      :: S3BucketId
      -> S3BucketId
      -> Text
      -> CwLambdaProgram
    extractItemsLambda fromBuckedId toBucketId filename _ = do
      objs <- listS3Objects fromBuckedId
      (items :: [Item]) <- catMaybes <$> traverse (map A.decode . getS3ObjectContent) objs

      void $ putS3ObjectContent (S3Object toBucketId $ S3Key filename) $ Csv.encodeByName Item.headers items
      success "success!"



    fetchRecentlySoldLambda
      :: S3BucketId
      -> CwLambdaProgram
    fetchRecentlySoldLambda bucketId _ = do

      for_ searchLocations $ \sl@SearchLocation{ city, state } -> do
        let criteria = searchCriteriaLiteral sl
            params = searchParams criteria "Search::RecentlySoldController"
        persistedPages <- persistPages Api.recentlySold params (persistItem bucketId) 1
        say $ "persisted " <> show persistedPages <> " pages for: '" <> criteria <> "'"

      success "success!"


    fetchForSaleLambda
      :: S3BucketId
      -> CwLambdaProgram
    fetchForSaleLambda bucketId _ = do

      for_ searchLocations $ \sl@SearchLocation{ city, state } -> do
        let criteria = searchCriteriaLiteral sl
            params = searchParams criteria "Search::PropertiesController"
        persistedPages <- persistPages Api.forSale params (persistItem bucketId) 1
        say $ "persisted " <> show persistedPages <> " pages for: '" <> criteria <> "'"

      success "success!"

