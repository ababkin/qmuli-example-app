{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Base64      as B64
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.Text                   as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
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
                                              getS3ObjectContent,
                                              putS3ObjectContent, runServant,
                                              say, sleep)
import           Qi.Util                     (success)
import           Realtor.Api                 (QueryResponse (..),
                                              SearchCriteria, search)
import qualified Realtor.Api                 as Api
import           Realtor.Item                (Item (Item))
import qualified Realtor.Item                as Item
import           Servant.Client


data SearchLocation = SearchLocation {
    city  :: Text
  , state :: Text
  }

searchCriteria :: SearchLocation -> Text
searchCriteria SearchLocation{ city, state } = T.replace " " "-" $ city <> "_" <> state

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

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)" -- every minute

      recentlySoldId <- s3Bucket "recently-sold"

      void $ cwEventLambda "myEventLambda" ruleProfile (eventLambda recentlySoldId) $ def
                                                                                      & lpMemorySize .~ M512
                                                                                      & lpTimeoutSeconds .~ 300

    eventLambda
      :: S3BucketId
      -> CwLambdaProgram
    eventLambda bucketId _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream


      for_ searchLocations $ \sl@SearchLocation{ city, state } -> do
        let search_criteria = searchCriteria sl
        persistedPages <- persistPages def{ Api.city = city
                                          , Api.state = state
                                          , Api.search_criteria = search_criteria
                                          } 1
        say $ "persisted " <> show persistedPages <> " pages for: '" <> search_criteria <> "'"

      success "success!"

      where
        contentType = "application/json"
        accept      = "application/json, text/javascript, */*; q=0.01"

        persistPages criteria page = do
          result <- runServant tlsManagerSettings
                        (BaseUrl Http "realtor.com" 80 "")
                        (search (Just contentType) (Just accept) criteria{ Api.page = page })
          case result of
            Left err -> do
              say $ "Error: " <> show err
              pure $ page - 1
            Right (QueryResponse{ items }) -> do
              traverse_ persist items
              if null items
                then pure $ page - 1
                else do
                  sleep 500000
                  persistPages criteria $ page + 1

        persist
          :: Item
          -> LambdaProgram ()
        persist item@Item { Item.address  = address
                          , Item.city     = city
                          , Item.state    = state
                          } =
          void $ putS3ObjectContent (S3Object bucketId $ S3Key key) $ A.encode item
          where
            key = toS . B64.encode . show $ hash (address, city, state)


