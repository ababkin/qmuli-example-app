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
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Protolude                   hiding (state)
import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEventsRuleProfile (ScheduledEventProfile))
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda)
import           Qi.Program.Config.Interface (ConfigProgram, s3Bucket)
import           Qi.Program.Lambda.Interface (CwLambdaProgram, LambdaProgram,
                                              getS3ObjectContent,
                                              putS3ObjectContent, runServant,
                                              say)
import           Qi.Util                     (success)
import           Realtor.Api                 (QueryResponse (..),
                                              SearchCriteria (..), search)
import           Realtor.Item                (Item (Item))
import qualified Realtor.Item                as Item
import           Servant.Client



main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)" -- every minute

      recentlySoldId <- s3Bucket "recently-sold"

      void $ cwEventLambda "myEventLambda" ruleProfile (eventLambda recentlySoldId) def

    eventLambda
      :: S3BucketId
      -> CwLambdaProgram
    eventLambda bucketId _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      say "tick"
      let contentType = "application/json"
          accept      = "application/json, text/javascript, */*; q=0.01"
      result <- runServant tlsManagerSettings
                        (BaseUrl Http "realtor.com" 80 "")
                        (search (Just contentType) (Just accept) def)
      case result of
        Left err ->
          say $ "Error: " <> show err
        Right (QueryResponse{ items }) ->
          traverse_ persist items

      success "lambda had executed successfully"

      where
        persist
          :: Item
          -> LambdaProgram ()
        persist item@Item { Item.address = address
                          , Item.city = city
                          , Item.state = state
                          } =
          void $ putS3ObjectContent (S3Object bucketId $ S3Key key) $ A.encode item
          where
            key = toS . B64.encode . show $ hash (address, city, state)


