{-# LANGUAGE NamedFieldPuns #-}

module Realtor.Util where

import qualified Data.Aeson                  as A
import qualified Data.ByteString.Base64      as B64
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Protolude                   hiding (state)
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Lambda.Interface (LambdaProgram, putS3ObjectContent,
                                              runServant, say, sleep)
import           Realtor.Item                (Item (Item))
import qualified Realtor.Item                as Item
import           Realtor.Search              (QueryResponse (..), SearchParams)
import qualified Realtor.Search              as Search
import           Servant.Client


contentType :: Text
contentType = "application/json"

accept :: Text
accept = "application/json, text/javascript, */*; q=0.01"

host :: [Char]
host = "realtor.com"

port :: Int
port = 80


persistPages
  :: (Maybe Text -> Maybe Text -> SearchParams -> ClientM QueryResponse)
  -> SearchParams
  -> (Item -> LambdaProgram ())
  -> Int
  -> LambdaProgram Int
persistPages api params persist page = do
  result <- runServant tlsManagerSettings
                (BaseUrl Http host port "")
                (api (Just contentType) (Just accept) params{ Search.page = page })
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
          persistPages api params persist $ page + 1

persistItem
  :: S3BucketId
  -> Item
  -> LambdaProgram ()
persistItem bucketId item@Item { Item.address  = address
                  , Item.city     = city
                  , Item.state    = state
                  } =
  void $ putS3ObjectContent (S3Object bucketId $ S3Key key) $ A.encode item
  where
    key = toS . B64.encode . show $ hash (address, city, state)


