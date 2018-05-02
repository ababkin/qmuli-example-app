{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Data.Aeson
import           Data.Default                    (def)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan       hiding (scan)
import           Protolude                       hiding (get, put, (&))
import           Qi                              (withConfig)
import           Qi.Config.AWS.ApiGw             (ApiVerb (Delete, Get, Post))
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..))
import           Qi.Config.AWS.Lambda            (LambdaMemorySize (..),
                                                  lpMemorySize)
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram, api,
                                                  apiMethodLambda, apiResource,
                                                  ddbTable)
import           Qi.Program.Lambda.Interface     (ApiLambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, say,
                                                  scanDdbRecords)
import           Qi.Util                         (internalError, result,
                                                  success, withSuccess)
import           Qi.Util.ApiGw
import           Qi.Util.DDB
import           Types


-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://5p07mx81w8.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things"
curl -v -X GET "$API/things/cup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/cup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      thingsTable <- ddbTable "things" (DdbAttrDef "name" S) def

      -- create a REST API
      api "world" >>= \world ->
        -- create a "things" resource
        apiResource "things" world >>= \things -> do
          -- create a GET method that is attached to the "scan" lambda
          apiMethodLambda "scanThings" Get
            things def (scan thingsTable) $
              def
              & lpMemorySize .~ M1024

          apiMethodLambda "putThing" Post
            things def (put thingsTable) $
              def
              & lpMemorySize .~ M1024

          -- create a "thingId" slug resource under "things"
          apiResource "{thingId}" things >>= \thing -> do

            apiMethodLambda "getThing" Get
              thing def (get thingsTable) def

            apiMethodLambda "deleteThing" Delete
              thing def (delete thingsTable) def

      return ()

    scan
      :: DdbTableId
      -> ApiLambdaProgram
    scan ddbTableId _ = do
      say "scanning records..."
      r <- scanDdbRecords ddbTableId
      say $ "got scan response: " <> show r
      withSuccess (r^.srsResponseStatus) $
        result
          (internalError . ("Parsing error: " <>))
          (success . (toJSON :: [Thing] -> Value))
          $ forM (r^.srsItems) parseAttrs


    get
      :: DdbTableId
      -> ApiLambdaProgram
    get ddbTableId event =
      withPathParam "thingId" event $ \tid -> do
        say $ "getting record with thingId: '" <> tid <> "'..."
        r <- getDdbRecord ddbTableId $ byNameKey tid

        withSuccess (r^.girsResponseStatus) $
          result
            (internalError . ("Parsing error: " <>))
            (success . (toJSON :: Thing -> Value))
            $ parseAttrs $ r^.girsItem


    put
      :: DdbTableId
      -> ApiLambdaProgram
    put ddbTableId event =
      withDeserializedBody event $ \(thing :: Thing) -> do
        say $ "putting record: " <>  show thing <> "..."
        r <- putDdbRecord ddbTableId $ toAttrs thing
        say "...done"

        withSuccess (r^.pirsResponseStatus) $
          success "successfully put thing"


    delete
      :: DdbTableId
      -> ApiLambdaProgram
    delete ddbTableId event = do
      withPathParam "thingId" event $ \tid -> do
        say $ "deletting record with thingId: '" <> tid <> "'..."
        r <- deleteDdbRecord ddbTableId $ byNameKey tid
        withSuccess (r^.dirsResponseStatus) $
          success "successfully deleted thing"





