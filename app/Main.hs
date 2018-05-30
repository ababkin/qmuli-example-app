{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Default                (def)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Protolude                   hiding (state)
import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEventsRuleProfile (ScheduledEventProfile))
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda)
import           Qi.Program.Lambda.Interface (CwLambdaProgram, runServant, say)
import           Qi.Util                     (success)
import           Realtor.Api                 (SearchCriteria (..), search)
import           Servant.Client


criteria :: SearchCriteria
criteria = SearchCriteria {
    search_criteria = "Madison_NJ"
  , city = "Madison"
  , state = "NJ"
  , facets = def
  , search_controller = "Search::RecentlySoldController"
  , types = ["property"]
  , page_size = 10
  , page = 1
  }


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"

      void $ cwEventLambda "myEventLambda" ruleProfile eventLambda def

    eventLambda
      :: CwLambdaProgram
    eventLambda _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      say "tick"
      let contentType = "application/json"
          accept      = "application/json, text/javascript, */*; q=0.01"
      result <- runServant tlsManagerSettings
                        (BaseUrl Http "realtor.com" 80 "")
                        (search (Just contentType) (Just accept) criteria)
      case result of
        Left err ->
          say $ "Error: " <> show err
        Right res ->
          say $ "Success: " <> show res

      success "lambda had executed successfully"


