{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where

import           Control.Lens                hiding (parts)
import qualified Data.Aeson                  as A
import           Data.Default                (def)
import           Protolude                   hiding (state)
import           Qi                          (withConfig)
import           Qi.AWS.Lex                  (BotName (BotName))
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize, lpTimeoutSeconds)
import qualified Qi.CustomResource.Lex       as Lex
import           Qi.Program.Config.Interface (ConfigProgram, customResource,
                                              genericLambda)
import           Qi.Program.Lambda.Interface


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      lexBot <- customResource "LexBot"
                  (Lex.providerLambda $ BotName "myBot") def

{-
      let defaultLambdaProfile = def & lpMemorySize     .~ M512
                                     & lpTimeoutSeconds .~ 300

      genericLambda
              "logTest"
              logTest
              defaultLambdaProfile
-}
      pass

{-
    logTest
      :: Int -> LambdaProgram Text
    logTest input = do

      say "blah"
      say "{\"hello\": 3}"

      pure $ "success string: " <> show input
-}


