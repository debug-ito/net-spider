{-# LANGUAGE OverloadedStrings #-}
module NetSpider.FoundSpec (main,spec) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.Found (FoundLink(..), LinkState(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FoundLink" $ do
    specJSONFromTo
      "Int attribute"
      "{\"target_node\":\"target\", \"link_state\": \"to_target\", \"link_attrs\":125}"
      FoundLink
      { targetNode = ("target" :: Text),
        linkState = LinkToTarget,
        linkAttributes = (125 :: Int)
      }
