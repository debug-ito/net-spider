{-# LANGUAGE OverloadedStrings #-}
module NetSpider.FoundSpec (main,spec) where

import qualified Data.Aeson as Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.Found (FoundLink(..), LinkState(..), FoundNode(..))
import NetSpider.Timestamp (fromEpochMillisecond)

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
  describe "FoundNode" $ do
    specJSONFromTo
      "Text attribute, () attribute (encoded as an empty array [])"
      (    "{\"subject_node\": \"foobar\","
        <> " \"found_at\": {\"epoch_time\": 99200},"
        <> " \"node_attrs\": \"hoge\","
        <> " \"neighbor_links\": ["
        <> "   {\"target_node\": \"quux\", \"link_state\": \"to_subject\", \"link_attrs\": []}"
        <> " ]}"
      )
      FoundNode
      { subjectNode = ("foobar" :: Text),
        foundAt = fromEpochMillisecond 99200,
        nodeAttributes = ("hoge" :: Text),
        neighborLinks =
          [ FoundLink
            { targetNode = ("quux" :: Text),
              linkState = LinkToSubject,
              linkAttributes = ()
            }
          ]
      }
