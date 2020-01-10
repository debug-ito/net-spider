{-# LANGUAGE OverloadedStrings #-}
module NetSpider.SnapshotSpec (main,spec) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Snapshot.Internal (SnapshotLink(..), SnapshotNode(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SnapshotLink" $ do
    specJSONFromTo
      "Text ID, () link attribute"
      (    "{\"source_node\": \"s\", \"dest_node\": \"d\","
        <> " \"is_directed\": false, \"link_attrs\": [],"
        <> " \"timestamp\": {\"epoch_time\": 5000} }"
      )
      SnapshotLink
      { _sourceNode = ("s" :: Text),
        _destinationNode = ("d" :: Text),
        _isDirected = False,
        _linkTimestamp = fromEpochMillisecond 5000,
        _linkAttributes = ()
      }
  describe "SnapshotNode" $ do
    specJSONFromTo
      "Int ID, null timestamp, Text attribute"
      (    "{\"node_id\": 999, \"is_on_boundary\": true,"
        <> " \"timestamp\": null, \"node_attrs\": \"foobar\"}"
      )
      SnapshotNode
      { _nodeId = (999 :: Int),
        _isOnBoundary = True,
        _nodeTimestamp = Nothing,
        _nodeAttributes = Just ("foobar" :: Text)
      }
