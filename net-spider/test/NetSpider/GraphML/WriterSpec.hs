{-# LANGUAGE OverloadedStrings #-}
module NetSpider.GraphML.WriterSpec (main,spec) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.IO as TLIO
import NetSpider.Snapshot.Internal
  ( SnapshotNode(..), SnapshotLink(..)
  )
import NetSpider.Timestamp (fromEpochMillisecond, fromS)
import Test.Hspec

import NetSpider.GraphML.Writer (writeGraphML)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeGraphML" $ do
    specify "no attribute, directed and undirected mixed, node id escaped" $ do
      let time_with_tz = fromS "2018-09-23T08:48:52+09:00"
          nodes :: [SnapshotNode Text ()]
          nodes = [ SnapshotNode
                    { _nodeId = "\"the root\"",
                      _isOnBoundary = False,
                      _nodeTimestamp = Just $ fromEpochMillisecond 100,
                      _nodeAttributes = Just ()
                    },
                    SnapshotNode
                    { _nodeId = "☃",
                      _isOnBoundary = True,
                      _nodeTimestamp = Nothing,
                      _nodeAttributes = Nothing
                    },
                    SnapshotNode
                    { _nodeId = "<child>",
                      _isOnBoundary = False,
                      _nodeTimestamp = Just $ time_with_tz,
                      _nodeAttributes = Just ()
                    }
                  ]
          links :: [SnapshotLink Text ()]
          links = [ SnapshotLink
                    { _sourceNode = "\"the root\"",
                      _destinationNode = "☃",
                      _isDirected = True,
                      _linkTimestamp = fromEpochMillisecond 100,
                      _linkAttributes = ()
                    },
                    SnapshotLink
                    { _sourceNode = "<child>",
                      _destinationNode = "\"the root\"",
                      _isDirected = False,
                      _linkTimestamp = time_with_tz,
                      _linkAttributes = ()
                    }
                  ]
          expected = mconcat $ map (<> "\n")
                     [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                       "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"",
                       " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
                       " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">",
                       "<key id=\"d0\" for=\"node\" attr.name=\"@timestamp\" attr.type=\"long\"/>",
                       "<key id=\"d1\" for=\"node\" attr.name=\"@is_on_boundary\" attr.type=\"boolean\"/>",
                       "<key id=\"d2\" for=\"node\" attr.name=\"@tz_offset_min\" attr.type=\"int\"/>",
                       "<key id=\"d3\" for=\"node\" attr.name=\"@tz_summer_only\" attr.type=\"boolean\"/>",
                       "<key id=\"d4\" for=\"node\" attr.name=\"@tz_name\" attr.type=\"string\"/>",
                       "<key id=\"d5\" for=\"edge\" attr.name=\"@timestamp\" attr.type=\"long\"/>",
                       "<key id=\"d6\" for=\"edge\" attr.name=\"@tz_offset_min\" attr.type=\"int\"/>",
                       "<key id=\"d7\" for=\"edge\" attr.name=\"@tz_summer_only\" attr.type=\"boolean\"/>",
                       "<key id=\"d8\" for=\"edge\" attr.name=\"@tz_name\" attr.type=\"string\"/>",
                       "<graph edgedefault=\"undirected\">",
                       "  <node id=\"&quot;the root&quot;\">",
                       "    <data key=\"d0\">100</data>",
                       "    <data key=\"d1\">false</data>",
                       "  </node>",
                       "  <node id=\"☃\">",
                       "    <data key=\"d1\">true</data>",
                       "  </node>",
                       "  <node id=\"&lt;child&gt;\">",
                       "    <data key=\"d0\">1537660132000</data>",
                       "    <data key=\"d2\">540</data>",
                       "    <data key=\"d3\">false</data>",
                       "    <data key=\"d4\"></data>",
                       "    <data key=\"d1\">false</data>",
                       "  </node>",
                       "  <edge source=\"&quot;the root&quot;\" target=\"☃\" directed=\"true\">",
                       "    <data key=\"d5\">100</data>",
                       "  </edge>",
                       "  <edge source=\"&lt;child&gt;\" target=\"&quot;the root&quot;\" directed=\"false\">",
                       "    <data key=\"d5\">1537660132000</data>",
                       "    <data key=\"d6\">540</data>",
                       "    <data key=\"d7\">false</data>",
                       "    <data key=\"d8\"></data>",
                       "  </edge>",
                       "</graph>",
                       "</graphml>"
                     ]
          got = writeGraphML nodes links
      -- TLIO.putStrLn got
      got `shouldBe` expected
