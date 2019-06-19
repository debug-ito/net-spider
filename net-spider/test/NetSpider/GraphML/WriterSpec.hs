{-# LANGUAGE OverloadedStrings #-}
module NetSpider.GraphML.WriterSpec (main,spec) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import NetSpider.Snapshot.Internal
  ( SnapshotNode(..), SnapshotLink(..)
  )
import NetSpider.Timestamp (fromEpochMillisecond)
import Test.Hspec

import NetSpider.GraphML.Writer (writeGraphML)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeGraphML" $ do
    specify "no attribute, directed and undirected mixed, node id escaped" $ do
      let nodes :: [SnapshotNode Text ()]
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
                      _nodeTimestamp = Just $ fromEpochMillisecond 120,
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
                      _linkTimestamp = fromEpochMillisecond 120,
                      _linkAttributes = ()
                    }
                  ]
          expected = TL.intercalate "\n"
                     [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                       "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"",
                       "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
                       "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">",
                       "<key id=\"d0\" for=\"all\" attr.name=\"@timestamp\" attr.type=\"long\"/>",
                       "<graph edgedefault=\"undirected\">",
                       "  <node id=\"&quot;the root&quot;\">",
                       "    <data key=\"d0\">100</data>",
                       "  </node>",
                       "  <node id=\"☃\">",
                       "  </node>",
                       "  <node id=\"&lt;child&gt;\">",
                       "    <data key=\"d0\">120</data>",
                       "  </node>",
                       "  <edge source=\"&quot;the root&quot;\" target=\"☃\" directed=\"true\">",
                       "    <data key=\"d0\">100</data>",
                       "  </edge>",
                       "  <edge source=\"&lt;child&gt;\" target=\"&quot;the root&quot;\" directed=\"false\">",
                       "    <data key=\"d0\">120</data>",
                       "  </edge>",
                       "</graph>"
                     ]
      writeGraphML nodes links `shouldBe` expected
