{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module NetSpider.GraphML.WriterSpec (main,spec) where

import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.IO as TLIO
import GHC.Generics (Generic)
import NetSpider.Snapshot.Internal
  ( SnapshotNode(..), SnapshotLink(..)
  )
import NetSpider.Timestamp (fromEpochMillisecond, fromS)
import Test.Hspec

import NetSpider.GraphML.Writer
  ( writeGraphML,
    ToAttributes(..),
    AttributeValue(..),
    attributesFromAeson
  )

main :: IO ()
main = hspec spec

data Att1 =
  Att1
  { at1Hoge :: Int,
    at1Foo :: Text,
    at1Buzz :: Bool
  }
  deriving (Show,Eq,Ord)

instance ToAttributes Att1 where
  toAttributes a = [ ("hoge", AttrInt $ at1Hoge a),
                     ("foo", AttrString $ at1Foo a),
                     ("buzz", AttrBoolean $ at1Buzz a)
                   ]

data Att2 =
  Att2
  { at2_quux :: Double,
    at2_huga :: Text
  }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON Att2 where
  toEncoding = genericToEncoding defaultOptions

instance ToAttributes Att2 where
  toAttributes a = sortOn fst $ fromJust $ attributesFromAeson $ toJSON a

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
    specify "with attributes" $ do
      let nodes :: [SnapshotNode Int Att1]
          nodes = [ SnapshotNode
                    { _nodeId = 100,
                      _isOnBoundary = False,
                      _nodeTimestamp = Just $ fromEpochMillisecond 155,
                      _nodeAttributes = Just $ Att1
                                        { at1Hoge = 99,
                                          at1Foo = "new\nline",
                                          at1Buzz = False
                                        }
                    },
                    SnapshotNode
                    { _nodeId = 200,
                      _isOnBoundary = False,
                      _nodeTimestamp = Nothing,
                      _nodeAttributes = Just $ Att1
                                        { at1Hoge = 2099,
                                          at1Foo = "",
                                          at1Buzz = True
                                        }
                    }
                  ]
          links :: [SnapshotLink Int Att2]
          links = [ SnapshotLink
                    { _sourceNode = 100,
                      _destinationNode = 200,
                      _isDirected = True,
                      _linkTimestamp = fromEpochMillisecond 155,
                      _linkAttributes = Att2
                                        { at2_quux = 109.25,
                                          at2_huga = "HUGA"
                                        }
                    
                    }
                  ]
          expected = mconcat $ map (<> "\n")
                     [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                       "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"",
                       " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
                       " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">",
                       "<key id=\"d0\" for=\"node\" attr.name=\"@timestamp\" attr.type=\"long\"/>",
                       "<key id=\"d1\" for=\"node\" attr.name=\"@is_on_boundary\" attr.type=\"boolean\"/>",
                       "<key id=\"d2\" for=\"node\" attr.name=\"hoge\" attr.type=\"int\"/>",
                       "<key id=\"d3\" for=\"node\" attr.name=\"foo\" attr.type=\"string\"/>",
                       "<key id=\"d4\" for=\"node\" attr.name=\"buzz\" attr.type=\"boolean\"/>",
                       "<key id=\"d5\" for=\"edge\" attr.name=\"@timestamp\" attr.type=\"long\"/>",
                       "<key id=\"d6\" for=\"edge\" attr.name=\"at2_huga\" attr.type=\"string\"/>",
                       "<key id=\"d7\" for=\"edge\" attr.name=\"at2_quux\" attr.type=\"double\"/>",
                       "<graph edgedefault=\"undirected\">",
                       "  <node id=\"100\">",
                       "    <data key=\"d0\">155</data>",
                       "    <data key=\"d1\">false</data>",
                       "    <data key=\"d2\">99</data>",
                       "    <data key=\"d3\">new&#x0a;line</data>",
                       "    <data key=\"d4\">false</data>",
                       "  </node>",
                       "  <node id=\"200\">",
                       "    <data key=\"d1\">false</data>",
                       "    <data key=\"d2\">2099</data>",
                       "    <data key=\"d3\"></data>",
                       "    <data key=\"d4\">true</data>",
                       "  </node>",
                       "  <edge source=\"100\" target=\"200\" directed=\"true\">",
                       "    <data key=\"d5\">155</data>",
                       "    <data key=\"d6\">HUGA</data>",
                       "    <data key=\"d7\">109.25</data>",
                       "  </edge>",
                       "</graph>",
                       "</graphml>"
                     ]
          got = writeGraphML nodes links
      -- TLIO.putStrLn got
      got `shouldBe` expected
