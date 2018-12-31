{-# LANGUAGE OverloadedStrings #-}
module NetSpider.PangraphSpec (main, spec) where

import Data.List (sort)
import Data.Text (Text)
import NetSpider.Snapshot.Internal (SnapshotNode(..), SnapshotLink(..))
import NetSpider.Timestamp (fromS)
import qualified Pangraph as P
import Test.Hspec

import NetSpider.Pangraph (makeVertex, ToAttributes(..), makeEdge)
import NetSpider.Pangraph.Atom (ToAtom(..))

data SampleAttr = SampleAttr Text Int
                deriving (Show,Eq,Ord)

instance ToAttributes SampleAttr where
  toAttributes (SampleAttr t i) = [ ("text", toAtom t),
                                    ("int", toAtom i)
                                  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeVertex" $ do
    specify "text ID, empty attriubutes" $ do
      let sn :: SnapshotNode Text ()
          sn = SnapshotNode { _nodeId = "node ID",
                              _isOnBoundary = False,
                              _nodeTimestamp = Just $ fromS "2018-03-22T09:00:00+09:00",
                              _nodeAttributes = Just ()
                            }
          got = makeVertex sn
      P.vertexID got `shouldBe` "node ID"
      (sort $ P.vertexAttributes got)
        `shouldBe` [ ("@is_on_boundary", "False"),
                     ("@timestamp", "1521676800000"),
                     ("@tz_name", ""),
                     ("@tz_offset_min", "540"),
                     ("@tz_summer_only", "False")
                   ]
    specify "int ID, list attributes" $ do
      let attrs :: [(Text, Text)]
          attrs = [("foo", "bar"), ("quux", "100")]
          sn :: SnapshotNode Int [(Text, Text)]
          sn = SnapshotNode { _nodeId = 119,
                              _isOnBoundary = True,
                              _nodeTimestamp = Nothing,
                              _nodeAttributes = Just attrs
                            }
          got = makeVertex sn
      P.vertexID got `shouldBe` "119"
      (sort $ P.vertexAttributes got)
        `shouldBe` [ ("@is_on_boundary", "True"),
                     ("foo", "bar"),
                     ("quux", "100")
                   ]
  describe "makeEdge" $ do
    specify "text ID, SampleAttr" $ do
      let sl :: SnapshotLink Text SampleAttr
          sl = SnapshotLink { _sourceNode = "src",
                              _destinationNode = "dst",
                              _isDirected = True,
                              _linkTimestamp = fromS "2018-07-18T22:34:01",
                              _linkAttributes = SampleAttr "hoge" 256
                            }
          got = makeEdge sl
      P.edgeEndpoints got `shouldBe` ("src", "dst")
      (sort $ P.edgeAttributes got)
        `shouldBe` [ ("@is_directed", "True"),
                     ("@timestamp", "1531953241000"),
                     ("int", "256"),
                     ("text", "hoge")
                   ]
