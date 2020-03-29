{-# LANGUAGE OverloadedStrings #-}
module NetSpider.SeqIDSpec (main,spec) where

import Data.Text (Text)
import Test.Hspec

import NetSpider.SeqID
  ( newSeqIDMaker, convertNodeID, convertGraph, originalIDFor
  )
import NetSpider.Snapshot.Internal (SnapshotNode(..), SnapshotLink(..))
import NetSpider.Snapshot (nodeId, linkNodeTuple)
import NetSpider.Timestamp (fromEpochMillisecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "SeqIDMaker" $ do
  specify "first ID" $ do
    let m = newSeqIDMaker (0 :: Int)
        (got_m, got) = convertNodeID m ("hoge" :: Text)
    got `shouldBe` 0
    (snd $ convertNodeID got_m "hoge") `shouldBe` 0
    originalIDFor got_m 0 `shouldBe` Just "hoge"
    originalIDFor got_m 1 `shouldBe` Nothing
    originalIDFor got_m 2 `shouldBe` Nothing
  specify "second ID" $ do
    let m = newSeqIDMaker (0 :: Int)
        (m1, _) = convertNodeID m ("bar" :: Text)
        (got_m, got) = convertNodeID m1 ("foo" :: Text)
    got `shouldBe` 1
    (snd $ convertNodeID got_m "bar") `shouldBe` 0
    (snd $ convertNodeID got_m "foo") `shouldBe` 1
    originalIDFor got_m 0 `shouldBe` Just "bar"
    originalIDFor got_m 1 `shouldBe` Just "foo"
    originalIDFor got_m 2 `shouldBe` Nothing
  specify "duplicate ID" $ do
    let m = newSeqIDMaker (5 :: Int)
        (m1, id1) = convertNodeID m ("bar" :: Text)
        (got_m,  id2) = convertNodeID m1 ("bar" :: Text)
    id1 `shouldBe` 5
    id2 `shouldBe` 5
    originalIDFor got_m 0 `shouldBe` Nothing
    originalIDFor got_m 5 `shouldBe` Just "bar"
  specify "convertGraph" $ do
    let input = (nodes, links)
        mkNode :: Text -> SnapshotNode Text ()
        mkNode nid = SnapshotNode
                     { _nodeId = nid,
                       _isOnBoundary = False,
                       _nodeTimestamp = Just $ fromEpochMillisecond 100,
                       _nodeAttributes = Just ()
                     }
        mkLink :: Text -> Text -> SnapshotLink Text ()
        mkLink source dest = SnapshotLink
                             { _sourceNode = source,
                               _destinationNode = dest,
                               _isDirected = True,
                               _linkTimestamp = fromEpochMillisecond 100,
                               _linkAttributes = ()
                             }
        nodes = [ mkNode "n1",
                  mkNode "n2",
                  mkNode "n3",
                  mkNode "n4",
                  mkNode "n5"
                ]
        links = [ mkLink "n1" "n3",
                  mkLink "n4" "n2",
                  mkLink "n3" "n5",
                  mkLink "n5" "n2"
                ]
        (got_m, (got_nodes, got_links)) = convertGraph (newSeqIDMaker (1 :: Int)) input
    map nodeId got_nodes `shouldBe` [1,2,3,4,5]
    map linkNodeTuple got_links `shouldBe`
      [ (1, 3), (4, 2), (3, 5), (5, 2)
      ]
    map (originalIDFor got_m) [1 .. 6] `shouldBe`
      [ Just "n1", Just "n2", Just "n3", Just "n4", Just "n5", Nothing
      ]

