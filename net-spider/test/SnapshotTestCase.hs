{-# LANGUAGE GADTs, OverloadedStrings #-}
module SnapshotTestCase
  ( SnapshotTestCase(..),
    snapshotTestCases
  ) where

import Data.Aeson (ToJSON)
import Data.Greskell (FromGraphSON)
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Text (Text)
import Test.Hspec

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (NodeAttributes, LinkAttributes)
import NetSpider.Query (Query, defQuery)
import NetSpider.Snapshot
  ( SnapshotLink, SnapshotGraph,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary, nodeTimestamp
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)

import NetSpider.Timestamp (fromS)

data SnapshotTestCase where
  SnapshotTestCase ::
    ( FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n,
      Eq na, Show na, NodeAttributes na,
      Eq fla, Show fla, LinkAttributes fla, 
      Eq sla, Show sla
    ) =>
    { caseName :: String,
      caseInput :: [FoundNode n na fla],
      caseQuery :: Query n na fla sla,
      caseAssert :: SnapshotGraph n na sla -> IO ()
    } -> SnapshotTestCase

oneNeighborFoundNodes :: [FoundNode Text () ()]
oneNeighborFoundNodes = [nbs]
  where
    link = FoundLink { targetNode = "n2",
                       linkState = LinkToTarget,
                       linkAttributes = ()
                     }
    nbs = FoundNode { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T10:00",
                      neighborLinks = return link,
                      nodeAttributes = ()
                    }

snapshotTestCases :: [SnapshotTestCase]
snapshotTestCases =
  [ SnapshotTestCase
    { caseName = "one neighbor",
      caseInput = oneNeighborFoundNodes,
      caseQuery = defQuery ["n1"],
      caseAssert = one_neighbor_assert
    },
    SnapshotTestCase
    { caseName = "no neighbor",
      caseInput =
        ([ FoundNode { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T20:00",
                      neighborLinks = mempty,
                      nodeAttributes = ()
                     }
         ] :: [FoundNode Text () ()]),
      caseQuery = defQuery ["n1"],
      caseAssert = no_neighbor_assert
    }
  ]
  where
    one_neighbor_assert (got_ns, got_ls) = do
      let (got_n1, got_n2, got_link) = case (sort got_ns, sort got_ls) of
            ([a, b], [c]) -> (a, b, c)
            _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
      nodeId got_n1 `shouldBe` "n1"
      isOnBoundary got_n1 `shouldBe` False
      nodeTimestamp got_n1 `shouldBe` Just (fromS "2018-12-01T10:00")
      S.nodeAttributes got_n1 `shouldBe` Just ()
      nodeId got_n2 `shouldBe` "n2"
      isOnBoundary got_n2 `shouldBe` False
      nodeTimestamp got_n2 `shouldBe` Nothing -- n1 is not observed.
      S.nodeAttributes got_n2 `shouldBe` Nothing -- n1 is not observed.
      linkNodeTuple got_link `shouldBe` ("n1", "n2")
      isDirected got_link `shouldBe` True
      linkTimestamp got_link `shouldBe` fromS "2018-12-01T10:00"
      S.linkAttributes got_link `shouldBe` ()
    no_neighbor_assert (got_ns, got_ls) = do
      let got_n1 = case (sort got_ns, sort got_ls) of
            ([a], []) -> a
            _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
      nodeId got_n1 `shouldBe` "n1"
      isOnBoundary got_n1 `shouldBe` False
      nodeTimestamp got_n1 `shouldBe` Just (fromS "2018-12-01T20:00")
      S.nodeAttributes got_n1 `shouldBe` Just ()

