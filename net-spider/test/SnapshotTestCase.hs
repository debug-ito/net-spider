{-# LANGUAGE GADTs, OverloadedStrings #-}
module SnapshotTestCase
  ( SnapshotTestCase(..),
    snapshotTestCases
  ) where

import Data.Aeson (ToJSON)
import Data.Greskell (FromGraphSON)
import Data.Hashable (Hashable)
import Data.List (sort, sortOn)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Data.Vector ((!), Vector)
import Test.Hspec

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (NodeAttributes, LinkAttributes)
import NetSpider.Query (Query, defQuery, unifyLinkSamples)
import NetSpider.Snapshot
  ( SnapshotLink, SnapshotGraph,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary, nodeTimestamp
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Unify
  ( UnifyStdConfig(..), defUnifyStdConfig, unifyStd, latestLinkSample
  )

import NetSpider.Timestamp (fromS)

import TestCommon
  ( AText(..), APorts(..),
    sortSnapshotElements, subIdWithAPorts, alignAPortsToLinkDirection
  )




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

queryWithAPorts :: Text -> Query Text () APorts APorts
queryWithAPorts nid = (defQuery [nid]) { unifyLinkSamples = unifyStd unify_conf }
  where
    unify_conf = defUnifyStdConfig
                 { makeLinkSubId = subIdWithAPorts,
                   mergeSamples = \l r -> fmap alignAPortsToLinkDirection $ latestLinkSample (l ++ r)
                 }

sortLinksWithAttr :: (Ord n, Ord la) => Vector (SnapshotLink n la) -> Vector (SnapshotLink n la)
sortLinksWithAttr = V.fromList . sortOn getKey . V.toList
  where
    getKey link = (linkNodeTuple link, S.linkAttributes link)

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
    },
    SnapshotTestCase
    { caseName = "missing starting node",
      caseInput = oneNeighborFoundNodes,
      caseQuery = defQuery ["no node"],
      caseAssert = \(got_ns, got_ls) -> do
        got_ns `shouldBe` mempty
        got_ls `shouldBe` mempty
    },
    SnapshotTestCase
    { caseName = "mutual neighbors",
      caseInput =
        let link_12 :: FoundLink Text ()
            link_12 = FoundLink { targetNode = "n2",
                                  linkState = LinkToSubject,
                                  linkAttributes = ()
                                }
            link_21 = FoundLink { targetNode = "n1",
                                  linkState = LinkToTarget,
                                  linkAttributes = ()
                                }
            nbs1 :: FoundNode Text () ()
            nbs1 = FoundNode { subjectNode = "n1",
                               foundAt = fromS "2018-12-01T10:00",
                               neighborLinks = return link_12,
                               nodeAttributes = ()
                             }
            nbs2 = FoundNode { subjectNode = "n2",
                               foundAt = fromS "2018-12-01T20:00",
                               neighborLinks = return link_21,
                               nodeAttributes = ()
                             }
        in [nbs1, nbs2],
      caseQuery = defQuery ["n1"],
      caseAssert = mutual_neighbor_assert
    },
    SnapshotTestCase
    { caseName = "multiple findings for a single node",
      caseInput =
        let fns :: [FoundNode Text AText ()]
            fns = [ FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T20:00",
                      neighborLinks = [ FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkToTarget,
                                          linkAttributes = ()
                                        },
                                        FoundLink
                                        { targetNode = "n3",
                                          linkState = LinkToSubject,
                                          linkAttributes = ()
                                        }
                                      ],
                      nodeAttributes = AText "at 20:00"
                    },
                    FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T10:00",
                      neighborLinks = mempty,
                      nodeAttributes = AText "at 10:00"
                    },
                    FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T15:00",
                      neighborLinks = return $ FoundLink
                                      { targetNode = "n2",
                                        linkState = LinkToTarget,
                                        linkAttributes = ()
                                      },
                      nodeAttributes = AText "at 15:00"
                    }
                  ]
        in fns,
      caseQuery = defQuery ["n1"],
      caseAssert = multi_findings_single_node_assert
    },
    SnapshotTestCase
    { caseName = "multi-hop neighbors",
      caseInput = 
        let fns :: [FoundNode Text () AText]
            fns = [ FoundNode
                    { subjectNode = intToNodeId 1,
                      foundAt = fromS "2018-12-01T10:00",
                      neighborLinks = return $ FoundLink
                                      { targetNode = intToNodeId 2,
                                        linkState = LinkToTarget,
                                        linkAttributes = AText "first"
                                      },
                      nodeAttributes = ()
                    },
                    middleNode 2 $ fromS "2018-12-01T05:00",
                    middleNode 3 $ fromS "2018-12-01T15:00",
                    middleNode 4 $ fromS "2018-12-01T20:00",
                    FoundNode
                    { subjectNode = intToNodeId 5,
                      foundAt = fromS "2018-12-01T15:00",
                      neighborLinks = return $ FoundLink
                                      { targetNode = intToNodeId 4,
                                        linkState = LinkToSubject,
                                        linkAttributes = AText "last"
                                      },
                      nodeAttributes = ()
                    }
                  ]
            intToNodeId :: Int -> Text
            intToNodeId i = pack ("n" ++ show i)
            middleNode node_i time =
              FoundNode
              { subjectNode = sub_nid,
                foundAt = time,
                neighborLinks = [ FoundLink
                                  { targetNode = intToNodeId (node_i - 1),
                                    linkState = LinkToSubject,
                                    linkAttributes = AText (sub_nid <> " to prev")
                                  },
                                  FoundLink
                                  { targetNode = intToNodeId (node_i + 1),
                                    linkState = LinkToTarget,
                                    linkAttributes = AText (sub_nid <> " to next")
                                  }
                                ],
                nodeAttributes = ()
              }
              where
                sub_nid = intToNodeId node_i
        in fns,
      caseQuery = defQuery ["n1"],
      caseAssert = multi_hop_neighbors_assert
    },
    SnapshotTestCase
    { caseName = "loop network",
      caseInput =
        let fns :: [FoundNode Text () ()]
            fns = [ FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T10:00",
                      neighborLinks = [ FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkToTarget,
                                          linkAttributes = ()
                                        }
                                      ],
                      nodeAttributes = ()
                    },
                    FoundNode
                    { subjectNode = "n2",
                      foundAt = fromS "2018-12-01T15:00",
                      neighborLinks = [ FoundLink
                                        { targetNode = "n1",
                                          linkState = LinkToSubject,
                                          linkAttributes = ()
                                        },
                                        FoundLink
                                        { targetNode = "n3",
                                          linkState = LinkBidirectional,
                                          linkAttributes = ()
                                        }
                                      ],
                      nodeAttributes = ()
                    },
                    FoundNode
                    { subjectNode = "n3",
                      foundAt = fromS "2018-12-01T10:00",
                      neighborLinks = [ FoundLink
                                        { targetNode = "n1",
                                          linkState = LinkToTarget,
                                          linkAttributes = ()
                                        },
                                        FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkBidirectional,
                                          linkAttributes = ()
                                        }
                                      ],
                      nodeAttributes = ()
                    }
                  ]
        in fns,
      caseQuery = defQuery ["n1"],
      caseAssert = loop_network_assert
    },
    SnapshotTestCase
    { caseName = "multiple links between two nodes",
      caseInput = 
        let fns :: [FoundNode Text () APorts]
            fns = [ FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T20:00",
                      nodeAttributes = (),
                      neighborLinks = [ FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkToTarget,
                                          linkAttributes = APorts "p4" "p8"
                                        },
                                        FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkToTarget,
                                          linkAttributes = APorts "p3" "p6"
                                        },
                                        FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkToTarget,
                                          linkAttributes = APorts "p5" "p10"
                                        }
                                      ]
                    },
                    FoundNode
                    { subjectNode = "n2",
                      foundAt = fromS "2018-12-01T10:00",
                      nodeAttributes = (),
                      neighborLinks = [ FoundLink
                                        { targetNode = "n1",
                                          linkState = LinkToSubject,
                                          linkAttributes = APorts "p6" "p3"
                                        },
                                        FoundLink
                                        { targetNode = "n1",
                                          linkState = LinkToSubject,
                                          linkAttributes = APorts "p10" "p5"
                                        },
                                        FoundLink
                                        { targetNode = "n1",
                                          linkState = LinkToSubject,
                                          linkAttributes = APorts "p8" "p4"
                                        }
                                      ]
                    }
                  ]
        in fns,
      caseQuery = queryWithAPorts "n1",
      caseAssert = multiple_links_assert
    },
    SnapshotTestCase
    { caseName = "link disappears",
      caseInput = 
        let fns :: [FoundNode Text () ()]
            fns = [ FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T10:00",
                      nodeAttributes = (),
                      neighborLinks = [ FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkBidirectional,
                                          linkAttributes = ()
                                        }
                                      ]
                    },
                    FoundNode
                    { subjectNode = "n2",
                      foundAt = fromS "2018-12-01T20:00",
                      nodeAttributes = (),
                      neighborLinks = []
                    }
                  ]
        in fns,
      caseQuery = defQuery ["n1"],
      caseAssert = \got_graph -> do
        let (got_ns, got_ls) = sortSnapshotElements got_graph
        nodeId (got_ns ! 0) `shouldBe` "n1"
        nodeId (got_ns ! 1) `shouldBe` "n2"
        V.length got_ns `shouldBe` 2
        V.length got_ls `shouldBe` 0
        -- the n2 observes at t=200 that there is no link to n1. So the
        -- Spider should consider the link disappears.
    },
    SnapshotTestCase
    { caseName = "link appears",
      caseInput = 
        let fns :: [FoundNode Text () ()]
            fns = [ FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T20:00",
                      nodeAttributes = (),
                      neighborLinks = [ FoundLink
                                        { targetNode = "n2",
                                          linkState = LinkBidirectional,
                                          linkAttributes = ()
                                        }
                                      ]
                    },
                    FoundNode
                    { subjectNode = "n2",
                      foundAt = fromS "2018-12-01T10:00",
                      nodeAttributes = (),
                      neighborLinks = []
                    }
                  ]
        in fns,
      caseQuery = defQuery ["n1"],
      caseAssert = \got_graph -> do
        let (got_ns, got_ls) = sortSnapshotElements got_graph
        nodeId (got_ns ! 0) `shouldBe` "n1"
        nodeId (got_ns ! 1) `shouldBe` "n2"
        V.length got_ns `shouldBe` 2
        linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
        isDirected (got_ls ! 0) `shouldBe` False
        linkTimestamp (got_ls ! 0) `shouldBe` fromS "2018-12-01T20:00"
        V.length got_ls `shouldBe` 1
        -- the n2 observes at t=100 that there is no link to n1, but n1
        -- observes there is a link at t=200.  Spider should consider the
        -- link appears.
    },
    SnapshotTestCase
    { caseName = "multiple links between pair, some appear, some disappear.",
      caseInput = 
        let fns :: [FoundNode Text () APorts]
            fns = [ FoundNode
                    { subjectNode = "n2",
                      foundAt = fromS "2018-12-01T20:00",
                      nodeAttributes = (),
                      neighborLinks = links2
                    },
                    FoundNode
                    { subjectNode = "n1",
                      foundAt = fromS "2018-12-01T10:00",
                      nodeAttributes = (),
                      neighborLinks = links1
                    }
                  ]
            links1 = [ FoundLink -- disappears
                       { targetNode = "n2",
                         linkState = LinkToTarget,
                         linkAttributes = APorts "p11" "p21"
                       },
                       FoundLink -- stays
                       { targetNode = "n2",
                         linkState = LinkToTarget,
                         linkAttributes = APorts "p12" "p22"
                       }
                     ]
            links2 = [ FoundLink -- appears
                       { targetNode = "n1",
                         linkState = LinkToSubject,
                         linkAttributes = APorts "p23" "p13"
                       },
                       FoundLink -- stays
                       { targetNode = "n1",
                         linkState = LinkToSubject,
                         linkAttributes = APorts "p22" "p12" 
                       }
                     ]
        in fns,
      caseQuery = queryWithAPorts "n1",
      caseAssert = \got_graph -> do
        let (got_ns, got_ls_raw) = sortSnapshotElements got_graph
        nodeId (got_ns ! 0) `shouldBe` "n1"
        nodeTimestamp (got_ns ! 0) `shouldBe` (Just $ fromS "2018-12-01T10:00")
        nodeId (got_ns ! 1) `shouldBe` "n2"
        nodeTimestamp (got_ns ! 1) `shouldBe` (Just $ fromS "2018-12-01T20:00")
        V.length got_ns `shouldBe` 2
        let got_ls = sortLinksWithAttr got_ls_raw
        linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
        S.linkAttributes (got_ls ! 0) `shouldBe` APorts "p12" "p22"
        linkTimestamp (got_ls ! 0) `shouldBe` fromS "2018-12-01T20:00"
        linkNodeTuple (got_ls ! 1) `shouldBe` ("n1", "n2") -- TODO: looks like this link is removed. why?
        S.linkAttributes (got_ls ! 1) `shouldBe` APorts "p13" "p23"
        linkTimestamp (got_ls ! 1) `shouldBe` fromS "2018-12-01T20:00"
        V.length got_ls `shouldBe` 2
    }

-- TODO: how linkState relates to the property of SnapshotLink ?
-- especially Bidirectional links? -> important thing is that if the
-- link is Bidirectional, (source, destination) of snapshot link is
-- always (subject, target).

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
    mutual_neighbor_assert (got_ns, got_ls) = do
      let (got_n1, got_n2, got_l) = case (sort got_ns, sort got_ls) of
            ([a,b], [c]) -> (a,b,c)
            _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
      nodeId got_n1 `shouldBe` "n1"
      isOnBoundary got_n1 `shouldBe` False
      nodeTimestamp got_n1 `shouldBe` Just (fromS "2018-12-01T10:00")
      S.nodeAttributes got_n1 `shouldBe` Just ()
      nodeId got_n2 `shouldBe` "n2"
      isOnBoundary got_n2 `shouldBe` False
      nodeTimestamp got_n2 `shouldBe` Just (fromS "2018-12-01T20:00")
      S.nodeAttributes got_n2 `shouldBe` Just ()
      linkNodeTuple got_l `shouldBe` ("n2", "n1")
      isDirected got_l `shouldBe` True
      linkTimestamp got_l `shouldBe` fromS "2018-12-01T20:00"
      S.linkAttributes got_l `shouldBe` ()
    multi_findings_single_node_assert (got_ns, got_ls) = do
      let (got_n1, got_n2, got_n3, got_l12, got_l31) = case (sort got_ns, sort got_ls) of
            ([g1, g2, g3], [g4, g5]) -> (g1, g2, g3, g4, g5)
            _ -> error ("Unexpected patter: got = " ++ show (got_ns, got_ls))
      nodeId got_n1 `shouldBe` "n1"
      isOnBoundary got_n1 `shouldBe` False
      nodeTimestamp got_n1 `shouldBe` Just (fromS "2018-12-01T20:00")
      S.nodeAttributes got_n1 `shouldBe` Just (AText "at 20:00")
      nodeId got_n2 `shouldBe` "n2"
      isOnBoundary got_n2 `shouldBe` False
      nodeTimestamp got_n2 `shouldBe` Nothing
      S.nodeAttributes got_n2 `shouldBe` Nothing
      nodeId got_n3 `shouldBe` "n3"
      isOnBoundary got_n3 `shouldBe` False
      nodeTimestamp got_n3 `shouldBe` Nothing
      S.nodeAttributes got_n3 `shouldBe` Nothing
      linkNodeTuple got_l12 `shouldBe` ("n1", "n2")
      isDirected got_l12 `shouldBe` True
      linkTimestamp got_l12 `shouldBe` fromS "2018-12-01T20:00"
      linkNodeTuple got_l31 `shouldBe` ("n3", "n1")
      isDirected got_l31 `shouldBe` True
      linkTimestamp got_l31 `shouldBe` fromS "2018-12-01T20:00"
    multi_hop_neighbors_assert got_graph = do
      let (got_ns, got_ls) = sortSnapshotElements got_graph
      nodeId (got_ns ! 0) `shouldBe` "n1"
      isOnBoundary (got_ns ! 0) `shouldBe` False
      nodeTimestamp (got_ns ! 0) `shouldBe` Just (fromS "2018-12-01T10:00")
      S.nodeAttributes (got_ns ! 0) `shouldBe` Just ()
      nodeId (got_ns ! 1) `shouldBe` "n2"
      isOnBoundary (got_ns ! 1) `shouldBe` False
      nodeTimestamp (got_ns ! 1) `shouldBe` Just (fromS "2018-12-01T05:00")
      S.nodeAttributes (got_ns ! 1) `shouldBe` Just ()
      nodeId (got_ns ! 2) `shouldBe` "n3"
      isOnBoundary (got_ns ! 2) `shouldBe` False
      nodeTimestamp (got_ns ! 2) `shouldBe` Just (fromS "2018-12-01T15:00")
      S.nodeAttributes (got_ns ! 2) `shouldBe` Just ()
      nodeId (got_ns ! 3) `shouldBe` "n4"
      isOnBoundary (got_ns ! 3) `shouldBe` False
      nodeTimestamp (got_ns ! 3) `shouldBe` Just (fromS "2018-12-01T20:00")
      S.nodeAttributes (got_ns ! 3) `shouldBe` Just ()
      nodeId (got_ns ! 4) `shouldBe` "n5"
      isOnBoundary (got_ns ! 4) `shouldBe` False
      nodeTimestamp (got_ns ! 4) `shouldBe` Just (fromS "2018-12-01T15:00")
      S.nodeAttributes (got_ns ! 4) `shouldBe` Just ()
      V.length got_ns `shouldBe` 5
      linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
      isDirected (got_ls ! 0) `shouldBe` True
      linkTimestamp (got_ls ! 0) `shouldBe` fromS "2018-12-01T10:00"
      S.linkAttributes (got_ls ! 0) `shouldBe` AText "first"
      linkNodeTuple (got_ls ! 1) `shouldBe` ("n2", "n3")
      isDirected (got_ls ! 1) `shouldBe` True
      linkTimestamp (got_ls ! 1) `shouldBe` fromS "2018-12-01T15:00"
      S.linkAttributes (got_ls ! 1) `shouldBe` AText "n3 to prev"
      linkNodeTuple (got_ls ! 2) `shouldBe` ("n3", "n4")
      isDirected (got_ls ! 2) `shouldBe` True
      linkTimestamp (got_ls ! 2) `shouldBe` fromS "2018-12-01T20:00"
      S.linkAttributes (got_ls ! 2) `shouldBe` AText "n4 to prev"
      linkNodeTuple (got_ls ! 3) `shouldBe` ("n4", "n5")
      isDirected (got_ls ! 3) `shouldBe` True
      linkTimestamp (got_ls ! 3) `shouldBe` fromS "2018-12-01T20:00"
      S.linkAttributes (got_ls ! 3) `shouldBe` AText "n4 to next"
      V.length got_ls `shouldBe` 4
    loop_network_assert got_graph = do
      let (got_ns, got_ls) = sortSnapshotElements got_graph
      nodeId (got_ns ! 0) `shouldBe` "n1"
      isOnBoundary (got_ns ! 0) `shouldBe` False
      nodeTimestamp (got_ns ! 0) `shouldBe` Just (fromS "2018-12-01T10:00")
      nodeId (got_ns ! 1) `shouldBe` "n2"
      isOnBoundary (got_ns ! 1) `shouldBe` False
      nodeTimestamp (got_ns ! 1) `shouldBe` Just (fromS "2018-12-01T15:00")
      nodeId (got_ns ! 2) `shouldBe` "n3"
      isOnBoundary (got_ns ! 2) `shouldBe` False
      nodeTimestamp (got_ns ! 2) `shouldBe` Just (fromS "2018-12-01T10:00")
      V.length got_ns `shouldBe` 3
      linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
      isDirected (got_ls ! 0) `shouldBe` True
      linkTimestamp (got_ls ! 0) `shouldBe` fromS "2018-12-01T15:00"
      linkNodeTuple (got_ls ! 1) `shouldBe` ("n2", "n3")
      isDirected (got_ls ! 1) `shouldBe` False
      linkTimestamp (got_ls ! 1) `shouldBe` fromS "2018-12-01T15:00"
      linkNodeTuple (got_ls ! 2) `shouldBe` ("n3", "n1")
      isDirected (got_ls ! 2) `shouldBe` True
      linkTimestamp (got_ls ! 2) `shouldBe` fromS "2018-12-01T10:00"
    multiple_links_assert got_graph = do
      let (got_ns, got_ls_raw) = sortSnapshotElements got_graph
      nodeId (got_ns ! 0) `shouldBe` "n1"
      nodeId (got_ns ! 1) `shouldBe` "n2"
      V.length got_ns `shouldBe` 2
      let got_ls = sortLinksWithAttr got_ls_raw
      linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
      S.linkAttributes (got_ls ! 0) `shouldBe` APorts "p3" "p6"
      linkTimestamp (got_ls ! 0) `shouldBe` fromS "2018-12-01T20:00"
      linkNodeTuple (got_ls ! 1) `shouldBe` ("n1", "n2")
      S.linkAttributes (got_ls ! 1) `shouldBe` APorts "p4" "p8"
      linkTimestamp (got_ls ! 1) `shouldBe` fromS "2018-12-01T20:00"
      linkNodeTuple (got_ls ! 2) `shouldBe` ("n1", "n2")
      S.linkAttributes (got_ls ! 2) `shouldBe` APorts "p5" "p10"
      linkTimestamp (got_ls ! 2) `shouldBe` fromS "2018-12-01T20:00"
      V.length got_ls `shouldBe` 3

      

