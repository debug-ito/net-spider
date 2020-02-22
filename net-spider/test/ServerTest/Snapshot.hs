{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Snapshot (main, spec) where

import Control.Exception.Safe (withException)
import Control.Monad (mapM_)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, sort)
import Data.Maybe (isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket as Gr
import qualified Network.Greskell.WebSocket.Response as Res
import System.IO (stderr)
import Test.Hspec

import TestCommon
  ( withServer, withSpider, sortSnapshotElements,
    AText(..), APorts(..), subIdWithAPorts,
    alignAPortsToLinkDirection
  )
import SnapshotTestCase (SnapshotTestCase(..), snapshotTestCases)

import NetSpider.Found
  ( FoundLink(..), LinkState(..), FoundNode(..)
  )
import NetSpider.Query
  ( Query, defQuery, startsFrom, unifyLinkSamples, timeInterval, foundNodePolicy,
    Extended(..), (<..<), (<..<=), (<=..<=), policyOverwrite, policyAppend
  )
import NetSpider.Snapshot
  ( SnapshotLink,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary, nodeTimestamp
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Spider
  ( Spider, addFoundNode, getSnapshotSimple, getSnapshot
  )
import NetSpider.Spider.Config (defConfig, Config, Host, Port)
import NetSpider.Unify
  ( unifyStd, UnifyStdConfig(..), defUnifyStdConfig,
    lsLinkAttributes, lsSubjectNode,
    latestLinkSample
  )
import NetSpider.Timestamp (fromS, fromEpochMillisecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_getSnapshot

makeOneNeighborExample :: Spider Text () () -> IO ()
makeOneNeighborExample spider = do
  let link = FoundLink { targetNode = "n2",
                         linkState = LinkToTarget,
                         linkAttributes = ()
                       }
      nbs = FoundNode { subjectNode = "n1",
                        foundAt = fromS "2018-12-01T10:00",
                        neighborLinks = return link,
                        nodeAttributes = ()
                      }
  debugShowE $ addFoundNode spider nbs

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

spec_getSnapshot :: Spec
spec_getSnapshot = withServer $ describe "getSnapshotSimple, getSnapshot" $ do
  spec_getSnapshot1
  spec_getSnapshot2
  spec_getSnapshot_timeInterval
  spec_getSnapshot_foundNodePolicy

makeTestCase :: SnapshotTestCase -> SpecWith (Host, Port)
makeTestCase SnapshotTestCase { caseName = name, caseInput = input, caseQuery = query, caseAssert = assert } = do
  specify name $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input
    got_graph <- getSnapshot spider query
    assert got_graph

spec_getSnapshot1 :: SpecWith (Host,Port)
spec_getSnapshot1 = do
  mapM_ makeTestCase snapshotTestCases
  specify "missing starting node" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    (got_ns, got_ls) <- getSnapshotSimple spider "no node"
    got_ns `shouldBe` mempty
    got_ls `shouldBe` mempty
  specify "mutual neighbors" $ withSpider $ \spider -> do
    let link_12 :: FoundLink Text ()
        link_12 = FoundLink { targetNode = "n2",
                              linkState = LinkToSubject,
                              linkAttributes = ()
                            }
        link_21 = FoundLink { targetNode = "n1",
                              linkState = LinkToTarget,
                              linkAttributes = ()
                            }
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
    mapM_ (addFoundNode spider) [nbs1, nbs2]
    (got_ns, got_ls) <- getSnapshotSimple spider "n1"
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
  specify "multiple findings for a single node" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- getSnapshotSimple spider "n1"
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
  specify "multi-hop neighbors" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getSnapshotSimple spider "n1"
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

spec_getSnapshot2 :: SpecWith (Host, Port)
spec_getSnapshot2 = do
  specify "loop network" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getSnapshotSimple spider "n1"
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
  specify "multiple links between two nodes" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls_raw) <- fmap sortSnapshotElements $ getSnapshot spider $ queryWithAPorts "n1"
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
  specify "link disappears" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getSnapshotSimple spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    nodeId (got_ns ! 1) `shouldBe` "n2"
    V.length got_ns `shouldBe` 2
    V.length got_ls `shouldBe` 0
    -- the n2 observes at t=200 that there is no link to n1. So the
    -- Spider should consider the link disappears.
  specify "link appears" $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getSnapshotSimple spider "n1"
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
  specify "multiple links between pair, some appear, some disappear." $ withSpider $ \spider -> do
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
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls_raw) <- fmap sortSnapshotElements $ getSnapshot spider $ queryWithAPorts "n1"
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


-- TODO: how linkState relates to the property of SnapshotLink ?
-- especially Bidirectional links? -> important thing is that if the
-- link is Bidirectional, (source, destination) of snapshot link is
-- always (subject, target).


debugShowE :: IO a -> IO a
debugShowE act = withException act showSubmitException

showSubmitException :: Gr.SubmitException -> IO ()
showSubmitException (Gr.ResponseError res) = showResponse res
showSubmitException (Gr.ParseError res _) = showResponse res

showResponse :: Res.ResponseMessage a -> IO ()
showResponse res = showResponseStatus $ Res.status res

showResponseStatus :: Res.ResponseStatus -> IO ()
showResponseStatus status = mapM_ showKeyValue $ ("message", Res.message status) : textAttributes
  where
    textAttributes = justValue =<< (HM.toList $ fmap vToText $ Res.attributes status)
    vToText (String s) = Just s
    vToText _ = Nothing
    justValue (k, Just v) = [(k, v)]
    justValue (_, Nothing) = []
    showKeyValue (key, val) = TIO.hPutStrLn stderr (key <> ": " <> val)

sort2 :: (Ord a, Ord b) => ([a], [b]) -> ([a], [b])
sort2 (a, b) = (sort a, sort b)

spec_getSnapshot_timeInterval :: SpecWith (Host,Port)
spec_getSnapshot_timeInterval = do
  let linkTo n = FoundLink
                 { targetNode = n,
                   linkState = LinkToTarget,
                   linkAttributes = ()
                 }
      linksTo ns = map linkTo ns
      node n t ls = FoundNode
                    { subjectNode = n,
                      foundAt = fromS ("2018-12-01T01:" <> t),
                      neighborLinks = ls,
                      nodeAttributes = ()
                    }
      input_fns :: [FoundNode Text () ()]
      input_fns = [ node "n1" "10" $ linksTo ["n2"],
                    node "n1" "20" $ linksTo ["n2", "n3"],
                    node "n1" "30" $ linksTo [],
                    node "n1" "40" $ linksTo ["n3"],
                    node "n2" "15" $ linksTo [],
                    node "n2" "25" $ linksTo ["n4"],
                    node "n2" "35" $ linksTo ["n4", "n3", "n5"],
                    node "n3" "10" $ linksTo ["n4", "n2"],
                    node "n3" "30" $ linksTo ["n4"],
                    node "n4" "05" $ linksTo [],
                    node "n4" "15" $ linksTo ["n1"],
                    node "n4" "25" $ linksTo ["n1", "n5"],
                    node "n4" "35" $ linksTo []
                  ]
      simple_unifier = unifyStd $ defUnifyStdConfig { negatesLinkSample = \_ _ -> False }
                       -- Disable negation to simplify the test.
  specify "only lower bound" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let q = (defQuery ["n1", "n2"]) { unifyLinkSamples = simple_unifier,
                                      timeInterval = (Finite $ fromS "2018-12-01T01:30") <..< PosInf
                                    }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider q
    map nodeId got_nodes `shouldBe` ["n1", "n2", "n3", "n4", "n5"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [False, False, True, False, True]
    map linkNodeTuple got_edges `shouldBe`
      [ ("n1", "n3"),
        ("n2", "n3"),
        ("n2", "n4"),
        ("n2", "n5")
      ]
    map linkTimestamp got_edges `shouldBe`
      map (\m -> fromS ("2018-12-01T01:" <> m)) ["40", "35", "35", "35"]
  specify "only upper bound (exclusive)" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let q = (defQuery ["n1"]) { unifyLinkSamples = simple_unifier,
                                timeInterval = NegInf
                                               <..< (Finite $ fromS "2018-12-01T01:30")
                              }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider q
    map nodeId got_nodes `shouldBe` ["n1", "n2", "n3", "n4", "n5"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [False, False, False, False, True]
    map linkNodeTuple got_edges `shouldBe`
      [ ("n1", "n2"),
        ("n1", "n3"),
        ("n2", "n4"),
        ("n3", "n2"),
        ("n3", "n4"),
        ("n4", "n1"),
        ("n4", "n5")
      ]
    map linkTimestamp got_edges `shouldBe`
      map (\m -> fromS ("2018-12-01T01:" <> m)) ["20", "20", "25", "10", "10", "25", "25"]
  specify "only upper bound (inclusive)" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let q = (defQuery ["n3"]) { unifyLinkSamples = simple_unifier,
                                timeInterval = NegInf
                                               <..<= (Finite $ fromS "2018-12-01T01:30")
                              }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider q
    map nodeId got_nodes `shouldBe` ["n1", "n3", "n4", "n5"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [False, False, False, True]
    map linkNodeTuple got_edges `shouldBe`
      [ ("n3", "n4"),
        ("n4", "n1"),
        ("n4", "n5")
      ]
    map linkTimestamp got_edges `shouldBe`
      map (\m -> fromS ("2018-12-01T01:" <> m)) ["30", "25", "25"]
  specify "both bounded" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let q = (defQuery ["n2"]) { unifyLinkSamples = simple_unifier,
                                timeInterval = (Finite $ fromS "2018-12-01T01:20")
                                               <..<= (Finite $ fromS "2018-12-01T01:25")
                              }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider q
    map nodeId got_nodes `shouldBe` ["n1", "n2", "n4", "n5"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [True, False, False, True]
    map linkNodeTuple got_edges `shouldBe`
      [ ("n2", "n4"),
        ("n4", "n1"),
        ("n4", "n5")
      ]
    map linkTimestamp got_edges `shouldBe`
      map (\m -> fromS ("2018-12-01T01:" <> m)) ["25", "25", "25"]

spec_getSnapshot_foundNodePolicy :: SpecWith (Host,Port)
spec_getSnapshot_foundNodePolicy = do
  let linkTo n = FoundLink
                 { targetNode = n,
                   linkState = LinkToTarget,
                   linkAttributes = ()
                 }
      linksTo = map linkTo
      node n t ls = FoundNode
                    { subjectNode = n,
                      foundAt = fromEpochMillisecond t,
                      neighborLinks = ls,
                      nodeAttributes = ()
                    }
      input_fns :: [FoundNode Text () ()]
      input_fns = [ node "n1" 10 $ linksTo ["n2"],
                    node "n1" 20 $ linksTo ["n3"],
                    node "n1" 30 $ linksTo ["n2"],
                    node "n2" 15 $ linksTo ["n1"],
                    node "n2" 25 $ linksTo ["n4"],
                    node "n2" 35 $ linksTo ["n4", "n1"],
                    node "n3" 17 $ linksTo [],
                    node "n3" 27 $ linksTo ["n1", "n4"],
                    node "n3" 37 $ linksTo [],
                    node "n4" 8  $ linksTo ["n2"],
                    node "n4" 18 $ linksTo [],
                    node "n4" 28 $ linksTo ["n2", "n3"]
                  ]
      simple_unifier = unifyStd $ defUnifyStdConfig { makeLinkSubId = \ls -> lsSubjectNode ls,
                                                      negatesLinkSample = \_ _ -> False
                                                    }
  specify "policyOverwrite with timeInterval" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let query = (defQuery ["n1"])
                { timeInterval = NegInf <..<= (Finite $ fromEpochMillisecond 27),
                  foundNodePolicy = policyOverwrite,
                  unifyLinkSamples = simple_unifier
                }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider query
    map linkNodeTuple got_edges `shouldBe`
      [ ("n1", "n3"),
        ("n3", "n1"),
        ("n3", "n4")
      ]
    map linkTimestamp got_edges `shouldBe` map fromEpochMillisecond [20, 27, 27]
    map nodeId got_nodes `shouldBe` ["n1", "n3", "n4"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [False, False, False]
  specify "policyAppend with timeInterval" $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input_fns
    let query = (defQuery ["n1"])
                { timeInterval = (Finite $ fromEpochMillisecond 15) <=..<= (Finite $ fromEpochMillisecond 30),
                  foundNodePolicy = policyAppend,
                  unifyLinkSamples = simple_unifier
                }
    (got_nodes, got_edges) <- fmap sort2 $ getSnapshot spider query
    map linkNodeTuple got_edges `shouldBe`
      [ ("n1", "n2"),
        ("n1", "n3"),
        ("n2", "n1"),
        ("n2", "n4"),
        ("n3", "n1"),
        ("n3", "n4"),
        ("n4", "n2"),
        ("n4", "n3")
      ]
    map linkTimestamp got_edges `shouldBe`
      map fromEpochMillisecond [30, 20, 15, 25, 27, 27, 28, 28]
    map nodeId got_nodes `shouldBe` ["n1", "n2", "n3", "n4"]
    map (isNothing . S.nodeAttributes) got_nodes `shouldBe` [False, False, False, False]
