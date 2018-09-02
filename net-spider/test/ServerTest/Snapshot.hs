{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Snapshot (main, spec) where

import Control.Exception.Safe (withException)
import Control.Monad (mapM_)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, sort)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket as Gr
import qualified Network.Greskell.WebSocket.Response as Res
import System.IO (stderr)
import Test.Hspec

import ServerTest.Common
  ( withServer, withSpider, withSpider', sortSnapshotElements,
    AText(..), APorts(..)
  )

import NetSpider.Found
  ( FoundLink(..), LinkState(..), FoundNode(..)
  )
import NetSpider.Snapshot
  ( SnapshotLink,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary, nodeTimestamp
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Spider
  ( Spider, addFoundNode, getLatestSnapshot
  )
import NetSpider.Spider.Config (defConfig, Config(unifyLinkSamples), Host, Port)
import NetSpider.Spider.Unify (unifyToMany, lsLinkAttributes)
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_getLatestSnapshot

makeOneNeighborExample :: Spider Text () () () -> IO ()
makeOneNeighborExample spider = do
  let link = FoundLink { targetNode = "n2",
                         linkState = LinkToTarget,
                         linkAttributes = ()
                       }
      nbs = FoundNode { subjectNode = "n1",
                        observationTime = fromEpochSecond 100,
                        neighborLinks = return link,
                        nodeAttributes = ()
                      }
  debugShowE $ addFoundNode spider nbs

confWithAPorts :: Config Text () APorts APorts
confWithAPorts = defConfig { unifyLinkSamples = unifyToMany lsLinkAttributes }

sortLinksWithAttr :: (Ord n, Ord la) => Vector (SnapshotLink n la) -> Vector (SnapshotLink n la)
sortLinksWithAttr = V.fromList . sortOn getKey . V.toList
  where
    getKey link = (linkNodeTuple link, S.linkAttributes link)

spec_getLatestSnapshot :: Spec
spec_getLatestSnapshot = withServer $ describe "getLatestSnapshot" $ do
  spec_getLatestSnapshot1
  spec_getLatestSnapshot2


spec_getLatestSnapshot1 :: SpecWith (Host,Port)
spec_getLatestSnapshot1 = do
  specify "one neighbor" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    (got_ns, got_ls) <- debugShowE $ getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_link) = case (sort got_ns, sort got_ls) of
          ([a, b], [c]) -> (a, b, c)
          _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeTimestamp got_n1 `shouldBe` Just (fromEpochSecond 100)
    S.nodeAttributes got_n1 `shouldBe` Just ()
    nodeId got_n2 `shouldBe` "n2"
    isOnBoundary got_n2 `shouldBe` False
    nodeTimestamp got_n2 `shouldBe` Nothing -- n1 is not observed.
    S.nodeAttributes got_n2 `shouldBe` Nothing -- n1 is not observed.
    linkNodeTuple got_link `shouldBe` ("n1", "n2")
    isDirected got_link `shouldBe` True
    linkTimestamp got_link `shouldBe` fromEpochSecond 100
    S.linkAttributes got_link `shouldBe` ()
  specify "no neighbor" $ withSpider $ \spider -> do
    let nbs :: FoundNode Text () ()
        nbs = FoundNode { subjectNode = "n1",
                          observationTime = fromEpochSecond 200,
                          neighborLinks = mempty,
                          nodeAttributes = ()
                        }
    addFoundNode spider nbs
    (got_ns, got_ls) <- getLatestSnapshot spider "n1"
    let got_n1 = case (sort got_ns, sort got_ls) of
          ([a], []) -> a
          _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeTimestamp got_n1 `shouldBe` Just (fromEpochSecond 200)
    S.nodeAttributes got_n1 `shouldBe` Just ()
  specify "missing starting node" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    (got_ns, got_ls) <- getLatestSnapshot spider "no node"
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
                           observationTime = fromEpochSecond 100,
                           neighborLinks = return link_12,
                           nodeAttributes = ()
                         }
        nbs2 = FoundNode { subjectNode = "n2",
                           observationTime = fromEpochSecond 200,
                           neighborLinks = return link_21,
                           nodeAttributes = ()
                         }
    mapM_ (addFoundNode spider) [nbs1, nbs2]
    (got_ns, got_ls) <- getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_l) = case (sort got_ns, sort got_ls) of
          ([a,b], [c]) -> (a,b,c)
          _ -> error ("Unexpected result: got = " ++ show (got_ns, got_ls))
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeTimestamp got_n1 `shouldBe` Just (fromEpochSecond 100)
    S.nodeAttributes got_n1 `shouldBe` Just ()
    nodeId got_n2 `shouldBe` "n2"
    isOnBoundary got_n2 `shouldBe` False
    nodeTimestamp got_n2 `shouldBe` Just (fromEpochSecond 200)
    S.nodeAttributes got_n2 `shouldBe` Just ()
    linkNodeTuple got_l `shouldBe` ("n2", "n1")
    isDirected got_l `shouldBe` True
    linkTimestamp got_l `shouldBe` fromEpochSecond 200
    S.linkAttributes got_l `shouldBe` ()
  specify "multiple findings for a single node" $ withSpider $ \spider -> do
    let fns :: [FoundNode Text AText ()]
        fns = [ FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 200,
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
                  nodeAttributes = AText "at 200"
                },
                FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 100,
                  neighborLinks = mempty,
                  nodeAttributes = AText "at 100"
                },
                FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 150,
                  neighborLinks = return $ FoundLink
                                  { targetNode = "n2",
                                    linkState = LinkToTarget,
                                    linkAttributes = ()
                                  },
                  nodeAttributes = AText "at 150"
                }
              ]
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_n3, got_l12, got_l31) = case (sort got_ns, sort got_ls) of
          ([g1, g2, g3], [g4, g5]) -> (g1, g2, g3, g4, g5)
          _ -> error ("Unexpected patter: got = " ++ show (got_ns, got_ls))
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeTimestamp got_n1 `shouldBe` Just (fromEpochSecond 200)
    S.nodeAttributes got_n1 `shouldBe` Just (AText "at 200")
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
    linkTimestamp got_l12 `shouldBe` fromEpochSecond 200
    linkNodeTuple got_l31 `shouldBe` ("n3", "n1")
    isDirected got_l31 `shouldBe` True
    linkTimestamp got_l31 `shouldBe` fromEpochSecond 200
  specify "multi-hop neighbors" $ withSpider $ \spider -> do
    let fns :: [FoundNode Text () AText]
        fns = [ FoundNode
                { subjectNode = intToNodeId 1,
                  observationTime = fromEpochSecond 100,
                  neighborLinks = return $ FoundLink
                                  { targetNode = intToNodeId 2,
                                    linkState = LinkToTarget,
                                    linkAttributes = AText "first"
                                  },
                  nodeAttributes = ()
                },
                middleNode 2 $ fromEpochSecond 50,
                middleNode 3 $ fromEpochSecond 150,
                middleNode 4 $ fromEpochSecond 200,
                FoundNode
                { subjectNode = intToNodeId 5,
                  observationTime = fromEpochSecond 150,
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
            observationTime = time,
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
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    isOnBoundary (got_ns ! 0) `shouldBe` False
    nodeTimestamp (got_ns ! 0) `shouldBe` Just (fromEpochSecond 100)
    S.nodeAttributes (got_ns ! 0) `shouldBe` Just ()
    nodeId (got_ns ! 1) `shouldBe` "n2"
    isOnBoundary (got_ns ! 1) `shouldBe` False
    nodeTimestamp (got_ns ! 1) `shouldBe` Just (fromEpochSecond 50)
    S.nodeAttributes (got_ns ! 1) `shouldBe` Just ()
    nodeId (got_ns ! 2) `shouldBe` "n3"
    isOnBoundary (got_ns ! 2) `shouldBe` False
    nodeTimestamp (got_ns ! 2) `shouldBe` Just (fromEpochSecond 150)
    S.nodeAttributes (got_ns ! 2) `shouldBe` Just ()
    nodeId (got_ns ! 3) `shouldBe` "n4"
    isOnBoundary (got_ns ! 3) `shouldBe` False
    nodeTimestamp (got_ns ! 3) `shouldBe` Just (fromEpochSecond 200)
    S.nodeAttributes (got_ns ! 3) `shouldBe` Just ()
    nodeId (got_ns ! 4) `shouldBe` "n5"
    isOnBoundary (got_ns ! 4) `shouldBe` False
    nodeTimestamp (got_ns ! 4) `shouldBe` Just (fromEpochSecond 150)
    S.nodeAttributes (got_ns ! 4) `shouldBe` Just ()
    V.length got_ns `shouldBe` 5
    linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
    isDirected (got_ls ! 0) `shouldBe` True
    linkTimestamp (got_ls ! 0) `shouldBe` fromEpochSecond 100
    S.linkAttributes (got_ls ! 0) `shouldBe` AText "first"
    linkNodeTuple (got_ls ! 1) `shouldBe` ("n2", "n3")
    isDirected (got_ls ! 1) `shouldBe` True
    linkTimestamp (got_ls ! 1) `shouldBe` fromEpochSecond 150
    S.linkAttributes (got_ls ! 1) `shouldBe` AText "n3 to prev"
    linkNodeTuple (got_ls ! 2) `shouldBe` ("n3", "n4")
    isDirected (got_ls ! 2) `shouldBe` True
    linkTimestamp (got_ls ! 2) `shouldBe` fromEpochSecond 200
    S.linkAttributes (got_ls ! 2) `shouldBe` AText "n4 to prev"
    linkNodeTuple (got_ls ! 3) `shouldBe` ("n4", "n5")
    isDirected (got_ls ! 3) `shouldBe` True
    linkTimestamp (got_ls ! 3) `shouldBe` fromEpochSecond 200
    S.linkAttributes (got_ls ! 3) `shouldBe` AText "n4 to next"
    V.length got_ls `shouldBe` 4

spec_getLatestSnapshot2 :: SpecWith (Host, Port)
spec_getLatestSnapshot2 = do
  specify "loop network" $ withSpider $ \spider -> do
    let fns :: [FoundNode Text () ()]
        fns = [ FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 100,
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
                  observationTime = fromEpochSecond 150,
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
                  observationTime = fromEpochSecond 100,
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
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    isOnBoundary (got_ns ! 0) `shouldBe` False
    nodeTimestamp (got_ns ! 0) `shouldBe` Just (fromEpochSecond 100)
    nodeId (got_ns ! 1) `shouldBe` "n2"
    isOnBoundary (got_ns ! 1) `shouldBe` False
    nodeTimestamp (got_ns ! 1) `shouldBe` Just (fromEpochSecond 150)
    nodeId (got_ns ! 2) `shouldBe` "n3"
    isOnBoundary (got_ns ! 2) `shouldBe` False
    nodeTimestamp (got_ns ! 2) `shouldBe` Just (fromEpochSecond 100)
    V.length got_ns `shouldBe` 3
    linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
    isDirected (got_ls ! 0) `shouldBe` True
    linkTimestamp (got_ls ! 0) `shouldBe` fromEpochSecond 150
    linkNodeTuple (got_ls ! 1) `shouldBe` ("n2", "n3")
    isDirected (got_ls ! 1) `shouldBe` False
    linkTimestamp (got_ls ! 1) `shouldBe` fromEpochSecond 150
    linkNodeTuple (got_ls ! 2) `shouldBe` ("n3", "n1")
    isDirected (got_ls ! 2) `shouldBe` True
    linkTimestamp (got_ls ! 2) `shouldBe` fromEpochSecond 100
  specify "multiple links between two nodes" $ withSpider' confWithAPorts $ \spider -> do
    let fns :: [FoundNode Text () APorts]
        fns = [ FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 200,
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
                  observationTime = fromEpochSecond 100,
                  nodeAttributes = (),
                  neighborLinks = [ FoundLink
                                    { targetNode = "n1",
                                      linkState = LinkToSubject,
                                      linkAttributes = APorts "p3" "p6"
                                    },
                                    FoundLink
                                    { targetNode = "n1",
                                      linkState = LinkToSubject,
                                      linkAttributes = APorts "p5" "p10"
                                    },
                                    FoundLink
                                    { targetNode = "n1",
                                      linkState = LinkToSubject,
                                      linkAttributes = APorts "p4" "p8"
                                    }
                                  ]
                }
              ]
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls_raw) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    nodeId (got_ns ! 1) `shouldBe` "n2"
    V.length got_ns `shouldBe` 2
    let got_ls = sortLinksWithAttr got_ls_raw
    linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
    S.linkAttributes (got_ls ! 0) `shouldBe` APorts "p3" "p6"
    linkTimestamp (got_ls ! 0) `shouldBe` fromEpochSecond 200
    linkNodeTuple (got_ls ! 1) `shouldBe` ("n1", "n2")
    S.linkAttributes (got_ls ! 1) `shouldBe` APorts "p4" "p8"
    linkTimestamp (got_ls ! 1) `shouldBe` fromEpochSecond 200
    linkNodeTuple (got_ls ! 2) `shouldBe` ("n1", "n2")
    S.linkAttributes (got_ls ! 2) `shouldBe` APorts "p5" "p10"
    linkTimestamp (got_ls ! 2) `shouldBe` fromEpochSecond 200
    V.length got_ls `shouldBe` 3
  specify "link disappears" $ withSpider $ \spider -> do
    let fns :: [FoundNode Text () ()]
        fns = [ FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 100,
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
                  observationTime = fromEpochSecond 200,
                  nodeAttributes = (),
                  neighborLinks = []
                }
              ]
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
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
                  observationTime = fromEpochSecond 200,
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
                  observationTime = fromEpochSecond 100,
                  nodeAttributes = (),
                  neighborLinks = []
                }
              ]
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    nodeId (got_ns ! 1) `shouldBe` "n2"
    V.length got_ns `shouldBe` 2
    linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
    isDirected (got_ls ! 0) `shouldBe` False
    linkTimestamp (got_ls ! 0) `shouldBe` fromEpochSecond 200
    V.length got_ls `shouldBe` 1
    -- the n2 observes at t=100 that there is no link to n1, but n1
    -- observes there is a link at t=200.  Spider should consider the
    -- link appears.
  specify "multiple links between pair, some appear, some disappear." $ withSpider' confWithAPorts $ \spider -> do
    let fns :: [FoundNode Text () APorts]
        fns = [ FoundNode
                { subjectNode = "n2",
                  observationTime = fromEpochSecond 200,
                  nodeAttributes = (),
                  neighborLinks = links2
                },
                FoundNode
                { subjectNode = "n1",
                  observationTime = fromEpochSecond 100,
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
                     linkAttributes = APorts "p13" "p23"
                   },
                   FoundLink -- stays
                   { targetNode = "n1",
                     linkState = LinkToSubject,
                     linkAttributes = APorts "p12" "p22"
                   }
                 ]
    mapM_ (addFoundNode spider) fns
    (got_ns, got_ls_raw) <- fmap sortSnapshotElements $ getLatestSnapshot spider "n1"
    nodeId (got_ns ! 0) `shouldBe` "n1"
    nodeTimestamp (got_ns ! 0) `shouldBe` (Just $ fromEpochSecond 100)
    nodeId (got_ns ! 1) `shouldBe` "n2"
    nodeTimestamp (got_ns ! 1) `shouldBe` (Just $ fromEpochSecond 200)
    V.length got_ns `shouldBe` 2
    let got_ls = sortLinksWithAttr got_ls_raw
    linkNodeTuple (got_ls ! 0) `shouldBe` ("n1", "n2")
    S.linkAttributes (got_ls ! 0) `shouldBe` APorts "p12" "p22"
    linkTimestamp (got_ls ! 0) `shouldBe` fromEpochSecond 200
    linkNodeTuple (got_ls ! 1) `shouldBe` ("n1", "n2") -- TODO: looks like this link is removed. why?
    S.linkAttributes (got_ls ! 1) `shouldBe` APorts "p13" "p23"
    linkTimestamp (got_ls ! 1) `shouldBe` fromEpochSecond 200
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
