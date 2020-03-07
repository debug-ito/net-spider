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
  ( sortSnapshotElements,
    AText(..), APorts(..), subIdWithAPorts,
    alignAPortsToLinkDirection
  )
import SnapshotTestCase (SnapshotTestCase(..), snapshotTestCases)
import ServerTest.ServerCommon (withServer, withSpider)

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
  mapM_ makeTestCase snapshotTestCases
  spec_getSnapshot_timeInterval
  spec_getSnapshot_foundNodePolicy

makeTestCase :: SnapshotTestCase -> SpecWith (Host, Port)
makeTestCase SnapshotTestCase { caseName = name, caseInput = input, caseQuery = query, caseAssert = assert } = do
  specify name $ withSpider $ \spider -> do
    mapM_ (addFoundNode spider) input
    got_graph <- getSnapshot spider query
    assert got_graph

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
