{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket, withException)
import Control.Monad (mapM_)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket as Gr
import qualified Network.Greskell.WebSocket.Response as Res
import System.IO (stderr)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Found
  ( FoundLink(..), LinkState(..), FoundNode(..)
  )
import NetSpider.Snapshot
  ( SnapshotElement,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary, nodeTimestamp
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Spider
  ( Host, Port, Spider,
    connectWS, close, clearAll, addFoundNode, getLatestSnapshot
  )
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_getLatestSnapshot

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: (Spider n la na -> IO ()) -> (Host, Port) -> IO ()
withSpider action (host, port) = bracket (connectWS host port) close $ \spider -> do
  clearAll spider
  action spider

makeOneNeighborExample :: Spider Text () () -> IO ()
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
  

spec_getLatestSnapshot :: Spec
spec_getLatestSnapshot = withServer $ describe "getLatestSnapshot" $ do
  specify "one neighbor" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    got <- debugShowE $ fmap (sort . V.toList) $ getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_link) = case got of
          [Left a, Left b, Right c] -> (a, b, c)
          _ -> error ("Unexpected result: got = " ++ show got)
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
    got <- fmap V.toList $ getLatestSnapshot spider "n1"
    let got_n1 = case got of
          [Left a] -> a
          _ -> error ("Unexpected result: got = " ++ show got)
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeTimestamp got_n1 `shouldBe` Just (fromEpochSecond 200)
    S.nodeAttributes got_n1 `shouldBe` Just ()
  specify "missing starting node" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    got <- getLatestSnapshot spider "no node"
    got `shouldBe` mempty
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
    got <- fmap (sort . V.toList) $ getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_l) = case got of
          [Left a, Left b, Right c] -> (a, b ,c)
          _ -> error ("Unexpected result: got = " ++ show got)
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
        

-- TODO: how linkState relates to the property of SnapshotLink ?

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

