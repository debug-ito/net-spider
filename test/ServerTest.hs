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

import NetSpider.ObservedNode
  ( FoundLink(..), LinkState(..), ObservedNode(..)
  )
import NetSpider.Snapshot
  ( SnapshotElement,
    nodeId, linkNodeTuple, isDirected, linkTimestamp,
    isOnBoundary
  )
import NetSpider.Spider
  ( Host, Port, Spider,
    connectWS, close, clearAll, addObservedNode, getLatestSnapshot
  )
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_getLatestSnapshot

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: (Spider n p -> IO ()) -> (Host, Port) -> IO ()
withSpider action (host, port) = bracket (connectWS host port) close $ \spider -> do
  clearAll spider
  action spider

makeOneNeighborExample :: Spider Text () -> IO ()
makeOneNeighborExample spider = do
  let link = FoundLink { targetNode = "n2",
                         linkState = LinkToTarget,
                         linkAttributes = ()
                       }
      nbs = ObservedNode { subjectNode = "n1",
                           observedTime = fromEpochSecond 100,
                           neighborLinks = return link
                         }
  debugShowE $ addObservedNode spider nbs
  

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
    nodeId got_n2 `shouldBe` "n2"
    isOnBoundary got_n2 `shouldBe` False
    linkNodeTuple got_link `shouldBe` ("n1", "n2")
    isDirected got_link `shouldBe` True
    linkTimestamp got_link `shouldBe` fromEpochSecond 100
  specify "no neighbor" $ withSpider $ \spider -> do
    let nbs :: ObservedNode Text ()
        nbs = ObservedNode { subjectNode = "n1",
                             observedTime = fromEpochSecond 200,
                             neighborLinks = mempty
                           }
    addObservedNode spider nbs
    got <- fmap V.toList $ getLatestSnapshot spider "n1"
    let got_n1 = case got of
          [Left a] -> a
          _ -> error ("Unexpected result: got = " ++ show got)
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
  specify "missing starting node" $ withSpider $ \spider -> do
    makeOneNeighborExample spider
    got <- getLatestSnapshot spider "no node"
    got `shouldBe` mempty
  specify "mutual neighbors" $ withSpider $ \spider -> do
    let link_12 :: FoundLink Int ()
        link_12 = FoundLink { targetNode = 2,
                              linkState = LinkToSubject,
                              linkAttributes = ()
                            }
        link_21 = FoundLink { targetNode = 1,
                              linkState = LinkToTarget,
                              linkAttributes = ()
                            }
        nbs1 = ObservedNode { subjectNode = 1,
                              observedTime = fromEpochSecond 100,
                              neighborLinks = return link_12
                            }
        nbs2 = ObservedNode { subjectNode = 2,
                              observedTime = fromEpochSecond 200,
                              neighborLinks = return link_21
                            }
    mapM_ (addObservedNode spider) [nbs1, nbs2]
    got <- fmap (sort . V.toList) $ getLatestSnapshot spider 1
    let (got_n1, got_n2, got_l) = case got of
          [Left a, Left b, Right c] -> (a, b ,c)
          _ -> error ("Unexpected result: got = " ++ show got)
    nodeId got_n1 `shouldBe` 1
    isOnBoundary got_n1 `shouldBe` False
    nodeId got_n2 `shouldBe` 2
    isOnBoundary got_n2 `shouldBe` False
    linkNodeTuple got_l `shouldBe` (2, 1)
    isDirected got_l `shouldBe` True
    linkTimestamp got_l `shouldBe` fromEpochSecond 200
        

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
