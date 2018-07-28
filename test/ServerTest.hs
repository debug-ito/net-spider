module Main (main,spec) where

import Control.Exception.Safe (bracket)
import Data.List (sort)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Neighbors
  ( FoundLink(..), LinkState(..), Neighbors(..)
  )
import NetSpider.Snapshot
  ( nodeId, linkTuple, isDirected, linkTimestamp,
    isOnBoundary
  )
import NetSpider.Spider
  ( Host, Port, Spider,
    connectWS, close, clearAll, addNeighbors, getLatestSnapshot
  )
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_getLatestSnapshot

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: (Spider -> IO ()) -> (Host, Port) -> IO ()
withSpider action (host, port) = bracket (connectWS host port) close $ \spider -> do
  clearAll spider
  action spider

spec_getLatestSnapshot :: Spec
spec_getLatestSnapshot = withServer $ describe "getLatestSnapshot" $ do
  specify "one neighbor" $ withSpider $ \spider -> do
    let link = FoundLink { subjectPort = "p1",
                           targetNode = "n2",
                           targetPort = "p9",
                           linkState = LinkToTarget
                         }
        nbs = Neighbors { subjectNode = "n1",
                          observedTime = fromEpochSecond 100,
                          neighborLinks = return link
                        }
    addNeighbors spider nbs
    got <- fmap (sort . V.toList) $ getLatestSnapshot spider 
    let [Left got_n1, Left got_n2, Right got_link] = got
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeId got_n2 `shouldBe` "n2"
    isOnBoundary got_n2 `shouldBe` False
    linkTuple got_link `shouldBe` ("n1", "n2", "p1", "p9")
    isDirected got_link `shouldBe` True
    linkTimestamp got_link `shouldBe` fromEpochSecond 100

-- TODO: how linkState relates to the property of SnapshotLink ?
