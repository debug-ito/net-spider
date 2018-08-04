{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket, withException)
import Control.Monad (mapM_)
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket as Gr
import qualified Network.Greskell.WebSocket.Response as Res
import System.IO (stderr)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Neighbors
  ( FoundLink(..), LinkState(..), Neighbors(..)
  )
import NetSpider.Snapshot
  ( SnapshotElement,
    nodeId, linkTuple, isDirected, linkTimestamp,
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
    let link :: FoundLink Text Text
        link = FoundLink { subjectPort = "p1",
                           targetNode = "n2",
                           targetPort = "p9",
                           linkState = LinkToTarget
                         }
        nbs :: Neighbors Text Text
        nbs = Neighbors { subjectNode = "n1",
                          observedTime = fromEpochSecond 100,
                          neighborLinks = return link
                        }
    flip withException showSubmitException $ addNeighbors spider nbs
    got <- flip withException showSubmitException
           $ fmap (sort . V.toList) $ (getLatestSnapshot spider :: IO (Vector (SnapshotElement Text Text)))
    let (got_n1, got_n2, got_link) = case got of
          [Left a, Left b, Right c] -> (a, b, c)
          _ -> error ("Unexpected result: got = " ++ show got)
    nodeId got_n1 `shouldBe` "n1"
    isOnBoundary got_n1 `shouldBe` False
    nodeId got_n2 `shouldBe` "n2"
    isOnBoundary got_n2 `shouldBe` False
    linkTuple got_link `shouldBe` ("n1", "n2", "p1", "p9")
    isDirected got_link `shouldBe` True
    linkTimestamp got_link `shouldBe` fromEpochSecond 100

-- TODO: how linkState relates to the property of SnapshotLink ?

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

