{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.CLI.AnalyzeSpec (main,spec) where

import Control.Monad.Logger (LogLevel(..))
import System.Log.FastLogger (fromLogStr)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Log (runWriterLoggingM, LogLine)
import NetSpider.Query (foundNodePolicy, unifyLinkSamples, Query)
import NetSpider.RPL.FindingID (FindingID, idFromText, ipv6FromText, IPv6ID)
import NetSpider.RPL.DAO
  ( DAONode(..), DAOLink(..), daoDefQuery, FoundNodeDAO,
    SnapshotGraphDAO
  )
import NetSpider.RPL.DIO
  ( DIONode(..), DIOLink(..), MergedDIOLink(..), NeighborType(..),
    FoundNodeDIO, SnapshotGraphDIO, dioDefQuery
  )
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Weaver (Weaver, newWeaver, getSnapshot, addFoundNode)
import Test.Hspec

import NetSpider.RPL.CLI.Analyze
  ( analyzeDAO, analyzeDIO,
    DODAGAttributes(DODAGAttributes)
  )
import qualified NetSpider.RPL.CLI.Analyze as A

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_DIO
  spec_DAO

makeSnapshotFromQuery :: (Ord n, Hashable n, Show n) => Query n na fla sla -> [FoundNode n na fla] -> SnapshotGraph n na sla
makeSnapshotFromQuery query fns = getS $ foldl' (\w fn -> addFoundNode fn w) newW fns
  where
    newW = newWeaver $ foundNodePolicy $ query
    getS = getSnapshot $ unifyLinkSamples $ query

makeSnapshotDAO :: [FoundNodeDAO] -> SnapshotGraphDAO
makeSnapshotDAO = makeSnapshotFromQuery $ daoDefQuery []

makeSnapshotDIO :: [FoundNodeDIO] -> SnapshotGraphDIO
makeSnapshotDIO = makeSnapshotFromQuery $ dioDefQuery []

idFromText' :: Text -> FindingID
idFromText' = fromJust . idFromText

ipFromText' :: Text -> IPv6ID
ipFromText' = fromJust . ipv6FromText

defPathLifeTime :: Word
defPathLifeTime = 3600

daoNode :: Int64 -- ^ timestamp
        -> Maybe Word -- ^ route num
        -> Text -- ^ subject
        -> [Text] -- ^ targets
        -> FoundNodeDAO
daoNode ts mroutes sub targets =
  FoundNode
  { subjectNode = idFromText' sub,
    foundAt = fromEpochMillisecond ts,
    nodeAttributes = DAONode $ mroutes,
    neighborLinks = map toFL targets
  }
  where
    toFL t = FoundLink
             { targetNode = idFromText' t,
               linkState = LinkToTarget,
               linkAttributes = DAOLink $ defPathLifeTime
             }

dioNode :: Int64
        -> Text -- ^ subject
        -> [Text] -- ^ targets
        -> FoundNodeDIO
dioNode ts sub targets =
  FoundNode
  { subjectNode = idFromText' sub,
    foundAt = fromEpochMillisecond ts,
    nodeAttributes = DIONode defRank defDioInterval,
    neighborLinks = map toFL targets
  }
  where
    defRank = 256
    defDioInterval = 10
    toFL t =
      FoundLink
      { targetNode = idFromText' t,
        linkState = LinkToTarget,
        linkAttributes = DIOLink PreferredParent defNRank Nothing
      }
    defNRank = 512

expectNoErrorLog :: [LogLine] -> IO ()
expectNoErrorLog got_logs =
  if (length $ filter matchLog got_logs) == 0
  then return ()
  else expectationFailure ("Expected no error/warn log, but found. " <> show got_logs)
  where
    matchLog (_, _, LevelError, _) = True
    matchLog (_, _, LevelWarn, _) = True
    matchLog (_, _, _, _) = False

expectErrorLog :: [LogLine] -> Text -> IO ()
expectErrorLog got_logs exp_err_msg =
  if (length $ filter matchLog got_logs) == 0
  then expectationFailure ("Expected an error log matching " <> unpack exp_err_msg <> ", but not found.")
  else return ()
  where
    matchLog (_, _, LevelError, logstr) = matchLogMsg logstr
    matchLog (_, _, _, _) = False
    matchLogMsg logstr = encodeUtf8 exp_err_msg `BS.isInfixOf` fromLogStr logstr

spec_DIO :: Spec
spec_DIO = describe "analyzeDIO" $ do
  specify "root only" $ do
    let fns = [ dioNode 100 "dio://[fd00::1]" []
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 1,
          A.edge_num = 0,
          A.depth = 0,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
    expectNoErrorLog logs
  specify "depth 1" $ do
    let fns = [ dioNode 100 "dio://[fd00::1]" [],
                dioNode 120 "dio://[fd00::2]" ["dio://[fd00::1]"],
                dioNode 140 "dio://[fd00::3]" ["dio://[fd00::1]"],
                dioNode 110 "dio://[fd00::4]" ["dio://[fd00::1]"]
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 4,
          A.edge_num = 3,
          A.depth = 1,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 140
        }
      )
    expectNoErrorLog logs
  specify "depth 4" $ do
    let fns = [ dioNode 150 "dio://[fd00::1]" [],
                dioNode 110 "dio://[fd00::4]" ["dio://[fd00::3]"],
                dioNode 120 "dio://[fd00::2]" ["dio://[fd00::1]"],
                dioNode 200 "dio://[fd00::3]" ["dio://[fd00::2]"],
                dioNode 170 "dio://[fd00::5]" ["dio://[fd00::4]"],
                dioNode 189 "dio://[fd00::6]" ["dio://[fd00::2]"]
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 6,
          A.edge_num = 5,
          A.depth = 4,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 200
        }
      )
    expectNoErrorLog logs
  specify "multiple orphan nodes with one root" $ do
    let fns = [ dioNode 110 "dio://[fd00::1]" [],
                dioNode 120 "dio://[fd00::2]" [],
                dioNode 130 "dio://[fd00::3]" [],
                dioNode 100 "dio://[fd00::4]" ["dio://[fd00::2]"]
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 4,
          A.edge_num = 1,
          A.depth = 1,
          A.root = ipFromText' "fd00::2",
          A.time = fromEpochMillisecond 130
        }
      )
    expectNoErrorLog logs
  specify "multiple roots" $ do
    let fns = [ dioNode 100 "dio://[fd00::1]" [],
                dioNode 200 "dio://[fd00::2]" ["dio://[fd00::1]"],
                dioNode 150 "dio://[fd00::3]" [],
                dioNode 130 "dio://[fd00::4]" ["dio://[fd00::3]"]
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe` Nothing
    expectErrorLog logs "multiple root"
  specify "all orphan nodes" $ do
    let fns = [ dioNode 120 "dio://[fd00::1]" [],
                dioNode 170 "dio://[fd00::2]" [],
                dioNode 110 "dio://[fd00::5]" [],
                dioNode 100 "dio://[fd00::3]" [],
                dioNode 180 "dio://[fd00::4]" []
              ]
        (got, logs) = runWriterLoggingM $ analyzeDIO $ makeSnapshotDIO fns
    got `shouldBe` Nothing
    expectErrorLog logs "orphan nodes only"


spec_DAO :: Spec
spec_DAO = describe "analyzeDAO" $ do
  specify "root only" $ do
    let fns = [ daoNode 100 (Just 0) "dao://[fd00::1]" []
              ]
        (got, logs) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 1,
          A.edge_num = 0,
          A.depth = 0,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
    expectNoErrorLog logs
  specify "depth 1" $ do
    let fns = [ daoNode 100 (Just 3) "dao://[fd00::1]"
                [ "dao://[fd00::2]",
                  "dao://[fd00::3]",
                  "dao://[fd00::4]"
                ],
                daoNode 100 Nothing "dao://[fd00::2]" [],
                daoNode 100 Nothing "dao://[fd00::3]" [],
                daoNode 100 Nothing "dao://[fd00::4]" []
              ]
        (got, logs) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 4,
          A.edge_num = 3,
          A.depth = 1,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
    expectNoErrorLog logs
  specify "depth 4" $ do
    let fns = [ daoNode 100 Nothing  "dao://[fd00::3]" ["dao://[fd00::4]"],
                daoNode 100 (Just 4) "dao://[fd00::1]" ["dao://[fd00::2]"],
                daoNode 100 Nothing  "dao://[fd00::5]" [],
                daoNode 100 Nothing  "dao://[fd00::2]" ["dao://[fd00::3]"],
                daoNode 100 Nothing  "dao://[fd00::4]" ["dao://[fd00::5]"]
              ]
        (got, logs) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 5,
          A.edge_num = 4,
          A.depth = 4,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
    expectNoErrorLog logs
  specify "multiple timestamps" $ do
    let fns = [ daoNode 100 (Just 4) "dao://[fd00::1]" ["dao://[fd00::2]", "dao://[fd00::3]"],
                daoNode 100 Nothing  "dao://[fd00::2]" ["dao://[fd00::4]", "dao://[fd00::5]"],
                daoNode 100 Nothing  "dao://[fd00::3]" [],
                daoNode 100 Nothing  "dao://[fd00::4]" [],
                daoNode 100 Nothing  "dao://[fd00::5]" [],

                daoNode 200 Nothing  "dao://[fd00::3]" ["dao://[fd00::6]"],
                daoNode 200 Nothing  "dao://[fd00::6]" []
                
                -- daoNode 200 (Just 4) "dao://[fd00::1]" ["dao://[fd00::2]", "dao://[fd00::3]", "dao://[fd00::4]"],
                -- daoNode 200 Nothing  "dao://[fd00::2]" ["dao://[fd00::5]"],
                -- daoNode 200 Nothing  "dao://[fd00::4]" [],
                -- daoNode 200 Nothing  "dao://[fd00::5]" [],
              ]
        (got, logs) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 6,
          A.edge_num = 5,
          A.depth = 2,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 200
        }
      )
    expectNoErrorLog logs

      
  
              
