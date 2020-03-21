{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.CLI.AnalyzeSpec (main,spec) where

import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Text (Text)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Log (runWriterLoggingM)
import NetSpider.Query (foundNodePolicy, unifyLinkSamples)
import NetSpider.RPL.FindingID (FindingID, idFromText, ipv6FromText, IPv6ID)
import NetSpider.RPL.DAO
  ( DAONode(..), DAOLink(..), daoDefQuery, FoundNodeDAO,
    SnapshotGraphDAO
  )
import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Weaver (Weaver, newWeaver, getSnapshot, addFoundNode)
import Test.Hspec

import NetSpider.RPL.CLI.Analyze (analyzeDAO, DODAGAttributes(DODAGAttributes))
import qualified NetSpider.RPL.CLI.Analyze as A

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_DAO


makeSnapshotDAO :: [FoundNodeDAO] -> SnapshotGraphDAO
makeSnapshotDAO fns = getDAOSnapshot $ foldl' (\w fn -> addFoundNode fn w) newDAOWeaver fns
  where
    newDAOWeaver :: Weaver FindingID DAONode DAOLink
    newDAOWeaver = newWeaver $ foundNodePolicy $ daoDefQuery []
    getDAOSnapshot :: Weaver FindingID DAONode DAOLink -> SnapshotGraphDAO
    getDAOSnapshot = getSnapshot $ unifyLinkSamples $ daoDefQuery []

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

spec_DAO :: Spec
spec_DAO = describe "analyzeDAO" $ do
  specify "root only" $ do
    let fns = [ daoNode 100 (Just 0) "dao://[fd00::1]" []
              ]
        (got, _) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 1,
          A.edge_num = 0,
          A.depth = 0,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
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
        (got, _) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 4,
          A.edge_num = 3,
          A.depth = 1,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )
  specify "depth 4" $ do
    let fns = [ daoNode 100 Nothing  "dao://[fd00::3]" ["dao://[fd00:4]"],
                daoNode 100 (Just 4) "dao://[fd00::1]" ["dao://[fd00:2]"],
                daoNode 100 Nothing  "dao://[fd00::5]" [],
                daoNode 100 Nothing  "dao://[fd00::2]" ["dao://[fd00:3]"],
                daoNode 100 Nothing  "dao://[fd00::4]" ["dao://[fd00:5]"]
              ]
        (got, _) = runWriterLoggingM $ analyzeDAO $ makeSnapshotDAO fns
    got `shouldBe`
      ( Just $ DODAGAttributes
        { A.node_num = 5,
          A.edge_num = 4,
          A.depth = 4,
          A.root = ipFromText' "fd00::1",
          A.time = fromEpochMillisecond 100
        }
      )

-- TODO: add more test cases.
--
-- - what if DAO graph data with multiple timestamps are mixed??
      
  
              
