{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.CLI.AnalyzeSpec (main,spec) where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Log (runWriterLoggingM)
import NetSpider.Query (foundNodePolicy, unifyLinkSamples)
import NetSpider.RPL.FindingID (FindingID, idFromText, ipv6FromText, IPv6ID)
import NetSpider.RPL.DAO
  ( DAONode(..), DAOLink, daoDefQuery, FoundNodeDAO,
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

spec_DAO :: Spec
spec_DAO = describe "analyzeDAO" $ do
  specify "root only" $ do
    let fns = [ FoundNode
                { subjectNode = idFromText' "dao://[fd00::1]",
                  foundAt = fromEpochMillisecond 100,
                  nodeAttributes = DAONode $ Just 0,
                  neighborLinks = []
                }
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
      
  
              
