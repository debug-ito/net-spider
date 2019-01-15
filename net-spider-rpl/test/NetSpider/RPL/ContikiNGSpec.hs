{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.ContikiNGSpec (main,spec) where

import Data.Maybe (fromJust)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromEpochMillisecond)
import Test.Hspec

import NetSpider.RPL.ContikiNG (parseFile, pCoojaLogHead')
import NetSpider.RPL.FindingID (idFromText)
import qualified NetSpider.RPL.Local as Local
import qualified NetSpider.RPL.SR as SR

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseFile" $ do
    specify "cooja log" $ do
      let exp_local =
            FoundNode { subjectNode = fromJust $ idFromText "local://fd00::201:1:1:1",
                        foundAt = fromEpochMillisecond 60382,
                        neighborLinks = exp_local_links,
                        nodeAttributes = Local.LocalNode { Local.rank = 128 }
                      }
          exp_local_links =
            [ FoundLink { targetNode = fromJust $ idFromText "local://fd00::202:2:2:2",
                          linkState = LinkToTarget,
                          linkAttributes = Local.LocalLink { Local.neighborType = Local.ParentCandidate,
                                                             Local.neighborRank = 299,
                                                             Local.metric = 141,
                                                             Local.rssi = Nothing
                                                           }
                        }
            ]
          exp_srs = [ FoundNode { subjectNode = fromJust $ idFromText "sr://fd00::201:1:1:1",
                                  foundAt = fromEpochMillisecond 60382,
                                  nodeAttributes = SR.SRNode,
                                  neighborLinks = [ FoundLink
                                                    { targetNode = fromJust $ idFromText "sr://fd00::202:2:2:2",
                                                      linkState = LinkToTarget,
                                                      linkAttributes = SR.SRLink
                                                    }
                                                  ]
                                }
                    ]
      (got_locals, got_srs) <- parseFile pCoojaLogHead' "test/data/cooja.log"
      got_locals `shouldBe` [exp_local]
      got_srs `shouldBe` exp_srs
