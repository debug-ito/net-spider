{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.ContikiNGSpec (main,spec) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (Timestamp, fromEpochMillisecond)
import Test.Hspec

import NetSpider.RPL.ContikiNG (parseFile, pCoojaLogHead', FoundNodeSR, pSyslogHead)
import NetSpider.RPL.FindingID (idFromText)
import qualified NetSpider.RPL.Local as Local
import qualified NetSpider.RPL.SR as SR

main :: IO ()
main = hspec spec

srEntry :: Timestamp -> Text -> [Text] -> FoundNodeSR
srEntry ts src dests =
  FoundNode
  { subjectNode = fromJust $ idFromText src,
    foundAt = ts,
    nodeAttributes = SR.SRNode,
    neighborLinks = map toLink dests
  }
  where
    toLink d = FoundLink
               { targetNode = fromJust $ idFromText d,
                 linkState = LinkToTarget,
                 linkAttributes = SR.SRLink
               }

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
          exp_srs = [ srEntry (fromEpochMillisecond 60382) "sr://fd00::201:1:1:1" ["sr://fd00::202:2:2:2"]
                    ]
      (got_locals, got_srs) <- parseFile pCoojaLogHead' "test/data/cooja.log"
      got_locals `shouldBe` [exp_local]
      got_srs `shouldBe` exp_srs
    specify "syslog root log" $ do
      let exp_timestamp = fromEpochMillisecond 1546968337000
          exp_local =
            FoundNode
            { subjectNode = fromJust $ idFromText "local://fd00::212:1199:eebb:62c4",
              foundAt = exp_timestamp,
              nodeAttributes = Local.LocalNode { Local.rank = 128 },
              neighborLinks =
                [ FoundLink
                  { targetNode = fromJust $ idFromText "local://fd00::212:1199:bbcc:4fdf",
                    linkState = LinkToTarget,
                    linkAttributes = Local.LocalLink { Local.neighborType = Local.ParentCandidate,
                                                       Local.neighborRank = 262,
                                                       Local.metric = 128,
                                                       Local.rssi = Nothing
                                                     }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "local://fd00::212:1199:bbcc:d52d",
                    linkState = LinkToTarget,
                    linkAttributes = Local.LocalLink { Local.neighborType = Local.ParentCandidate,
                                                       Local.neighborRank = 263,
                                                       Local.metric = 128,
                                                       Local.rssi = Nothing
                                                     }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "local://fd00::212:1199:bbcc:5e88",
                    linkState = LinkToTarget,
                    linkAttributes = Local.LocalLink { Local.neighborType = Local.ParentCandidate,
                                                       Local.neighborRank = 256,
                                                       Local.metric = 128,
                                                       Local.rssi = Nothing
                                                     }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "local://fd00::212:1199:eebb:62fe",
                    linkState = LinkToTarget,
                    linkAttributes = Local.LocalLink { Local.neighborType = Local.ParentCandidate,
                                                       Local.neighborRank = 256,
                                                       Local.metric = 129,
                                                       Local.rssi = Nothing
                                                     }
                  }
                ]
            }
          exp_srs = [ srEntry exp_timestamp "sr://fd00::212:1199:eebb:62c4"
                      [ "sr://fd00::212:1199:bbcc:d52d",
                        "sr://fd00::212:1199:bbcc:4fdf",
                        "sr://fd00::212:1199:eebb:62fe",
                        "sr://fd00::212:1199:bbcc:5e88"
                      ]
                    ]
          pHead = pSyslogHead 2019 Nothing
      (got_locals, got_srs) <- parseFile pHead "test/data/syslog_root.log"
      got_locals `shouldBe` [exp_local]
      got_srs `shouldBe` exp_srs

