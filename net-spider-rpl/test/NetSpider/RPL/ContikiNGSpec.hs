{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.ContikiNGSpec (main,spec) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (Timestamp, fromEpochMillisecond)
import Test.Hspec

import NetSpider.RPL.ContikiNG (parseFile, pCoojaLogHead', pSyslogHead)
import NetSpider.RPL.FindingID (idFromText)
import qualified NetSpider.RPL.DIO as DIO
import qualified NetSpider.RPL.DAO as DAO

main :: IO ()
main = hspec spec

daoEntry :: Timestamp -> Text -> Maybe Word -> [(Text, Word)] -> DAO.FoundNodeDAO
daoEntry ts src mroute_num dests =
  FoundNode
  { subjectNode = fromJust $ idFromText src,
    foundAt = ts,
    nodeAttributes = DAO.DAONode mroute_num,
    neighborLinks = map toLink dests
  }
  where
    toLink (dest, lifetime) =
      FoundLink
      { targetNode = fromJust $ idFromText dest,
        linkState = LinkToTarget,
        linkAttributes = DAO.DAOLink lifetime
      }

spec :: Spec
spec = do
  describe "parseFile" $ do
    specify "cooja log" $ do
      let exp_dio =
            FoundNode { subjectNode = fromJust $ idFromText "dio://[fd00::201:1:1:1]",
                        foundAt = fromEpochMillisecond 60382,
                        neighborLinks = exp_dio_links,
                        nodeAttributes = DIO.DIONode { DIO.rank = 128, DIO.dioInterval = 15 }
                      }
          exp_dio_links =
            [ FoundLink { targetNode = fromJust $ idFromText "dio://[fd00::202:2:2:2]",
                          linkState = LinkUnused,
                          linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                         DIO.neighborRank = 299,
                                                         DIO.metric = Just 141
                                                       }
                        }
            ]
          exp_daos = [ daoEntry (fromEpochMillisecond 60382)
                       "dao://[fd00::201:1:1:1]" (Just 2)
                       [("dao://[fd00::202:2:2:2]", 1740)]
                     ]
      (got_dios, got_daos) <- parseFile pCoojaLogHead' "test/data/cooja.log"
      got_dios `shouldBe` [exp_dio]
      got_daos `shouldBe` exp_daos
    specify "syslog root log" $ do
      let exp_timestamp = fromEpochMillisecond 1546968337000
          exp_dio =
            FoundNode
            { subjectNode = fromJust $ idFromText "dio://[fd00::212:1199:eebb:62c4]",
              foundAt = exp_timestamp,
              nodeAttributes = DIO.DIONode { DIO.rank = 128, DIO.dioInterval = 14 },
              neighborLinks =
                [ FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:1199:bbcc:4fdf]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 262,
                                                   DIO.metric = Just 128
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:1199:bbcc:d52d]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 263,
                                                   DIO.metric = Just 128
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:1199:bbcc:5e88]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 256,
                                                   DIO.metric = Just 128
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:1199:eebb:62fe]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 256,
                                                   DIO.metric = Just 129
                                                 }
                  }
                ]
            }
          exp_daos = [ daoEntry exp_timestamp
                       "dao://[fd00::212:1199:eebb:62c4]" (Just 5)
                       [ ("dao://[fd00::212:1199:bbcc:d52d]", 1080),
                         ("dao://[fd00::212:1199:bbcc:4fdf]", 1260),
                         ("dao://[fd00::212:1199:eebb:62fe]", 1440),
                         ("dao://[fd00::212:1199:bbcc:5e88]", 1140)
                       ]
                     ]
          pHead = pSyslogHead 2019 Nothing
      (got_dios, got_daos) <- parseFile pHead "test/data/syslog_root.log"
      got_dios `shouldBe` [exp_dio]
      got_daos `shouldBe` exp_daos
    specify "syslog nonroot" $ do
      let exp_timestamp = fromEpochMillisecond 1547558149000
          exp_dio =
            FoundNode
            { subjectNode = fromJust $ idFromText "dio://[fd00::212:eeaa:0077:2f9c]",
              foundAt = exp_timestamp,
              nodeAttributes = DIO.DIONode { DIO.rank = 423, DIO.dioInterval = 16 },
              neighborLinks =
                [ FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:33cc:632a]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 256,
                                                   DIO.metric = Just 198
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:33ff:a874]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 256,
                                                   DIO.metric = Just 177
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:33cc:63d0]",
                    linkState = LinkToTarget,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.PreferredParent,
                                                   DIO.neighborRank = 272,
                                                   DIO.metric = Just 151
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:9977:13ba]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 283,
                                                   DIO.metric = Just 152
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:33cc:6350]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 400,
                                                   DIO.metric = Just 171
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:e88:db36]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                                   DIO.neighborRank = 432,
                                                   DIO.metric = Just 166
                                                 }
                  },
                  FoundLink
                  { targetNode = fromJust $ idFromText "dio://[fd00::212:eeaa:9977:b4]",
                    linkState = LinkUnused,
                    linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.OtherNeighbor,
                                                   DIO.neighborRank = 584,
                                                   DIO.metric = Just 65535
                                                 }
                  }
                ]
            }
          pHead = pSyslogHead 2019 Nothing
      (got_dios, got_daos) <- parseFile pHead "test/data/syslog_nonroot.log"
      got_dios `shouldBe` [exp_dio]
      got_daos `shouldBe` []
    specify "syslog sr tables" $ do
      let exp_ts_jan = fromEpochMillisecond 1548843376000
          exp_ts_feb = fromEpochMillisecond 1549373162000
          exp_dio1 =
            FoundNode
            { subjectNode = fromJust $ idFromText "dio://[fd00::222:5566:cc99:62c4]",
              foundAt = exp_ts_jan,
              nodeAttributes = DIO.DIONode { DIO.rank = 128, DIO.dioInterval = 15 },
              neighborLinks = []
            }
          exp_dio2 =
            FoundNode
            { subjectNode = fromJust $ idFromText "dio://[fd00::222:5566:cc99:62c4]",
              foundAt = exp_ts_feb,
              nodeAttributes = DIO.DIONode { DIO.rank = 128, DIO.dioInterval = 18 },
              neighborLinks = exp_dio_links2
            }
          exp_dio_links2 =
            [ FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd00::222:5566:ddee:4fdf]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 256,
                                               DIO.metric = Just 128
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd00::222:5566:ddee:d52d]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 396,
                                               DIO.metric = Just 128
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd00::222:5566:cc99:62fe]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 266,
                                               DIO.metric = Just 137
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd00::222:5566:ddee:401e]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 384,
                                               DIO.metric = Just 128
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd00::222:5566:ddee:5e88]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 406,
                                               DIO.metric = Just 128
                                             }
              }
            ]
          exp_daos = [ daoEntry exp_ts_feb
                       "dao://[fd00::222:5566:cc99:62c4]" (Just 6)
                       [ ("dao://[fd00::222:5566:ddee:4fdf]", 1140),
                         ("dao://[fd00::222:5566:cc99:62fe]", 1380)
                       ],
                       daoEntry exp_ts_feb
                       "dao://[fd00::222:5566:ddee:4fdf]" Nothing
                       [ ("dao://[fd00::222:5566:ddee:d52d]", 1380),
                         ("dao://[fd00::222:5566:ddee:401e]", 1740),
                         ("dao://[fd00::222:5566:ddee:5e88]", 1740)
                       ]
                    ]
          pHead = pSyslogHead 2019 Nothing
      (got_dios, got_daos) <- parseFile pHead "test/data/syslog_sr_tables.log"
      got_dios `shouldBe` [exp_dio1, exp_dio2]
      got_daos `shouldMatchList` exp_daos
    specify "syslog_inf_rank" $ do
      let exp_dio =
            FoundNode
            { subjectNode = fromJust $ idFromText "dio://[fd00::aaa:bbbb:bcc:1008]",
              foundAt = fromEpochMillisecond 1551104666000,
              nodeAttributes = DIO.DIONode { DIO.rank = 65535, DIO.dioInterval = 12 },
              neighborLinks = exp_links
            }
          exp_links =
            [ FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd80::aaa:bbbb:bcc:100a]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 492,
                                               DIO.metric = Just 601
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd80::aaa:bbbb:9221:d51a]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 422,
                                               DIO.metric = Just 601
                                             }
              },
              FoundLink
              { targetNode = fromJust $ idFromText "dio://[fd80::aaa:bbbb:bcc:d5e8]",
                linkState = LinkUnused,
                linkAttributes = DIO.DIOLink { DIO.neighborType = DIO.ParentCandidate,
                                               DIO.neighborRank = 65535,
                                               DIO.metric = Just 133
                                             }
              }
            ]
          pHead = pSyslogHead 2019 Nothing
      (got_dios, got_daos) <- parseFile pHead "test/data/syslog_inf_rank.log"
      got_dios `shouldBe` [exp_dio]
      got_daos `shouldMatchList` []
