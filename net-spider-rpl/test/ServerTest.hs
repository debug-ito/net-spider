module Main (main,spec) where

import Control.Monad (mapM_)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import qualified NetSpider.Snapshot as Sn
import NetSpider.Spider (Spider, withSpider, addFoundNode, clearAll, getSnapshot)
import NetSpider.Spider.Config (Host, Port, defConfig, wsHost, wsPort)
import NetSpider.Timestamp (fromEpochMillisecond)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.RPL.DIO
  ( DIONode(..), DIOLink(..), NeighborType(..), dioDefQuery,
    MergedDIOLink(..)
  )
import NetSpider.RPL.FindingID (FindingID, idFromText)

main :: IO ()
main = hspec spec

withSpider' :: (Spider FindingID na fla -> IO ()) -> (Host, Port) -> IO ()
withSpider' act (h, p) = withSpider conf act
  where
    conf = defConfig { wsHost = h, wsPort = p }

sortBoth :: (Ord a, Ord b) => ([a], [b]) -> ([a], [b])
sortBoth (as, bs) = (sort as, sort bs)

idBoth :: (Text, Text) -> (FindingID, FindingID)
idBoth (a, b) = (fromJust $ idFromText a, fromJust $ idFromText b)

spec :: Spec
spec = before (needEnvHostPort Need "NET_SPIDER_TEST") $ describe "server test" $ do
  specify "DIONode and DIOLink" $ withSpider' $ \spider -> do
    let input = [ FoundNode
                  { subjectNode = fromJust $ idFromText "dio://[fd00::1]",
                    foundAt = fromEpochMillisecond 120,
                    nodeAttributes = DIONode { rank = 256, dioInterval = 5 },
                    neighborLinks = []
                  },
                  FoundNode
                  { subjectNode = fromJust $ idFromText "dio://[fd00::2]",
                    foundAt = fromEpochMillisecond 125,
                    nodeAttributes = DIONode { rank = 556, dioInterval = 7 },
                    neighborLinks =
                      [ FoundLink
                        { targetNode = fromJust $ idFromText "dio://[fd00::1]",
                          linkState = LinkToTarget,
                          linkAttributes =
                            DIOLink
                            { neighborType = PreferredParent,
                              neighborRank = 256,
                              metric = Just 300
                            }
                        }
                      ]
                  },
                  FoundNode
                  { subjectNode = fromJust $ idFromText "dio://[fd00::3]",
                    foundAt = fromEpochMillisecond 122,
                    nodeAttributes = DIONode { rank = 618 , dioInterval = 7 },
                    neighborLinks =
                      [ FoundLink
                        { targetNode = fromJust $ idFromText "dio://[fd00::1]",
                          linkState = LinkToTarget,
                          linkAttributes =
                            DIOLink
                            { neighborType = PreferredParent,
                              neighborRank = 256,
                              metric = Nothing
                            }
                        }
                      ]
                  }
                ]
    clearAll spider
    mapM_ (addFoundNode spider) input
    let query = dioDefQuery $ map (fromJust . idFromText) ["dio://[fd00::2]", "dio://[fd00::3]"]
    (got_nodes, got_links) <- fmap sortBoth $ getSnapshot spider query
    map Sn.nodeId got_nodes `shouldBe`
      map (fromJust . idFromText) [ "dio://[fd00::1]",
                                    "dio://[fd00::2]",
                                    "dio://[fd00::3]"
                                  ]
    map Sn.nodeAttributes got_nodes `shouldBe`
      [ Just $ DIONode { rank = 256, dioInterval = 5 },
        Just $ DIONode { rank = 556, dioInterval = 7 },
        Just $ DIONode { rank = 618, dioInterval = 7 }
      ]
    map Sn.linkNodeTuple got_links `shouldBe`
      [ idBoth ("dio://[fd00::2]", "dio://[fd00::1]"),
        idBoth ("dio://[fd00::3]", "dio://[fd00::1]")
      ]
    map Sn.linkAttributes got_links `shouldBe`
      [ MergedDIOLink
        ( DIOLink
          { neighborType = PreferredParent,
            neighborRank = 256,
            metric = Just 300
          }
        )
        Nothing,
        MergedDIOLink
        ( DIOLink
          { neighborType = PreferredParent,
            neighborRank = 256,
            metric = Nothing
          }
        )
        Nothing
      ]
  specify "DAONode and DAOLink" $ \(_, _) -> do
    True `shouldBe` False -- TODO
  
