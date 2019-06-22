{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Attributes (main,spec) where

import Data.Aeson (ToJSON)
import Data.List (sort)
import Data.Hashable (Hashable)
import Data.Greskell
  ( FromGraphSON,
    Key
  )
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Test.Hspec

import ServerTest.Common
  ( withServer, withSpider', withSpider,
    AText(..), AInt(..)
  )

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Spider
  ( addFoundNode, getSnapshotSimple, Spider
  )
import NetSpider.Spider.Config (Host, Port, Config(..), defConfig)
import NetSpider.Snapshot (nodeTimestamp, linkTimestamp, SnapshotNode, SnapshotLink)
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Timestamp (Timestamp(..), fromS)

main :: IO ()
main = hspec spec


typeTestCase :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, NodeAttributes na, Eq na, Show na, LinkAttributes la, Eq la, Show la)
             => String
             -> Config
             -> n
             -> n
             -> na
             -> la
             -> SpecWith (Host,Port)
typeTestCase test_label conf n1_id n2_id node_attrs link_attrs =
  specify test_label $ withSpider' conf $ \spider -> do
    let n1 = FoundNode { subjectNode = n1_id,
                         foundAt = fromS "2018-10-11T11:00:00",
                         neighborLinks = return link1,
                         nodeAttributes = node_attrs
                       }
        link1 = FoundLink { targetNode = n2_id,
                            linkState = LinkToSubject,
                            linkAttributes = link_attrs
                          }
    addFoundNode spider n1
    (got_ns, got_ls) <- getSnapshotSimple spider n1_id
    let (got_n1, got_n2, got_l) = case (sort got_ns, sort got_ls) of
          ([a,b], [c]) -> (a,b,c)
          _ -> error ("Unexpected pattern: got = " ++ show (got_ns, got_ls))
    S.nodeAttributes got_n1 `shouldBe` Just node_attrs
    S.nodeAttributes got_n2 `shouldBe` Nothing
    S.linkAttributes got_l `shouldBe` link_attrs

attributeTestCase :: (NodeAttributes na, Eq na, Show na, LinkAttributes la, Eq la, Show la)
                  => String
                  -> na
                  -> la
                  -> SpecWith (Host,Port)
attributeTestCase type_label na la = typeTestCase (type_label ++ " attributes") defConfig
                                     ("n1" :: Text) ("n2" :: Text) na la

nodeIdTestCase :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n)
               => String
               -> Text
               -> n -> n -> SpecWith (Host,Port)
nodeIdTestCase label node_id_key n1 n2 = typeTestCase (label ++ " nodeID") conf n1 n2 () ()
  where
    conf = defConfig { nodeIdKey = node_id_key
                     }

timestampTestCase :: String -> Timestamp -> SpecWith (Host, Port)
timestampTestCase label ts = specify label $ withSpider $ \spider -> do
  let fn :: FoundNode Text () ()
      fn = FoundNode
           { subjectNode = "n1",
             foundAt = ts,
             nodeAttributes = (),
             neighborLinks = return $ FoundLink
                             { targetNode = "n2",
                               linkState = LinkToTarget,
                               linkAttributes = ()
                             }
           }
      getSnapshotSimple' :: Spider -> Text -> IO ([SnapshotNode Text ()], [SnapshotLink Text ()])
      getSnapshotSimple' = getSnapshotSimple
  addFoundNode spider fn
  (got_ns, got_ls) <- getSnapshotSimple' spider "n1"
  let (got_n1, got_n2, got_l) = case (sort got_ns, sort got_ls) of
        ([a,b], [c]) -> (a,b,c)
        _ -> error ("Unexpected pattern: got = " ++ show (got_ns, got_ls))
  nodeTimestamp got_n1 `shouldBe` Just ts
  nodeTimestamp got_n2 `shouldBe` Nothing
  linkTimestamp got_l `shouldBe` ts

spec :: Spec
spec = withServer $ do
  describe "node and link attributes" $ do
    attributeTestCase "Text" (AText "node attrs") (AText "link attrs")
    attributeTestCase "Int" (AInt 128) (AInt 64)
  describe "nodeId" $ do
    nodeIdTestCase "Text" "@node_id_text" ("n1" :: Text) ("n2" :: Text)
    nodeIdTestCase "Int" "@node_id_int" (100 :: Int) (255 :: Int)
    -- We need to use different property keys for different NodeID
    -- types, because the graph database (at least JanusGraph)
    -- automatically fixes the schema (internal data type) for a
    -- property key when some data is given for it for the first time.
  describe "timestamp with timezone" $ do
    timestampTestCase "positive timezone"
      $ Timestamp { epochTime = 200,
                    timeZone = Just $ TimeZone
                               { timeZoneMinutes = 9*60,
                                 timeZoneSummerOnly = False,
                                 timeZoneName = "Asia/Tokyo"
                               }
                  }
    timestampTestCase "negative timezone"
      $ Timestamp { epochTime = 150,
                    timeZone = Just $ TimeZone
                               { timeZoneMinutes = (-5)*60,
                                 timeZoneSummerOnly = True,
                                 timeZoneName = "America/Chicago"
                               }
                  }
  
