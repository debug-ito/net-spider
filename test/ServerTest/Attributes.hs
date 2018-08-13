{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Attributes (main,spec) where

import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Greskell
  ( FromGraphSON,
    Key
  )
import Data.Text (Text)
import Test.Hspec

import ServerTest.Common
  ( withServer, withSpider', toSortedList,
    AText(..), AInt(..)
  )

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Spider
  ( Host, Port, addFoundNode, getLatestSnapshot,
    Config(..), defConfig
  )
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec


typeTestCase :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, NodeAttributes na, Eq na, Show na, LinkAttributes la, Eq la, Show la)
             => String
             -> Config n na la
             -> n
             -> n
             -> na
             -> la
             -> SpecWith (Host,Port)
typeTestCase test_label conf n1_id n2_id node_attrs link_attrs =
  specify test_label $ withSpider' conf $ \spider -> do
    let n1 = FoundNode { subjectNode = n1_id,
                         observationTime = fromEpochSecond 128,
                         neighborLinks = return link1,
                         nodeAttributes = node_attrs
                       }
        link1 = FoundLink { targetNode = n2_id,
                            linkState = LinkToSubject,
                            linkAttributes = link_attrs
                          }
    addFoundNode spider n1
    got <- fmap toSortedList $ getLatestSnapshot spider n1_id
    let (got_n1, got_n2, got_l) = case got of
          [Left a, Left b, Right c] -> (a,b,c)
          _ -> error ("Unexpected pattern: got = " ++ show got)
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
               -> Key VNode n
               -> n -> n -> SpecWith (Host,Port)
nodeIdTestCase label node_id_key n1 n2 = typeTestCase (label ++ " nodeID") conf n1 n2 () ()
  where
    conf = defConfig { nodeIdKey = node_id_key
                     }

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
  
