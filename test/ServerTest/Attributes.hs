{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Attributes (main,spec) where

import Control.Applicative ((<$>))
import Data.Greskell
  ( gProperty,
    newBind,
    parseOneValue
  )
import Data.Text (Text)
import Test.Hspec

import ServerTest.Common (withServer, withSpider, toSortedList)

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Spider (Host, Port, addFoundNode, getLatestSnapshot)
import qualified NetSpider.Snapshot as S (nodeAttributes, linkAttributes)
import NetSpider.Timestamp (fromEpochSecond)

main :: IO ()
main = hspec spec

newtype AText = AText Text
              deriving (Show,Eq,Ord)

instance NodeAttributes AText where
  writeNodeAttributes (AText t) = gProperty "text" <$> newBind t
  parseNodeAttributes ps = AText <$> parseOneValue "text" ps

instance LinkAttributes AText where
  writeLinkAttributes (AText t) = gProperty "text" <$> newBind t
  parseLinkAttributes ps = AText <$> parseOneValue "text" ps

newtype AInt = AInt Int
             deriving (Show,Eq,Ord)

instance NodeAttributes AInt where
  writeNodeAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseNodeAttributes ps = AInt <$> parseOneValue "integer" ps

instance LinkAttributes AInt where
  writeLinkAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseLinkAttributes ps = AInt <$> parseOneValue "integer" ps


attributeTestCase :: (NodeAttributes na, Eq na, Show na, LinkAttributes la, Eq la, Show la)
                  => String
                  -> na
                  -> la
                  -> SpecWith (Host,Port)
attributeTestCase type_label node_attrs link_attrs =
  specify (type_label ++ " attribute") $ withSpider $ \spider -> do
    let n1 = FoundNode { subjectNode = ("n1" :: Text),
                         observationTime = fromEpochSecond 128,
                         neighborLinks = return link1,
                         nodeAttributes = node_attrs
                       }
        link1 = FoundLink { targetNode = ("n2" :: Text),
                            linkState = LinkToSubject,
                            linkAttributes = link_attrs
                          }
    addFoundNode spider n1
    got <- fmap toSortedList $ getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_l) = case got of
          [Left a, Left b, Right c] -> (a,b,c)
          _ -> error ("Unexpected pattern: got = " ++ show got)
    S.nodeAttributes got_n1 `shouldBe` Just node_attrs
    S.nodeAttributes got_n2 `shouldBe` Nothing
    S.linkAttributes got_l `shouldBe` link_attrs
  


spec :: Spec
spec = withServer $ describe "node and link attributes" $ do
  attributeTestCase "Text" (AText "node attrs") (AText "link attrs")
  attributeTestCase "Int" (AInt 128) (AInt 64)
