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
import NetSpider.Spider (addFoundNode, getLatestSnapshot)
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


spec :: Spec
spec = withServer $ describe "node and link attributes" $ do
  specify "Text attribute" $ withSpider $ \spider -> do
    let n1 :: FoundNode Text AText AText
        n1 = FoundNode { subjectNode = "n1",
                         observationTime = fromEpochSecond 128,
                         neighborLinks = return link1,
                         nodeAttributes = AText "node attrs"
                       }
        link1 = FoundLink { targetNode = "n2",
                            linkState = LinkToSubject,
                            linkAttributes = AText "link attrs"
                          }
    addFoundNode spider n1
    got <- fmap toSortedList $ getLatestSnapshot spider "n1"
    let (got_n1, got_n2, got_l) = case got of
          [Left a, Left b, Right c] -> (a,b,c)
          _ -> error ("Unexpected pattern: got = " ++ show got)
    S.nodeAttributes got_n1 `shouldBe` Just (AText "node attrs")
    S.nodeAttributes got_n2 `shouldBe` Nothing
    S.linkAttributes got_l `shouldBe` (AText "link attrs")
    
