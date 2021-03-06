{-# LANGUAGE OverloadedStrings #-}
module TestCommon
       ( sortSnapshotElements,
         AText(..),
         AInt(..),
         APorts(..),
         subIdWithAPorts,
         alignAPortsToLinkDirection
       ) where

import Control.Applicative ((<$>))
import Control.Category ((<<<))
import Data.Aeson.Types (Parser)
import Data.Greskell (gProperty, newBind, lookupAs, pMapToFail, Key)
import Data.List (sort)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec

import NetSpider.Found (LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Pair (Pair(..))
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink
  )
import NetSpider.Unify
  ( LinkSample(..)
  )

sortSnapshotElements :: (Ord n, Eq na, Eq la)
                     => ([SnapshotNode n na], [SnapshotLink n la])
                     -> (Vector (SnapshotNode n na), Vector (SnapshotLink n la))
sortSnapshotElements (ns, ls) = (sortV ns, sortV ls)
  where
    sortV :: Ord a => [a] -> Vector a
    sortV = V.fromList . sort


newtype AText = AText Text
              deriving (Show,Eq,Ord)

keyText :: Key e Text
keyText = "text"

instance NodeAttributes AText where
  writeNodeAttributes (AText t) = gProperty keyText <$> newBind t
  parseNodeAttributes ps = fmap AText $ pMapToFail $ lookupAs keyText ps

instance LinkAttributes AText where
  writeLinkAttributes (AText t) = gProperty keyText <$> newBind t
  parseLinkAttributes ps = fmap AText $ pMapToFail $ lookupAs keyText ps

newtype AInt = AInt Int
             deriving (Show,Eq,Ord)

keyInt :: Key e Int 
keyInt = "integer"

instance NodeAttributes AInt where
  writeNodeAttributes (AInt n) = gProperty keyInt <$> newBind n
  parseNodeAttributes ps = fmap AInt $ pMapToFail $ lookupAs keyInt ps

instance LinkAttributes AInt where
  writeLinkAttributes (AInt n) = gProperty keyInt <$> newBind n
  parseLinkAttributes ps = fmap AInt $ pMapToFail $ lookupAs keyInt ps


-- | Pair of ports.
--
-- This type is used in two ways. (subject port, target port) and
-- (source port, destination port).
data APorts =
  APorts
  { apFst :: Text,
    apSnd :: Text
  }
  deriving (Show,Eq,Ord)

keySubjectPort :: Key e Text
keySubjectPort = "subject_port"

keyTargetPort :: Key e Text
keyTargetPort = "target_port"

instance LinkAttributes APorts where
  writeLinkAttributes ap = do
    writeSource <- gProperty keySubjectPort <$> newBind (apFst ap)
    writeDest <- gProperty keyTargetPort <$> newBind (apSnd ap)
    return (writeDest <<< writeSource)
  parseLinkAttributes ps = APorts
                           <$> (pMapToFail $ lookupAs keySubjectPort ps)
                           <*> (pMapToFail $ lookupAs keyTargetPort ps)

swapAPorts :: APorts -> APorts
swapAPorts ap = ap { apFst = apSnd ap,
                     apSnd = apFst ap
                   }

-- | Link sub-ID with 'APorts'.
subIdWithAPorts :: LinkSample n APorts -> Pair (n, Text)
subIdWithAPorts ls = Pair ( (lsSubjectNode ls, apFst $ lsLinkAttributes ls),
                            (lsTargetNode ls, apSnd $ lsLinkAttributes ls)
                          )

-- | Sort 'APorts' according to 'LinkState'. This converts the
-- 'APorts' as (subject port, target port) into (source port,
-- destination port).
alignAPortsToLinkDirection :: LinkSample n APorts -> LinkSample n APorts
alignAPortsToLinkDirection ls = ls { lsLinkAttributes = updated }
  where
    updated = case lsLinkState ls of
               LinkToSubject -> swapAPorts $ lsLinkAttributes ls
               _ -> lsLinkAttributes ls
