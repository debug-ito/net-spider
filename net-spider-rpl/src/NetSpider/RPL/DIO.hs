{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.DIO
-- Description: Node and link information based on DIO (DODAG Information Object)
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.DIO
  ( -- * Types
    FoundNodeDIO,
    DIONode(..),
    DIOLink(..),
    MergedDIOLink(..),
    Rank,
    NeighborType(..),
    neighborTypeToText,
    neighborTypeFromText,
    RSSI,
    -- * Unifier
    dioUnifierConf
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..))
import Data.Bifunctor (bimap)
import Data.Greskell
  ( PropertyMap, Property, GValue, parseOneValue,
    Binder, Walk, SideEffect, Element, Parser
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import qualified NetSpider.Pangraph as Pan
import NetSpider.Pangraph.Atom (toAtom, Atom)
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | 'FoundNode' for a network described by DIOs.
type FoundNodeDIO = FoundNode FindingID DIONode DIOLink

-- | RPL rank
type Rank = Word

-- | Node attributes about DIO.
data DIONode =
  DIONode
  { rank :: !Rank
    -- ^ RPL rank
  }
  deriving (Show,Eq,Ord)

instance NodeAttributes DIONode where
  writeNodeAttributes ln = writePropertyKeyValues pairs
    where
      pairs = [ ("rank", toJSON $ rank ln)
              ]
  parseNodeAttributes ps = DIONode <$> parseOneValue "rank" ps

instance Pan.ToAttributes DIONode where
  toAttributes ln = [ ("rank", toAtom $ rank ln)
                    ]

-- | Classification of RPL neighbors.
data NeighborType = PreferredParent
                    -- ^ The neighbor is the preferred parent.
                  | ParentCandidate
                    -- ^ The neighbor is not the preferred parent, but
                    -- is in the parent set.
                  | OtherNeighbor
                    -- ^ The neighbor is not in the parent set.
                  deriving (Show,Eq,Ord,Enum,Bounded)

neighborTypeToText :: NeighborType -> Text
neighborTypeToText nt = case nt of
  PreferredParent -> "preferred_parent"
  ParentCandidate -> "parent_candidate"
  OtherNeighbor -> "other_neighbor"

neighborTypeFromText :: Text -> Maybe NeighborType
neighborTypeFromText t = case t of
  "preferred_parent" -> return PreferredParent
  "parent_candidate" -> return ParentCandidate
  "other_neighbor" -> return OtherNeighbor
  _ -> Nothing

-- | Unsafely convert walk's type signature
adaptWalk :: (Element e1, Element e2) => Walk SideEffect e1 e1 -> Walk SideEffect e2 e2
adaptWalk = bimap undefined undefined

instance LinkAttributes NeighborType where
  writeLinkAttributes nt = writePropertyKeyValues [("neighbor_type", neighborTypeToText nt)]
  parseLinkAttributes ps = fromT =<< parseOneValue "neighbor_type" ps
    where
      fromT t = maybe (fail ("Unknown neighbor type: " <> unpack t)) return $ neighborTypeFromText t

-- | Link attributes about DIO.
data DIOLink =
  DIOLink
  { neighborType :: !NeighborType,
    -- ^ Type of the neighbor at the other end of this link.
    neighborRank :: !Rank,
    -- ^ Observed rank of the neighbor.
    metric :: !(Maybe Rank)
    -- ^ Link metric of this link, calculated as step of Rank. Because
    -- Rank computation is up to the OF, this field is optional.
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes DIOLink where
  writeLinkAttributes ll = do
    nt_steps <- writeLinkAttributes $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (adaptWalk nt_steps <> other)
    where
      pairs = [ ("neighbor_rank", toJSON $ neighborRank ll),
                ("metric", toJSON $ metric ll)
              ]
  parseLinkAttributes ps =
    DIOLink
    <$> parseLinkAttributes ps
    <*> parseOneValue "neighbor_rank" ps
    <*> parseOneValue "metric" ps

toAttributesPrefix :: Atom -> DIOLink -> [Pan.Attribute]
toAttributesPrefix prefix ll = 
  [ (prefix <> "neighbor_type", toAtom $ neighborTypeToText $ neighborType ll),
    (prefix <> "neighbor_rank", toAtom $ neighborRank ll)
  ]
  ++
  ( case metric ll of
      Nothing -> []
      Just metric_val -> [(prefix <> "metric", toAtom metric_val)]
  )

instance Pan.ToAttributes DIOLink where
  toAttributes = toAttributesPrefix ""

-- | Link attributes merging two 'DIOLink's from the two end nodes
-- of the link.
data MergedDIOLink =
  MergedDIOLink
  { fromSource :: !DIOLink,
    fromDest :: !(Maybe DIOLink)
  }
  deriving (Show,Eq,Ord)

-- | 'UnifyStdConfig' for RPL DIO data
dioUnifierConf :: UnifyStdConfig FindingID DIONode DIOLink MergedDIOLink ()
dioUnifierConf = Unify.UnifyStdConfig
                 { Unify.makeLinkSubId = const (),
                   Unify.mergeSamples = merger,
                   Unify.negatesLinkSample = \_ _ -> False
                 }
  where
    merger llinks rlinks =
      case (latestLinkSample llinks, latestLinkSample rlinks) of
        (Nothing, Nothing) -> Nothing
        (Just ll, Nothing) -> Just $ doMerge ll Nothing
        (Nothing, Just rl) -> Just $ doMerge rl Nothing
        (Just ll, Just rl) -> Just $ doMerge ll $ Just rl
    doMerge main_link msub_link =
      case msub_link of
        Nothing -> main_link
                   { lsLinkAttributes = MergedDIOLink main_ll Nothing }
        Just sub_link ->
          if neighborType main_ll <= neighborType sub_ll
          then main_link { lsLinkAttributes = MergedDIOLink main_ll $ Just sub_ll }
          else sub_link { lsLinkAttributes = MergedDIOLink sub_ll $ Just main_ll }
          where
            sub_ll = lsLinkAttributes sub_link
      where
        main_ll = lsLinkAttributes main_link

instance Pan.ToAttributes MergedDIOLink where
  toAttributes ml =
    toAttributesPrefix "source_" (fromSource ml)
    ++
    ( case fromDest ml of
        Nothing -> []
        Just dl -> toAttributesPrefix "dest_" dl
    )