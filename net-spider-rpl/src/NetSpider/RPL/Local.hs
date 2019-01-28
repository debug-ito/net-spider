{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.Local
-- Description: NetSpider data model for RPL neighbor table
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.Local
  ( -- * Types
    FoundNodeLocal,
    LocalNode(..),
    LocalLink(..),
    MergedLocalLink(..),
    Rank,
    NeighborType(..),
    neighborTypeToText,
    neighborTypeFromText,
    RSSI,
    -- * Unifier
    localUnifierConf
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

-- | The 'FoundNode' observed in the local neighbor table of a node.
type FoundNodeLocal = FoundNode FindingID LocalNode LocalLink

-- | RPL rank
type Rank = Word

-- | Node attributes observed locally at an individual node.
data LocalNode =
  LocalNode
  { rank :: !Rank
    -- ^ RPL rank
  }
  deriving (Show,Eq,Ord)

instance NodeAttributes LocalNode where
  writeNodeAttributes ln = writePropertyKeyValues pairs
    where
      pairs = [ ("rank", toJSON $ rank ln)
              ]
  parseNodeAttributes ps = LocalNode <$> parseOneValue "rank" ps

instance Pan.ToAttributes LocalNode where
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

-- | Type of RSSI (Radio Signal Strength Indicator) in dBm.
type RSSI = Int

-- | Link attributes observed locally at an individual node.
data LocalLink =
  LocalLink
  { neighborType :: !NeighborType,
    -- ^ Type of the neighbor at the other end of this link.
    neighborRank :: !Rank,
    -- ^ Observed rank of the neighbor.
    metric :: !Rank,
    -- ^ Link metric of this link.
    rssi :: !(Maybe RSSI)
    -- ^ RSSI observed via this link.
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes LocalLink where
  writeLinkAttributes ll = do
    nt_steps <- writeLinkAttributes $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (adaptWalk nt_steps <> other)
    where
      pairs = [ ("neighbor_rank", toJSON $ neighborRank ll),
                ("metric", toJSON $ metric ll),
                ("rssi", toJSON $ rssi ll)
              ]
  parseLinkAttributes ps =
    LocalLink
    <$> parseLinkAttributes ps
    <*> parseOneValue "neighbor_rank" ps
    <*> parseOneValue "metric" ps
    <*> parseOneValue "rssi" ps

toAttributesPrefix :: Atom -> LocalLink -> [Pan.Attribute]
toAttributesPrefix prefix ll = 
  [ (prefix <> "neighbor_type", toAtom $ neighborTypeToText $ neighborType ll),
    (prefix <> "neighbor_rank", toAtom $ neighborRank ll),
    (prefix <> "metric", toAtom $ metric ll)
  ]
  ++
  ( case rssi ll of
      Nothing -> []
      Just rssi_val -> [(prefix <> "rssi", toAtom rssi_val)]
  )

instance Pan.ToAttributes LocalLink where
  toAttributes = toAttributesPrefix ""

-- | Link attributes merging two 'LocalLink's from the two end nodes
-- of the link.
data MergedLocalLink =
  MergedLocalLink
  { fromSource :: !LocalLink,
    fromDest :: !(Maybe LocalLink)
  }
  deriving (Show,Eq,Ord)

-- | 'UnifyStdConfig' for RPL local findings.
localUnifierConf :: UnifyStdConfig FindingID LocalNode LocalLink MergedLocalLink ()
localUnifierConf = Unify.UnifyStdConfig
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
                   { lsLinkAttributes = MergedLocalLink main_ll Nothing }
        Just sub_link ->
          if neighborType main_ll <= neighborType sub_ll
          then main_link { lsLinkAttributes = MergedLocalLink main_ll $ Just sub_ll }
          else sub_link { lsLinkAttributes = MergedLocalLink sub_ll $ Just main_ll }
          where
            sub_ll = lsLinkAttributes sub_link
      where
        main_ll = lsLinkAttributes main_link

instance Pan.ToAttributes MergedLocalLink where
  toAttributes ml =
    toAttributesPrefix "source_" (fromSource ml)
    ++
    ( case fromDest ml of
        Nothing -> []
        Just dl -> toAttributesPrefix "dest_" dl
    )
