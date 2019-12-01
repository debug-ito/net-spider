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
    SnapshotGraphDIO,
    DIONode(..),
    DIOLink(..),
    dioLinkState,
    MergedDIOLink(..),
    Rank,
    TrickleInterval,
    NeighborType(..),
    neighborTypeToText,
    neighborTypeFromText,
    -- * Query
    dioDefQuery,
    dioUnifierConf
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..))
import Data.Bifunctor (bimap)
import Data.Greskell
  ( Property, GValue,
    Binder, Walk, SideEffect, Element, Parser,
    Key,
    lookupAsF, lookupAs, maybeNotFound, describePMapLookupException
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Word (Word32)
import NetSpider.Found (FoundNode, LinkState(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VFoundNode, EFinds)
import qualified NetSpider.GraphML.Writer as GraphML
import qualified NetSpider.Query as Query
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | 'FoundNode' for a network described by DIOs.
type FoundNodeDIO = FoundNode FindingID DIONode DIOLink

-- | 'SnapshotGraph' for a network described by DIOs. This is what you
-- get by 'dioDefQuery'.
type SnapshotGraphDIO = SnapshotGraph FindingID DIONode MergedDIOLink

-- | RPL rank
type Rank = Word

-- | The interval of Trickle timer as decribed as number of doublings
-- of the minimum interval, i.e. log2(I / Imin).
--
-- @since 0.2.1.0
type TrickleInterval = Word

-- | Node attributes about DIO.
data DIONode =
  DIONode
  { rank :: Rank,
    -- ^ RPL rank
    dioInterval :: TrickleInterval
    -- ^ Current interval of Trickle timer for DIO transmission.
  }
  deriving (Show,Eq,Ord)

instance NodeAttributes DIONode where
  writeNodeAttributes ln = writePropertyKeyValues pairs
    where
      pairs = [ ("rank", toJSON $ rank ln),
                ("dio_interval", toJSON $ dioInterval ln)
              ]
  parseNodeAttributes ps = DIONode
                           <$> lookupAsF ("rank" :: Key VFoundNode Rank) ps
                           <*> lookupAsF ("dio_interval" :: Key VFoundNode TrickleInterval) ps

instance GraphML.ToAttributes DIONode where
  toAttributes ln = [ ("rank", GraphML.AttrInt $ fromIntegral $ rank ln),
                      ("dio_interval", GraphML.AttrInt $ fromIntegral $ dioInterval ln)
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
  parseLinkAttributes ps = fromT =<< lookupAsF ("neighbor_type" :: Key EFinds Text) ps
    where
      fromT t = maybe (fail ("Unknown neighbor type: " <> unpack t)) return $ neighborTypeFromText t

-- | Link attributes about DIO.
--
-- Basically this represents information of a neighbor learned from
-- the DIOs it has sent.
data DIOLink =
  DIOLink
  { neighborType :: NeighborType,
    -- ^ Type of the neighbor at the other end of this link.
    neighborRank :: Rank,
    -- ^ Observed rank of the neighbor.
    metric :: Maybe Rank
    -- ^ Link metric of this link, calculated as step of Rank. Because
    -- Rank computation is up to the Objective Function, this field is
    -- optional.
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes DIOLink where
  writeLinkAttributes ll = do
    nt_steps <- writeLinkAttributes $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (adaptWalk nt_steps <> other)
    where
      pairs = [ ("neighbor_rank", toJSON $ neighborRank ll)
              ] ++
              ( case metric ll of
                  Just m -> [("metric", toJSON m)]
                  Nothing -> []
              )
  parseLinkAttributes ps =
    DIOLink
    <$> parseLinkAttributes ps
    <*> lookupAsF ("neighbor_rank" :: Key EFinds Rank) ps
    <*> ( either (fail . describePMapLookupException) return $
          maybeNotFound $ lookupAs ("metric" :: Key EFinds (Maybe Rank)) ps
        )

-- | 'LinkState' that should be set for given 'DIOLink'.
dioLinkState :: DIOLink -> LinkState
dioLinkState l =
  case neighborType l of
    PreferredParent -> LinkToTarget
    _ -> LinkUnused

instance GraphML.ToAttributes DIOLink where
  toAttributes ll = [ ("neighbor_type", GraphML.AttrString $ neighborTypeToText $ neighborType ll),
                      ("neighbor_rank", GraphML.AttrInt $ fromIntegral $ neighborRank ll)
                    ]
                    ++ at_metric
    where
      at_metric =
        case metric ll of
          Nothing -> []
          Just m -> [("metric", GraphML.AttrInt $ fromIntegral m)]
                    

-- | Link attributes merging two 'DIOLink's from the two end nodes
-- of the link.
data MergedDIOLink =
  MergedDIOLink
  { fromSource :: DIOLink,
    fromDest :: Maybe DIOLink
  }
  deriving (Show,Eq,Ord)

withKeyPrefix :: Monoid k
              => k
              -> [(k, v)]
              -> [(k, v)]
withKeyPrefix prefix = map prependPrefix
  where
    prependPrefix (k, v) = (prefix <> k, v)

-- | Default 'Query.Query' for DIO nodes.
dioDefQuery :: [FindingID] -- ^ 'Query.startsFrom' field.
         -> Query.Query FindingID DIONode DIOLink MergedDIOLink
dioDefQuery start =
  (Query.defQuery start)
  { Query.startsFrom = start,
    Query.unifyLinkSamples = Unify.unifyStd dioUnifierConf
  }

-- | 'UnifyStdConfig' for RPL DIO data. Used in 'defQuery'.
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

instance GraphML.ToAttributes MergedDIOLink where
  toAttributes ml =
    (withKeyPrefix "source_" $ GraphML.toAttributes $ fromSource ml)
    ++
    ( case fromDest ml of
        Nothing -> []
        Just dl -> withKeyPrefix "dest_" $ GraphML.toAttributes dl
    )
