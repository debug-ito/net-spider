{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.Combined
-- Description: Snapshot graph combining DIO and DAO graphs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines functions and data models that combine DIO
-- (defined in "NetSpider.RPL.DIO") and DAO (defined in
-- "NetSpider.RPL.DAO") graphs.
module NetSpider.RPL.Combined
  ( -- * Functions
    combineGraphs,
    combineNodes,
    combineLinks,
    -- * Types
    SnapshotGraphCombined,
    CombinedNode(..),
    CombinedLink(..),
    combinedLinkType
  ) where

import Data.Bifunctor (bimap, second)
import Data.List (sortOn, reverse)
import Data.Semigroup (Semigroup(..))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..), First(..))
import GHC.Exts (groupWith)
import qualified NetSpider.GraphML.Writer as GraphML
import qualified NetSpider.Pangraph as Pan
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink, SnapshotGraph,
    nodeId, nodeAttributes, nodeTimestamp
  )

import NetSpider.RPL.FindingID (FindingID(..), FindingType(..), IPv6ID, ipv6Only)
import NetSpider.RPL.DIO (DIONode, MergedDIOLink, SnapshotGraphDIO)
import NetSpider.RPL.DAO (DAONode, DAOLink, SnapshotGraphDAO)

-- | Node attributes combining 'DIONode' and 'DAONode'.
data CombinedNode =
  CombinedNode
  { attrsDIO :: Maybe DIONode,
    attrsDAO :: Maybe DAONode
  }
  deriving (Show,Eq,Ord)

-- | Based on instance of 'First'.
instance Semigroup CombinedNode where
  a <> b = CombinedNode dio dao
    where
      dio = getFirst $ (First $ attrsDIO a) <> (First $ attrsDIO b)
      dao = getFirst $ (First $ attrsDAO a) <> (First $ attrsDAO b)

-- | Based on instance of 'First'.
instance Monoid CombinedNode where
  mappend a b = a <> b
  mempty = CombinedNode Nothing Nothing

instance Pan.ToAttributes CombinedNode where
  toAttributes cn = (Pan.toAttributes $ attrsDIO cn)
                    ++ (Pan.toAttributes $ attrsDAO cn)

instance GraphML.ToAttributes CombinedNode where
  toAttributes cn = (GraphML.toAttributes $ attrsDIO cn)
                    ++ (GraphML.toAttributes $ attrsDAO cn)

-- | Link attribute combining 'MergedDIOLink' and 'DAOLink'.
data CombinedLink = CombinedDIOLink MergedDIOLink
                  | CombinedDAOLink DAOLink
                  deriving (Show,Eq,Ord)

instance Pan.ToAttributes CombinedLink where
  toAttributes (CombinedDIOLink ll) =
    ("link_type", "dio") : Pan.toAttributes ll
  toAttributes (CombinedDAOLink sl) =
    ("link_type", "dao") : Pan.toAttributes sl

instance GraphML.ToAttributes CombinedLink where
  toAttributes (CombinedDIOLink ll) =
    ("link_type", GraphML.AttrString "dio") : GraphML.toAttributes ll
  toAttributes (CombinedDAOLink ll) =
    ("link_type", GraphML.AttrString "dao") : GraphML.toAttributes ll

combinedLinkType :: CombinedLink -> FindingType
combinedLinkType (CombinedDIOLink _) = FindingDIO
combinedLinkType (CombinedDAOLink _) = FindingDAO

-- | Combine DIO and DAO 'SnapshotNode's. Attributes from 'DIONode'
-- and 'DAONode' for the same 'IPv6ID' are combined into one
-- 'CombinedNode'. Timestamp of a combined 'SnapshotNode' is the
-- latest timestamp of input nodes for that 'IPv6ID'.
combineNodes :: [SnapshotNode FindingID DIONode]
             -> [SnapshotNode FindingID DAONode]
             -> [SnapshotNode IPv6ID CombinedNode]
combineNodes dio_ns dao_ns = concatNodes $ map fromDIO dio_ns ++ map fromDAO dao_ns
  where
    fromDIO = bimap ipv6Only (\ln -> CombinedNode (Just ln) Nothing)
    fromDAO = bimap ipv6Only (\sn -> CombinedNode Nothing (Just sn))
    concatNodes nodes = map (merge . sortByTimestamp) $ groupWith nodeId nodes
      where
        sortByTimestamp = reverse . sortOn nodeTimestamp
        merge [] = error "Empty group should not happen."
        merge group_nodes@(head_node : _) =
          case mmerged_attr of
            Nothing ->
              -- No node has NodeAttribute. There is no need to merge.
              head_node
            Just merged_attr ->
              case filter hasNodeAttr group_nodes of
                [] -> error "At least one node must have NodeAttributes"
                (representative_node : _) -> second (const merged_attr) representative_node
          where
            mmerged_attr = mconcat $ map nodeAttributes group_nodes
            hasNodeAttr n = isJust $ nodeAttributes n

-- | Convert DIO and DAO links into combined links. Despite its name,
-- this function does not combine input links. It just make one
-- combined link from each of the input links.
combineLinks :: [SnapshotLink FindingID MergedDIOLink]
             -> [SnapshotLink FindingID DAOLink]
             -> [SnapshotLink IPv6ID CombinedLink]
combineLinks dio_ls dao_ls = map fromDIO dio_ls ++ map fromDAO dao_ls
  where
    fromDIO = bimap ipv6Only (\ll -> CombinedDIOLink ll)
    fromDAO = bimap ipv6Only (\sl -> CombinedDAOLink sl)

-- | 'SnapshotGraph' combining DIO and DAO networks.
type SnapshotGraphCombined = SnapshotGraph IPv6ID CombinedNode CombinedLink

-- | Combine DIO and DAO graphs into the combined graph, using
-- 'combineNodes' and 'combineLinks'.
combineGraphs :: SnapshotGraphDIO
              -> SnapshotGraphDAO
              -> SnapshotGraphCombined
combineGraphs (dio_ns, dio_ls) (dao_ns, dao_ls) =
  (combineNodes dio_ns dao_ns, combineLinks dio_ls dao_ls)
