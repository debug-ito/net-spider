{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.Combined
-- Description: Snapshot graph combining Local and SR
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.Combined
  ( CombinedNode(..),
    CombinedLink(..),
    combineNodes,
    combineLinks,
    combineGraphs
  ) where

import Data.Bifunctor (bimap, second)
import Data.Semigroup (Semigroup(..))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..), First(..))
import GHC.Exts (groupWith)
import qualified NetSpider.Pangraph as Pan
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink,
    nodeId, nodeAttributes
  )

import NetSpider.RPL.FindingID (FindingID, IPv6ID, ipv6Only)
import NetSpider.RPL.Local (LocalNode, MergedLocalLink)
import NetSpider.RPL.SR (SRNode, SRLink)

data CombinedNode =
  CombinedNode
  { attrsLocal :: !(Maybe LocalNode),
    attrsSR :: !(Maybe SRNode)
  }
  deriving (Show,Eq,Ord)

-- | Based on instance of 'First'.
instance Semigroup CombinedNode where
  a <> b = CombinedNode local sr
    where
      local = getFirst $ (First $ attrsLocal a) <> (First $ attrsLocal b)
      sr = getFirst $ (First $ attrsSR a) <> (First $ attrsSR b)

-- | Based on instance of 'First'.
instance Monoid CombinedNode where
  mappend a b = a <> b
  mempty = CombinedNode Nothing Nothing

instance Pan.ToAttributes CombinedNode where
  toAttributes cn = (Pan.toAttributes $ attrsLocal cn)
                    ++ (Pan.toAttributes $ attrsSR cn)

data CombinedLink = CombinedLocalLink !MergedLocalLink
                  | CombinedSRLink !SRLink
                  deriving (Show,Eq,Ord)

instance Pan.ToAttributes CombinedLink where
  toAttributes (CombinedLocalLink ll) =
    ("link_type", "local") : Pan.toAttributes ll
  toAttributes (CombinedSRLink sl) =
    ("link_type", "sr") : Pan.toAttributes sl

combineNodes :: [SnapshotNode FindingID LocalNode]
             -> [SnapshotNode FindingID SRNode]
             -> [SnapshotNode IPv6ID CombinedNode]
combineNodes local_ns sr_ns = concatNodes $ map fromLocal local_ns ++ map fromSR sr_ns
  where
    fromLocal = bimap ipv6Only (\ln -> CombinedNode (Just ln) Nothing)
    fromSR = bimap ipv6Only (\sn -> CombinedNode Nothing (Just sn))
    concatNodes nodes = map merge $ groupWith nodeId nodes
      where
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

combineLinks :: [SnapshotLink FindingID MergedLocalLink]
             -> [SnapshotLink FindingID SRLink]
             -> [SnapshotLink IPv6ID CombinedLink]
combineLinks local_ls sr_ls = map fromLocal local_ls ++ map fromSR sr_ls
  where
    fromLocal = bimap ipv6Only (\ll -> CombinedLocalLink ll)
    fromSR = bimap ipv6Only (\sl -> CombinedSRLink sl)

combineGraphs :: ([SnapshotNode FindingID LocalNode], [SnapshotLink FindingID MergedLocalLink])
              -> ([SnapshotNode FindingID SRNode], [SnapshotLink FindingID SRLink])
              -> ([SnapshotNode IPv6ID CombinedNode], [SnapshotLink IPv6ID CombinedLink])
combineGraphs (local_ns, local_ls) (sr_ns, sr_ls) =
  (combineNodes local_ns sr_ns, combineLinks local_ls sr_ls)
