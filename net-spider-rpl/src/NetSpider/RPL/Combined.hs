{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.Combined
-- Description: Snapshot graph combining DIO and DAO graphs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.Combined
  ( -- * Functions
    combineNodes,
    combineLinks,
    combineGraphs,
    -- * Types
    CombinedNode(..),
    CombinedLink(..),
    combinedLinkType
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

import NetSpider.RPL.FindingID (FindingID(..), FindingType(..), IPv6ID, ipv6Only)
import NetSpider.RPL.DIO (DIONode, MergedDIOLink)
import NetSpider.RPL.DAO (DAONode, DAOLink)

data CombinedNode =
  CombinedNode
  { attrsDIO :: !(Maybe DIONode),
    attrsDAO :: !(Maybe DAONode)
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

data CombinedLink = CombinedDIOLink !MergedDIOLink
                  | CombinedDAOLink !DAOLink
                  deriving (Show,Eq,Ord)

instance Pan.ToAttributes CombinedLink where
  toAttributes (CombinedDIOLink ll) =
    ("link_type", "dio") : Pan.toAttributes ll
  toAttributes (CombinedDAOLink sl) =
    ("link_type", "dao") : Pan.toAttributes sl

combinedLinkType :: CombinedLink -> FindingType
combinedLinkType (CombinedDIOLink _) = FindingDIO
combinedLinkType (CombinedDAOLink _) = FindingDAO

combineNodes :: [SnapshotNode FindingID DIONode]
             -> [SnapshotNode FindingID DAONode]
             -> [SnapshotNode IPv6ID CombinedNode]
combineNodes dio_ns dao_ns = concatNodes $ map fromDIO dio_ns ++ map fromDAO dao_ns
  where
    fromDIO = bimap ipv6Only (\ln -> CombinedNode (Just ln) Nothing)
    fromDAO = bimap ipv6Only (\sn -> CombinedNode Nothing (Just sn))
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

combineLinks :: [SnapshotLink FindingID MergedDIOLink]
             -> [SnapshotLink FindingID DAOLink]
             -> [SnapshotLink IPv6ID CombinedLink]
combineLinks dio_ls dao_ls = map fromDIO dio_ls ++ map fromDAO dao_ls
  where
    fromDIO = bimap ipv6Only (\ll -> CombinedDIOLink ll)
    fromDAO = bimap ipv6Only (\sl -> CombinedDAOLink sl)

combineGraphs :: ([SnapshotNode FindingID DIONode], [SnapshotLink FindingID MergedDIOLink])
              -> ([SnapshotNode FindingID DAONode], [SnapshotLink FindingID DAOLink])
              -> ([SnapshotNode IPv6ID CombinedNode], [SnapshotLink IPv6ID CombinedLink])
combineGraphs (dio_ns, dio_ls) (dao_ns, dao_ls) =
  (combineNodes dio_ns dao_ns, combineLinks dio_ls dao_ls)
