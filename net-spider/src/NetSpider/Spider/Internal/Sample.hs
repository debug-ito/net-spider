-- |
-- Module: NetSpider.Spider.Internal.Sample
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Sample
       ( SnapshotLinkID(..),
         SnapshotLinkSample(..)
       ) where

import Data.Hashable (Hashable(hashWithSalt))

import NetSpider.Found (LinkState)
import NetSpider.Timestamp (Timestamp)


-- | Identitfy of link while making the snapshot graph.
--
-- 'SnapshotLinkID' is the unordered pair of nodes. 'Eq', 'Ord' and
-- 'Hashable' instances treat 'SnapshotLinkID's that have subject and
-- target nodes swapped as equivalent.
data SnapshotLinkID n =
  SnapshotLinkID
  { sliSubjectNode :: !n,
    sliTargetNode :: !n
  }
  deriving (Show)

sortedLinkID :: Ord n => SnapshotLinkID n -> (n, n)
sortedLinkID lid = if sn <= tn
                   then (sn, tn)
                   else (tn, sn)
  where
    sn = sliSubjectNode lid
    tn = sliTargetNode lid

instance Ord n => Eq (SnapshotLinkID n) where
  r == l = sortedLinkID r == sortedLinkID l

instance Ord n => Ord (SnapshotLinkID n) where
  compare r l = compare (sortedLinkID r) (sortedLinkID l)

instance (Ord n, Hashable n) => Hashable (SnapshotLinkID n) where
  hashWithSalt s lid = hashWithSalt s $ sortedLinkID lid


-- | Observation sample of a link while making the snapshot graph.
data SnapshotLinkSample n la =
  SnapshotLinkSample
  { slsLinkId :: !(SnapshotLinkID n),
    slsLinkState :: !LinkState,
    slsTimestamp :: !Timestamp,
    slsLinkAttributes :: !la
  }
  deriving (Show,Eq)

