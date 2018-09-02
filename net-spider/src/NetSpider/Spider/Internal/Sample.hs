-- |
-- Module: NetSpider.Spider.Internal.Sample
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Sample
       ( LinkSample(..),
         linkSampleId
       ) where

import Data.Hashable (Hashable(hashWithSalt))

import NetSpider.Found (LinkState)
import NetSpider.Pair (Pair(..))
import NetSpider.Timestamp (Timestamp)

-- | 'LinkSample' is an intermediate type between
-- 'NetSpider.Found.FoundLink' and
-- 'NetSpider.Snapshot.SnapshotLink'. 'LinkSample's are collected from
-- the history graph, and are unified into
-- 'NetSpider.Snapshot.SnapshotLink's.
data LinkSample n la =
  LinkSample
  { lsSubjectNode :: !n,
    lsTargetNode :: !n,
    lsLinkState :: !LinkState,
    lsTimestamp :: !Timestamp,
    lsLinkAttributes :: !la
  }
  deriving (Show,Eq,Ord)

-- | Link ID of the 'LinkSample'. It's the 'Pair' of 'lsSubjectNode'
-- and 'lsTargetNode'.
linkSampleId :: LinkSample n la -> Pair n
linkSampleId l = Pair (lsSubjectNode l, lsTargetNode l)
