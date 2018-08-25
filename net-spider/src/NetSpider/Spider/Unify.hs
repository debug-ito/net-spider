-- |
-- Module: NetSpider.Spider.Unify
-- Description: LinkSampleUnifier type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider.Unify
       ( -- * Types
         LinkSampleUnifier,
         SnapshotLinkSample(..),
         SnapshotLinkID(..),
         -- * Standard unifiers
         unifyToOne,
         unifyToMultiOn,
         -- * Building blocks
         latestSnapshotLinkSample,
         partitionByLinkAttributes
       ) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import GHC.Exts (groupWith)

import NetSpider.Snapshot (SnapshotNode)
import NetSpider.Spider.Internal.Sample
  ( SnapshotLinkSample(..), SnapshotLinkID(..)
  )

-- | Function to unify 'SnapshotLinkSample's collected for the given
-- pair of nodes and return 'SnapshotLinkSample' per physical
-- link. The returned 'SnapshotLinkSample's will be directly converted
-- to 'NetSpider.Snapshot.SnapshotLink's in the snapshot graph.
--
-- This function has a number of important roles during construction
-- of the snapshot graph.
--
-- - Because a link can be observed from both of its end nodes, there
--   can be multiple 'SnapshotLinkSample's for one physical link. This
--   function is supposed to return one reasonable link sample for the
--   physical link from those input link samples.
-- - There can be multiple physical links for a given pair of nodes,
--   but the 'Spider' has no way to distinguish them. So, this
--   function is supposed to distinguish 'SnapshotLinkSample's for
--   different physical links, and return multiple
--   'SnapshotLinkSample's, each of which corresponds to a physical
--   link.
-- - Sometimes a link is found by one end node but not found by the
--   other end node. Should 'Spider' treats the link is available or
--   not? This function is supposed to answer that question.
type LinkSampleUnifier n na la = SnapshotNode n na -> SnapshotNode n na -> [SnapshotLinkSample n la] -> [SnapshotLinkSample n la]

-- | Unify 'SnapshotLinkSamples's to one. This is the sensible unifier
-- if there is at most one physical link for a given pair of nodes.
unifyToOne :: LinkSampleUnifier n na la
unifyToOne _ _ samples = maybe [] return $ latestSnapshotLinkSample samples
  -- TODO: handle the case where the link is NOT observed by one of the end nodes.

-- | Unify 'SnapshotLinkSample's to possibly multiple samples. The
-- input samples are partitioned to groups based on the link sub-ID,
-- defined by the given getter function. Each group represents one of
-- the final samples.
unifyToMultiOn :: Ord b
               => (la -> b) -- ^ Getter of the link sub-ID
               -> LinkSampleUnifier n na la
unifyToMultiOn getKey lnode rnode samples =
  concat $ map (unifyToOne lnode rnode) $ partitionByLinkAttributes getKey samples


-- | Get the 'SnapshotLinkSample' that has the latest (biggest)
-- timestamp.
latestSnapshotLinkSample :: [SnapshotLinkSample n la] -> Maybe (SnapshotLinkSample n la)
latestSnapshotLinkSample [] = Nothing
latestSnapshotLinkSample samples = Just $ maximumBy comp samples
  where
    comp = compare `on` slsTimestamp

-- | Partition 'SnapshotLinkSample's using their link
-- attributes. Partitions are defined based on the link sub-ID, which
-- is defined by the given getter function.
partitionByLinkAttributes :: Ord b
                          => (la -> b) -- ^ Getter of the link sub-ID
                          -> [SnapshotLinkSample n la]
                          -> [[SnapshotLinkSample n la]]
partitionByLinkAttributes getKey = groupWith (getKey . slsLinkAttributes)
