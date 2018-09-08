-- |
-- Module: NetSpider.Unify
-- Description: LinkSampleUnifier type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Unify
       ( -- * Types
         LinkSampleUnifier,
         LinkSample(..),
         LinkSampleID,
         linkSampleId,
         -- * Standard unifiers
         unifyToOne,
         unifyToMany,
         unifyStd,
         UnifyStdConfig(..),
         defUnifyStdConfig,
         -- * Building blocks
         latestLinkSample,
         defNegatesLinkSample
       ) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Maybe (mapMaybe)
import GHC.Exts (groupWith)

import NetSpider.Found (FoundLink, LinkState)
import NetSpider.Pair (Pair(..))
import NetSpider.Snapshot (SnapshotNode, nodeTimestamp, nodeId, SnapshotLink)
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
type LinkSampleID n = Pair n

-- | Get 'LinkSampleID' of the 'LinkSample'.
linkSampleId :: LinkSample n la -> LinkSampleID n
linkSampleId l = Pair (lsSubjectNode l, lsTargetNode l)



-- | Function to unify 'LinkSample's collected for the given pair of
-- nodes and return 'LinkSample' per physical link. The returned
-- 'LinkSample's will be directly converted to 'SnapshotLink's in the
-- snapshot graph.
--
-- This function has a number of important roles during construction
-- of the snapshot graph.
--
-- - Because a link can be observed from both of its end nodes, there
--   can be multiple 'LinkSample's for one physical link. This
--   function is supposed to return one reasonable link sample for the
--   physical link from those input link samples.
-- - There can be multiple physical links for a given pair of nodes,
--   but the 'Spider' has no way to distinguish them. So, this
--   function is supposed to distinguish 'LinkSample's for different
--   physical links, and return one or more 'LinkSample's, each of
--   which corresponds to a physical link.
-- - Sometimes a link is found by one end node but not found by the
--   other end node. Should 'Spider' treats the link is available or
--   not? This function is supposed to answer that question by
--   returning non-empty result (if the link is available) or empty
--   result (if the link is not available.)
-- - Sometimes it is natural to have different data models of link
--   attributes for 'FoundLink's (@fla@) and for 'SnapshotLink's
--   (@sla@). For example, when you want to combine link attributes
--   obtained from both of the end nodes to make the link attributes
--   of 'SnapshotLink'. This function is supposed to convert the link
--   attribute type.
type LinkSampleUnifier n na fla sla = SnapshotNode n na -> SnapshotNode n na -> [LinkSample n fla] -> [LinkSample n sla]

-- | Unify 'LinkSample's to one. This is the sensible unifier if there
-- is at most one physical link for a given pair of nodes.
unifyToOne :: Eq n => LinkSampleUnifier n na la la
unifyToOne = unifyStd defUnifyStdConfig

-- | Unify 'LinkSample's to possibly multiple samples. The input
-- samples are partitioned to groups based on the link sub-ID, defined
-- by the given getter function. Each group represents one of the
-- final samples.
unifyToMany :: (Eq n, Ord lsid)
            => (LinkSample n fla -> lsid) -- ^ Getter of the link sub-ID
            -> LinkSampleUnifier n na fla fla
unifyToMany getKey = unifyStd conf
  where
    conf = defUnifyStdConfig { makeLinkSubId = getKey }

-- | Configuration of 'unifyStd'. See 'unifyStd' for detail.
data UnifyStdConfig n na fla sla lsid =
  UnifyStdConfig
  { makeLinkSubId :: LinkSample n fla -> lsid,
    -- ^ Function to create the link sub-ID from
    -- 'LinkSample'.
    --
    -- Default: always return @()@. All 'LinkSample's have the same
    -- link sub-ID.
    mergeSamples :: [LinkSample n fla] -> [LinkSample n fla] -> Maybe (LinkSample n sla),
    -- ^ Function to merge 'LinkSample's for a physical link. The
    -- input is 'LinkSample's found by each of the end nodes. This
    -- function optionally converts the link attributes from @fla@ to
    -- @sla@.
    --
    -- Default: Concatenate the input 'LinkSample's and get
    -- 'latestLinkSample'.

    negatesLinkSample :: SnapshotNode n na -> LinkSample n sla -> Bool
    -- ^ This function is supposed to return 'True' if the
    -- 'SnapshotNode' negates the presence of the 'LinkSample'.
    --
    -- Default: 'defNegatesLinkSample'.
  }

-- | Default of 'UnifyStdConfig'.
defUnifyStdConfig :: Eq n => UnifyStdConfig n na fla fla ()
defUnifyStdConfig = UnifyStdConfig
                    { makeLinkSubId = const (),
                      mergeSamples = \ls rs -> latestLinkSample (ls ++ rs),
                      negatesLinkSample = defNegatesLinkSample
                    }

-- | The standard unifier. This unifier does the following.
--
-- 1. It partitions 'LinkSample's based on their link sub-IDs. The
--    link sub-ID is defined by 'makeLinkSubId' function. Each link
--    sub-ID corresponds to a physical link.
-- 2. For each partition, 'LinkSample's are merged to one using
--    'mergeSamples' function.
-- 3. After merge, the link is checked against its end nodes. If
--    'negatesLinkSample' returns 'True' for either of the end nodes,
--    the link is removed from the final result.
unifyStd :: (Eq n, Ord lsid) => UnifyStdConfig n na fla sla lsid -> LinkSampleUnifier n na fla sla
unifyStd conf lnode rnode = mapMaybe forGroup . groupWith (makeLinkSubId conf)
  where
    forGroup samples = maybeNegates rnode
                       =<< maybeNegates lnode
                       =<< mergeSamples conf (samplesFor samples lnode) (samplesFor samples rnode)
    samplesFor samples sn = filter (\s -> nodeId sn == (lsSubjectNode s)) samples
    maybeNegates sn sample = if negatesLinkSample conf sn sample
                             then Nothing
                             else Just sample

-- | Get the 'LinkSample' that has the latest (biggest) timestamp.
latestLinkSample :: [LinkSample n la] -> Maybe (LinkSample n la)
latestLinkSample [] = Nothing
latestLinkSample samples = Just $ maximumBy comp samples
  where
    comp = compare `on` lsTimestamp

-- | Default of 'negatesLinkSample'. This function returns 'True' if
-- all of the following conditions are met.
--
-- - The 'SnapshotNode' has 'nodeTimestamp'.
-- - The 'nodeTimestamp' is greater (newer) than the link's timestamp.
-- - The 'lsSubjectNode' is not the node ID of the 'SnapshotNode'.
--
-- If the above conditions are met, it implies that the 'LinkSample'
-- is found by the other end node, but the given 'SnapshotNode' does
-- not find it. This is possibly because the link has just
-- disappeared, so the link should be negated.
defNegatesLinkSample :: Eq n => SnapshotNode n na -> LinkSample n la -> Bool
defNegatesLinkSample sn l =
  case nodeTimestamp sn of
   Nothing -> False
   Just t -> lsTimestamp l < t && lsSubjectNode l /= nodeId sn
