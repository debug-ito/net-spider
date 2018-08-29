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
         unifyToMany,
         unifyStd,
         UnifyStdConfig(..),
         defUnifyStdConfig,
         -- * Building blocks
         latestSnapshotLinkSample,
         partitionByLinkAttributes,
         removeByNegativeFinding
       ) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import GHC.Exts (groupWith)

import NetSpider.Snapshot (SnapshotNode, nodeTimestamp, nodeId, SnapshotLink)
import NetSpider.Spider.Internal.Sample
  ( SnapshotLinkSample(..), SnapshotLinkID(..)
  )

-- | Function to unify 'SnapshotLinkSample's collected for the given
-- pair of nodes and return 'SnapshotLinkSample' per physical
-- link. The returned 'SnapshotLinkSample's will be directly converted
-- to 'SnapshotLink's in the snapshot graph.
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
--   different physical links, and return one or more
--   'SnapshotLinkSample's, each of which corresponds to a physical
--   link.
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
type LinkSampleUnifier n na fla sla = SnapshotNode n na -> SnapshotNode n na -> [SnapshotLinkSample n fla] -> [SnapshotLinkSample n sla]

-- | Unify 'SnapshotLinkSamples's to one. This is the sensible unifier
-- if there is at most one physical link for a given pair of nodes.
unifyToOne :: Eq n => LinkSampleUnifier n na la la
unifyToOne = unifyStd defUnifyStdConfig

-- | Unify 'SnapshotLinkSample's to possibly multiple samples. The
-- input samples are partitioned to groups based on the link sub-ID,
-- defined by the given getter function. Each group represents one of
-- the final samples.
unifyToMany :: (Eq n, Ord lsid)
            => (SnapshotLinkSample n fla -> lsid) -- ^ Getter of the link sub-ID
            -> LinkSampleUnifier n na fla fla
unifyToMany getKey = unifyStd conf
  where
    conf = defUnifyStdConfig { makeLinkSubId = getKey }

-- | TODO: write doc
data UnifyStdConfig n na fla sla lsid =
  UnifyStdConfig
  { makeLinkSubId :: SnapshotLinkSample n fla -> lsid,
    mergeSamples :: [SnapshotLinkSample n fla] -> [SnapshotLinkSample n fla] -> Maybe (SnapshotLinkSample n sla),
    isLinkDetectable :: SnapshotNode n na -> SnapshotLinkSample n sla -> Bool
  }

defUnifyStdConfig :: UnifyStdConfig n na fla fla ()
defUnifyStdConfig = UnifyStdConfig
                    { makeLinkSubId = const (),
                      mergeSamples = \ls rs -> latestSnapshotLinkSample (ls ++ rs),
                      isLinkDetectable = \_ _ -> True
                    }

-- | TODO: write doc.
unifyStd :: (Eq n, Ord lsid) => UnifyStdConfig n na fla sla lsid -> LinkSampleUnifier n na fla sla
unifyStd conf lnode rnode = mapMaybe forGroup . groupWith (makeLinkSubId conf)
  where
    forGroup samples = maybeNegates rnode
                       =<< maybeNegates lnode
                       =<< mergeSamples conf (samplesFor samples lnode) (samplesFor samples rnode)
    samplesFor samples sn = filter (\s -> nodeId sn == (sliSubjectNode $ slsLinkId s)) samples
    maybeNegates sn sample = if (isLinkDetectable conf sn sample) && (negatesLinkSample sn sample)
                             then Nothing
                             else Just sample

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

-- | Remove 'SnapshotLinkSample's that are negated by the given
-- 'SnapshotNode'.
--
-- This function is effective if 'SnapshotNode' has
-- 'nodeTimestamp'. If 'nodeTimestamp' is 'Just', this function
-- removes 'SnapshotLinkSample's whose timestamp is lower than the
-- 'nodeTimestamp'. Those 'SnapshotLinkSample's are removed because
-- the 'SnapshotNode' indicates that the link already disappears.
removeByNegativeFinding :: SnapshotNode n na -> [SnapshotLinkSample n la] -> [SnapshotLinkSample n la]
removeByNegativeFinding sn = filter (not . negatesLinkSample sn)

negatesLinkSample :: SnapshotNode n na -> SnapshotLinkSample n la -> Bool
negatesLinkSample sn l =
  case nodeTimestamp sn of
   Nothing -> False
   Just t -> slsTimestamp l < t
  
