{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Internal.Type
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Type
       ( Spider(..),
         Config(..),
         defConfig,
         subgroupByLinkAttributes,
         SnapshotLinkID(..),
         SnapshotLinkSample(..)
       ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Greskell (Key)
import GHC.Exts (groupWith)
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Found (LinkState(..))
import NetSpider.Graph (VNode)
import NetSpider.Snapshot (SnapshotNode)
import NetSpider.Timestamp (Timestamp)

-- | An IO agent of the NetSpider database.
--
-- - type @n@: node ID.
-- - type @na@: node attributes
-- - type @la@: link attributes
data Spider n na la =
  Spider
  { spiderConfig :: Config n na la,
    spiderClient :: Gr.Client
  }

-- | Configuration to create a 'Spider' object.
data Config n na la =
  Config
  { wsHost :: Gr.Host,
    -- ^ Host of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: \"localhost\".
    wsPort :: Gr.Port,
    -- ^ Port of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: 8182
    nodeIdKey :: Key VNode n,
    -- ^ Name of graph property that stores the node ID. Default:
    -- \"@node_id\".
    unifyLinkSamples :: SnapshotNode n na -> SnapshotNode n na -> [SnapshotLinkSample n la] -> [SnapshotLinkSample n la]
    -- ^ The function to unify 'SnapshotLinkSample's collected for the
    -- given pair of nodes and return 'SnapshotLinkSample' per
    -- physical link. The returned 'SnapshotLinkSample's will be
    -- directly converted to 'NetSpider.Snapshot.SnapshotLink's in the
    -- snapshot graph.
    --
    -- This function has a number of important roles during
    -- construction of the snapshot graph.
    --
    -- - Because a link can be observed from both of its end nodes,
    --   there can be multiple 'SnapshotLinkSample's for one physical
    --   link. This function is supposed to return one reasonable link
    --   sample for the physical link from those input link samples.
    -- - There can be multiple physical links for a given pair of
    --   nodes, but the 'Spider' has no way to distinguish them. So,
    --   this function is supposed to distinguish
    --   'SnapshotLinkSample's for different physical links, and
    --   return multiple 'SnapshotLinkSample's, each of which
    --   corresponds to a physical link.
    -- - Sometimes a link is found by one end node but not found by
    --   the other end node. Should 'Spider' treats the link is
    --   available or not? This function is supposed to answer that
    --   question.
    --
    -- TODO: Show default. Maybe this whole documentation should be
    -- moved to another module..
  }

defConfig :: Config n na la
defConfig =
  Config
  { wsHost = "localhost",
    wsPort = 8182,
    nodeIdKey = "@node_id",
    subgroupSnapshotLinkSamples = \s -> return s
  }

-- | Partition 'SnapshotLinkSample's using their link attributes. You
-- can use this function for 'subgroupSnapshotLinkSamples' config
-- field.
subgroupByLinkAttributes :: Ord b => (la -> b) -> [SnapshotLinkSample n la] -> [[SnapshotLinkSample n la]]
subgroupByLinkAttributes getKey = groupWith (getKey . slsLinkAttributes)

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

