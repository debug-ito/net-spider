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
         SnapshotLinkID(..),
         SnapshotLinkSample(..)
       ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Greskell (Key)
import Data.Vector (Vector)
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Found (LinkState(..))
import NetSpider.Graph (VNode)
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
    subgroupSnapshotLinkSamples :: Vector (SnapshotLinkSample n la) -> Vector (Vector (SnapshotLinkSample n la))
    -- ^ Function to partition 'SnapshotLinkSample's into
    -- subgroups. This functions is used by the 'Spider' when it makes
    -- the snapshot graph.
    --
    -- The input is the 'SnapshotLinkSample's collected for a single
    -- pair of nodes. The function is supposed to partition them into
    -- subgroups, where each subgroup represents a physical link. If
    -- your network can have multiple physical links between the same
    -- pair of nodes, you should customize this function.
    --
    -- Default: just return the input as the only subgroup.
  }

defConfig :: Config n na la
defConfig =
  Config
  { wsHost = "localhost",
    wsPort = 8182,
    nodeIdKey = "@node_id",
    subgroupSnapshotLinkSamples = \s -> return s
  }


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

