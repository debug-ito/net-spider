-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
module NetSpider.Snapshot.Internal
       ( SnapshotLink(..),
         linkNodeTuple,
         SnapshotNode(..),
         SnapshotElement
       ) where

import NetSpider.Timestamp (Timestamp)

-- | A link in the snapshot graph.
--
-- Basically 'SnapshotLink' is summary of one or more link
-- observations by different subject nodes. It is possible that those
-- observations contradict each other.
--
-- - type @n@: node ID.
-- - type @la@: link attributes.
data SnapshotLink n la =
  SnapshotLink
  { _sourceNode :: !n,
    _destinationNode :: !n,
    _isDirected :: !Bool,
    _linkTimestamp :: !Timestamp,
    _linkAttributes :: !la
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq)

-- | Comparison by node-tuple (source node, destination node).
instance (Ord n, Eq la) => Ord (SnapshotLink n la) where
  compare l r = compare (linkNodeTuple l) (linkNodeTuple r)

-- | node-tuple (source node, destination node) of the link.
linkNodeTuple :: SnapshotLink n la -> (n, n)
linkNodeTuple link = (_sourceNode link, _destinationNode link)

-- | A node in the snapshot graph.
data SnapshotNode n na =
  SnapshotNode
  { _nodeId :: !n,
    _isOnBoundary :: !Bool,
    _nodeAttributes :: !(Maybe na)
    -- ^ If a node is found, but no observation is done for it, its
    -- node attributes is 'Nothing'.
    
    -- TODO: maybe node should have timestamp? because node can have no links.
  }
  deriving (Show,Eq)

-- | Comparison by node ID.
instance (Ord n, Eq na) => Ord (SnapshotNode n na) where
  compare l r = compare (_nodeId l) (_nodeId r)

type SnapshotElement n la na = Either (SnapshotNode n na) (SnapshotLink n la)
