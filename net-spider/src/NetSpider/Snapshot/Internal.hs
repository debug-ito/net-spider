-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: Implementation of Snapshot graph types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
--
-- Implementation of Snapshot graph types. This module is for internal
-- and testing purposes only.
module NetSpider.Snapshot.Internal
       ( SnapshotLink(..),
         linkNodeTuple,
         linkNodePair,
         SnapshotNode(..)
       ) where

import NetSpider.Pair (Pair(..))
import NetSpider.Timestamp (Timestamp)

-- | A link in the snapshot graph.
--
-- 'SnapshotLink' is summary of one or more link observations by
-- different subject nodes. Basically the latest of these observations
-- is used to make 'SnapshotLink'.
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

-- | Node-tuple (source node, destination node) of the link.
linkNodeTuple :: SnapshotLink n la -> (n, n)
linkNodeTuple link = (_sourceNode link, _destinationNode link)

-- | Like 'linkNodeTuple', but this returns a 'Pair'.
linkNodePair :: SnapshotLink n la -> Pair n
linkNodePair = Pair . linkNodeTuple

-- | A node in the snapshot graph.
data SnapshotNode n na =
  SnapshotNode
  { _nodeId :: !n,
    _isOnBoundary :: !Bool,
    _nodeTimestamp :: !(Maybe Timestamp),
    _nodeAttributes :: !(Maybe na)
  }
  deriving (Show,Eq)

-- | Comparison by node ID.
instance (Ord n, Eq na) => Ord (SnapshotNode n na) where
  compare l r = compare (_nodeId l) (_nodeId r)
