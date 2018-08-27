-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
module NetSpider.Snapshot.Internal
       ( SnapshotLink(..),
         linkNodeTuple,
         sortLinkNodeTuple,
         SnapshotNode(..),
         SnapshotElement
       ) where

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

-- | If the link is not directed, sort its (source node, destination
-- node) pair. If the link is directed, this function does nothing.
sortLinkNodeTuple :: Ord n => SnapshotLink n la -> SnapshotLink n la
sortLinkNodeTuple l =
  if _isDirected l
  then l
  else l { _sourceNode = new_source,
           _destinationNode = new_dest
         }
  where
    old_source = _sourceNode l
    old_dest = _destinationNode l
    (new_source, new_dest) = if old_source <= old_dest
                             then (old_source, old_dest)
                             else (old_dest, old_source)
                           

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

type SnapshotElement n na la = Either (SnapshotNode n na) (SnapshotLink n la)
