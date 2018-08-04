-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
module NetSpider.Snapshot.Internal
       ( SnapshotLink(..),
         linkTuple,
         SnapshotNode(..),
         SnapshotElement
       ) where

import NetSpider.Timestamp (Timestamp)

-- data ObservedLink n p =
--   ObservedLink
--   { observeTime :: Timestamp,
--     observeSubjectNode :: n,
--     observeFoundLink :: FoundLink n p
--   }

-- | A link observed at a specific time.
--
-- Basically 'SnapshotLink' is summary of one or more link
-- observations by different subject nodes. It is possible that those
-- observations contradict each other.
--
-- - type @n@: node ID.
-- - type @p@: port ID.
data SnapshotLink n p =
  SnapshotLink
  { _sourceNode :: !n,
    _sourcePort :: !p,
    _destinationNode :: !n,
    _destinationPort :: !p,
    _isDirected :: !Bool,
    _linkTimestamp :: !Timestamp
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq)

-- | Comparison by 4-tuple (source node, destination node, source
-- port, destination port).
instance (Ord n, Ord p) => Ord (SnapshotLink n p) where
  compare l r = compare (linkTuple l) (linkTuple r)

-- TODO: should linkTuple be (source node, source port, des node, des port) ???

-- | 4-tuple (source node, destination node, source port, destination
-- port) of the link.
linkTuple :: SnapshotLink n p -> (n, n, p, p)
linkTuple link = (_sourceNode link, _destinationNode link, _sourcePort link, _destinationPort link)

-- | A node observed at a specific time.
data SnapshotNode n =
  SnapshotNode
  { _nodeId :: !n,
    _isOnBoundary :: !Bool
    
    -- node attributes?

    -- TODO: maybe node should have timestamp? because node can have no links.
  }
  deriving (Show,Eq)

-- | Comparison by node ID.
instance Ord n => Ord (SnapshotNode n) where
  compare l r = compare (_nodeId l) (_nodeId r)

type SnapshotElement n p = Either (SnapshotNode n) (SnapshotLink n p)
