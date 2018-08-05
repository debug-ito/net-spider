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
-- - type @la@: link attributes.
data SnapshotLink n la =
  SnapshotLink
  { _sourceNode :: !n,
    _destinationNode :: !n,
    _isDirected :: !Bool,
    _linkTimestamp :: !Timestamp

    -- TODO: add link attributes
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq)

-- | Comparison by node-tuple (source node, destination node).
instance (Ord n) => Ord (SnapshotLink n la) where
  compare l r = compare (linkNodeTuple l) (linkNodeTuple r)

-- | node-tuple (source node, destination node) of the link.
linkNodeTuple :: SnapshotLink n la -> (n, n)
linkNodeTuple link = (_sourceNode link, _destinationNode link)

-- | A node observed at a specific time.
data SnapshotNode n =
  SnapshotNode
  { _nodeId :: !n,
    _isOnBoundary :: !Bool
    
    -- TODO: add node attributes?

    -- TODO: maybe node should have timestamp? because node can have no links.
  }
  deriving (Show,Eq)

-- | Comparison by node ID.
instance Ord n => Ord (SnapshotNode n) where
  compare l r = compare (_nodeId l) (_nodeId r)

type SnapshotElement n la = Either (SnapshotNode n) (SnapshotLink n la)
