-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
module NetSpider.Snapshot.Internal
       ( SnapshotLink(..),
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
  { _sourceNode :: n,
    _sourcePort :: p,
    _destinationNode :: n,
    _destinationPort :: p,
    _isDirected :: Bool,
    _linkTimestamp :: Timestamp
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq)

-- | A node observed at a specific time.
data SnapshotNode n =
  SnapshotNode
  { _nodeId :: n
    
    -- node attributes?
  }
  deriving (Show,Eq)


type SnapshotElement n p = Either (SnapshotNode n) (SnapshotLink n p)

