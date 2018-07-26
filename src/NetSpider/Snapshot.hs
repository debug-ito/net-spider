-- |
-- Module: NetSpider.Snapshot
-- Description: Types about snapshot graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Snapshot
       ( SnapshotNode(..),
         SnapshotLink(..),
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
  { sourceNode :: n,
    sourcePort :: p,
    destinationNode :: n,
    destinationPort :: p,
    isDirected :: Bool,
    linkTimestamp :: Timestamp
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq)

-- | A node observed at a specific time.
data SnapshotNode n =
  SnapshotNode
  { nodeId :: n
    
    -- node attributes?
  }
  deriving (Show,Eq)


type SnapshotElement n p = Either (SnapshotNode n) (SnapshotLink n p)
