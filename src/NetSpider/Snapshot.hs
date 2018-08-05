-- |
-- Module: NetSpider.Snapshot
-- Description: Types about snapshot graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Snapshot
       ( -- * SnapshotNode
         SnapshotNode,
         nodeId,
         isOnBoundary,
         -- * SnapshotLink
         SnapshotLink,
         sourceNode,
         destinationNode,
         linkNodeTuple,
         isDirected,
         linkTimestamp,
         -- * SnapshotElement
         SnapshotElement
       ) where

import NetSpider.Snapshot.Internal
  ( SnapshotNode(..),
    SnapshotLink(..),
    linkNodeTuple,
    SnapshotElement
  )
import NetSpider.Timestamp (Timestamp)


nodeId :: SnapshotNode n -> n
nodeId = _nodeId

-- | This property is 'True' if the node is on the boundary of the
-- query. This means that nodes adjacent to this node may not be
-- included in the query result.
isOnBoundary :: SnapshotNode n -> Bool
isOnBoundary = _isOnBoundary

sourceNode :: SnapshotLink n p -> n
sourceNode = _sourceNode

destinationNode :: SnapshotLink n p -> n
destinationNode = _destinationNode

isDirected :: SnapshotLink n p -> Bool
isDirected = _isDirected

linkTimestamp :: SnapshotLink n p -> Timestamp
linkTimestamp = _linkTimestamp
