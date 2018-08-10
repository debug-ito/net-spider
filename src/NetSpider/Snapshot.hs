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
         linkAttributes,
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


nodeId :: SnapshotNode n na -> n
nodeId = _nodeId

-- | This property is 'True' if the node is on the boundary of the
-- query. This means that nodes adjacent to this node may not be
-- included in the query result.
isOnBoundary :: SnapshotNode n na -> Bool
isOnBoundary = _isOnBoundary

sourceNode :: SnapshotLink n la -> n
sourceNode = _sourceNode

destinationNode :: SnapshotLink n la -> n
destinationNode = _destinationNode

isDirected :: SnapshotLink n la -> Bool
isDirected = _isDirected

linkTimestamp :: SnapshotLink n la -> Timestamp
linkTimestamp = _linkTimestamp

linkAttributes :: SnapshotLink n la -> la
linkAttributes = _linkAttributes
