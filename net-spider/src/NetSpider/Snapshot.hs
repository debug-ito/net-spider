-- |
-- Module: NetSpider.Snapshot
-- Description: Types about snapshot graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- A snapshot graph is a graph constructed from the NetSpider
-- database. It reprensents a graph at specific time.
module NetSpider.Snapshot
       ( -- * SnapshotGraph
         SnapshotGraph,
         graphTimestamp,
         -- * SnapshotNode
         SnapshotNode,
         nodeId,
         isOnBoundary,
         nodeTimestamp,
         nodeAttributes,
         -- * SnapshotLink
         SnapshotLink,
         sourceNode,
         destinationNode,
         linkNodeTuple,
         linkNodePair,
         isDirected,
         linkTimestamp,
         linkAttributes
       ) where

import Data.Maybe (catMaybes)

import NetSpider.Snapshot.Internal
  ( SnapshotGraph,
    SnapshotNode(..),
    SnapshotLink(..),
    linkNodeTuple,
    linkNodePair
  )
import NetSpider.Timestamp (Timestamp)

-- | Get the timestamp of the graph. It's the latest timestamp of the
-- nodes and links.
--
-- @since 0.4.3.0
graphTimestamp :: SnapshotGraph n na la -> Maybe Timestamp
graphTimestamp (nodes, links) = maximum' (node_times ++ link_times)
  where
    maximum' [] = Nothing
    maximum' ts = Just $ maximum ts
    node_times = catMaybes $ map nodeTimestamp nodes
    link_times = map linkTimestamp links

nodeId :: SnapshotNode n na -> n
nodeId = _nodeId

-- | This property is 'True' if the node is on the boundary of the
-- query. This means that nodes adjacent to this node may not be
-- included in the query result.
isOnBoundary :: SnapshotNode n na -> Bool
isOnBoundary = _isOnBoundary

-- | If the node is not observed yet or 'isOnBoundary' is 'True', its
-- timestamp is 'Nothing'.
nodeTimestamp :: SnapshotNode n na -> Maybe Timestamp
nodeTimestamp = _nodeTimestamp

-- | If the node is not observed yet or 'isOnBoundary' is 'True', its
-- node attributes is 'Nothing'.
nodeAttributes :: SnapshotNode n na -> Maybe na
nodeAttributes = _nodeAttributes

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
