{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: NetSpider.Weaver
-- Description: On-memory builder for snapshot graphs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- @since 0.4.2.0
module NetSpider.Weaver
  ( -- * Type
    Weaver,
    -- * Construction
    newWeaver,
    -- * Add FoundNode
    addFoundNode,
    markAsVisited,
    -- * Query
    getSnapshot,
    getSnapshot',
    isVisited,
    getVisitedNodes,
    getBoundaryNodes
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sort, reverse)
import Data.Maybe (listToMaybe)

import NetSpider.Found (FoundNode(..))
import NetSpider.Log (LogLine)
import NetSpider.Query.Internal (FoundNodePolicy(..))
import NetSpider.Query (policyOverwrite, policyAppend)
import NetSpider.Snapshot.Internal (SnapshotGraph, SnapshotNode(..))
import NetSpider.Timestamp (Timestamp)
import NetSpider.Unify (LinkSampleUnifier)

-- | 'Weaver' is an on-memory builder for snapshot graphs. It builds a
-- 'SnapshotGraph' from 'FoundNode's without using an external graph
-- database.
data Weaver n na la =
  Weaver
  { visitedNodes :: HashMap n [FoundNode n na la],
    -- ^ Node IDs for visited nodes are kept as the key. The value is
    -- empty if there is no observation for that visited node.
    foundNodePolicy :: FoundNodePolicy n na
    -- ^ Policy to maintain the value of 'visitedNodes'.
  }
  deriving (Show,Eq)

-- | Make a new 'Weaver'.
--
-- The 'FoundNodePolicy' controls the behavior of 'newWeaver'. If it's
-- 'policyOverwrite', 'Weaver' maintains only the 'FoundNode' with the
-- latest timestamp for each node. If it's 'policyAppend', 'Weaver'
-- maintains all 'FoundNode's added.
newWeaver :: FoundNodePolicy n na -> Weaver n na la
newWeaver p = Weaver HM.empty p

-- | Add a 'FoundNode' to the 'Weaver'. See also 'newWeaver'.
addFoundNode :: (Eq n, Hashable n) => FoundNode n na la -> Weaver n na la -> Weaver n na la
addFoundNode fn w = w { visitedNodes = HM.insertWith updater nid [fn] $ visitedNodes w }
  where
    nid = subjectNode fn
    updater =
      case foundNodePolicy w of
        PolicyOverwrite -> \new old -> if latestTimeOfNodes new >= latestTimeOfNodes old
                                       then new
                                       else old
        PolicyAppend -> \new old -> new ++ old
    latestTimeOfNodes ns = listToMaybe $ reverse $ sort $ map foundAt ns

-- | Mark the node ID as visited in the 'Weaver' without any
-- 'FoundNode'. If there is already some 'FoundNode' for the node ID,
-- this function does nothing.
markAsVisited :: (Eq n, Hashable n) => n -> Weaver n na la -> Weaver n na la
markAsVisited nid w = w { visitedNodes = HM.insertWith updater nid [] $ visitedNodes w }
  where
    updater _ old = old

-- | Returns 'True' if the node ID is already visited in the 'Weaver'.
isVisited :: (Eq n, Hashable n) => n -> Weaver n na la -> Bool
isVisited n w = HM.member n (visitedNodes w)

-- | Get the visited 'FoundNode's for the given node ID kept in
-- 'Weaver'. It returns 'Nothing' if the node ID is not visited.
getVisitedNodes :: (Eq n, Hashable n) => n -> Weaver n na la -> Maybe [FoundNode n na la]
getVisitedNodes n w = HM.lookup n (visitedNodes w)

-- | Make 'SnapshotGraph' from the current 'Weaver'.
getSnapshot :: (Eq n, Hashable n) => LinkSampleUnifier n na fla sla -> Weaver n na fla -> SnapshotGraph n na sla
getSnapshot u w = fst $ getSnapshot' u w

-- | Get boundary nodes from the 'Weaver'. A boundary node is a node
-- that has been observed as a target of some links but not visited
-- yet.
getBoundaryNodes :: Weaver n na fla -> [n]
getBoundaryNodes = undefined -- TODO

latestFoundNodeFor :: (Eq n, Hashable n) => n -> Weaver n na fla -> Maybe (FoundNode n na fla)
latestFoundNodeFor = undefined -- TODO

timestampFor :: (Eq n, Hashable n) => n -> Weaver n na fla -> Maybe Timestamp
timestampFor n w = fmap foundAt $ latestFoundNodeFor n w

nodeAttributesFor :: (Eq n, Hashable n) => n -> Weaver n na fla -> Maybe na
nodeAttributesFor n w = fmap nodeAttributes $ latestFoundNodeFor n w

makeSnapshotNode :: (Eq n, Hashable n) => Weaver n na fla -> n -> SnapshotNode n na
makeSnapshotNode weaver nid =
  SnapshotNode { _nodeId = nid,
                 _isOnBoundary = not $ isVisited nid weaver,
                 _nodeTimestamp = timestampFor nid weaver,
                 _nodeAttributes = nodeAttributesFor nid weaver
               }

-- | Same as 'getSnapshot', but it also returns logs.
getSnapshot' :: (Eq n, Hashable n)
             => LinkSampleUnifier n na fla sla
             -> Weaver n na fla
             -> (SnapshotGraph n na sla, [LogLine])
getSnapshot' unifier weaver = ((nodes, links), logs)
  where
    nodes = visited_nodes ++ boundary_nodes
    visited_nodes = map (makeSnapshotNode weaver) $ HM.keys $ visitedNodes weaver
    boundary_nodes = map (makeSnapshotNode weaver) $ getBoundaryNodes weaver
    links = undefined -- TODO
    logs = undefined -- TODO
