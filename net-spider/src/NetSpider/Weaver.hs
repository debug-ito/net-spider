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
    isVisited,
    getVisitedNodes,
    getSnapshot
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import NetSpider.Found (FoundNode(..))
import NetSpider.Query.Internal (FoundNodePolicy(..))
import NetSpider.Query (policyOverwrite, policyAppend)
import NetSpider.Snapshot.Internal (SnapshotGraph)
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
addFoundNode = undefined -- TODO

-- | Mark the node ID as visited in the 'Weaver' without any
-- 'FoundNode'. If there is already some 'FoundNode' for the node ID,
-- this function does nothing.
markAsVisited :: (Eq n, Hashable n) => n -> Weaver n na la -> Weaver n na la
markAsVisited = undefined -- TODO

-- | Returns 'True' if the node ID is already visited in the 'Weaver'.
isVisited :: (Eq n, Hashable n) => Weaver n na la -> n -> Bool
isVisited w n = HM.member n (visitedNodes w)

-- | Get the visited 'FoundNode's for the given node ID kept in
-- 'Weaver'. It returns 'Nothing' if the node ID is not visited.
getVisitedNodes :: (Eq n, Hashable n) => Weaver n na la -> n -> Maybe [FoundNode n na la]
getVisitedNodes w n = HM.lookup n (visitedNodes w)

-- | Make 'SnapshotGraph' from the current 'Weaver'.
getSnapshot :: LinkSampleUnifier n na fla sla -> Weaver n na fla -> SnapshotGraph n na sla
getSnapshot = undefined -- TODO
