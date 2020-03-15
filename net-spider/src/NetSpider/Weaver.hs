{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
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
    getBoundaryNodes,
    -- * Misc.
    visitAllBoundaryNodes
  ) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sort, reverse, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Exts (groupWith)

import NetSpider.Found (FoundNode(..), LinkState(..), FoundLink(targetNode))
import NetSpider.Log
  ( runWriterLoggingM, WriterLoggingM, logDebugW, LogLine, spack
  )
import NetSpider.Log ()
import NetSpider.Query.Internal (FoundNodePolicy(..))
import NetSpider.Query (policyOverwrite, policyAppend)
import NetSpider.Snapshot.Internal
  ( SnapshotGraph, SnapshotNode(..), SnapshotLink(..)
  )
import NetSpider.Timestamp (Timestamp)
import NetSpider.Unify
  ( LinkSampleUnifier,
    LinkSampleID,
    LinkSample(..),
    linkSampleId
  )
import qualified NetSpider.Unify as Unify

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
-- The 'FoundNodePolicy' controls the behavior of 'addFoundNode'. If
-- it's 'policyOverwrite', 'Weaver' maintains only the 'FoundNode'
-- with the latest timestamp for each node. If it's 'policyAppend',
-- 'Weaver' maintains all 'FoundNode's added.
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
--
-- A visited node is the one that has at least one 'FoundNode' added,
-- or on which 'markAsVisited' has executed.
isVisited :: (Eq n, Hashable n) => n -> Weaver n na la -> Bool
isVisited n w = HM.member n (visitedNodes w)

-- | Get the visited 'FoundNode's for the given node ID kept in
-- 'Weaver'. It returns 'Nothing' if the node ID is not visited.
getVisitedNodes :: (Eq n, Hashable n) => n -> Weaver n na la -> Maybe [FoundNode n na la]
getVisitedNodes n w = HM.lookup n (visitedNodes w)

-- | Make 'SnapshotGraph' from the current 'Weaver'.
--
-- The 'SnapshotGraph' is constructed from all 'FoundNode's added to
-- the 'Weaver' so far.
getSnapshot :: (Ord n, Hashable n, Show n) => LinkSampleUnifier n na fla sla -> Weaver n na fla -> SnapshotGraph n na sla
getSnapshot u w = fst $ getSnapshot' u w

-- | Get boundary nodes from the 'Weaver'.
--
-- A boundary node is a node that has been observed as a target of
-- some links but not visited yet.
getBoundaryNodes :: (Eq n, Hashable n) => Weaver n na fla -> [n]
getBoundaryNodes weaver = filter (\nid -> not $ isVisited nid weaver) $ all_target_nodes
  where
    all_target_nodes = (map targetNode . neighborLinks) =<< (concat $ HM.elems $ visitedNodes weaver)

-- | (Basically for testing): run 'markAsVisited' on all boundary
-- nodes.
visitAllBoundaryNodes :: (Eq n, Hashable n) => Weaver n na fla -> Weaver n na fla
visitAllBoundaryNodes weaver = foldl' (\w n -> markAsVisited n w) weaver $ getBoundaryNodes weaver

latestFoundNodeFor :: (Eq n, Hashable n) => n -> Weaver n na fla -> Maybe (FoundNode n na fla)
latestFoundNodeFor nid weaver = do
  found_nodes <- HM.lookup nid $ visitedNodes weaver
  listToMaybe $ reverse $ sortOn foundAt $ found_nodes

makeSnapshotNode :: (Eq n, Hashable n) => Weaver n na fla -> n -> SnapshotNode n na
makeSnapshotNode weaver nid =
  SnapshotNode { _nodeId = nid,
                 _isOnBoundary = not $ isVisited nid weaver,
                 _nodeTimestamp = m_timestamp,
                 _nodeAttributes = m_attributes
               }
  where
    mfn = latestFoundNodeFor nid weaver
    m_timestamp = fmap foundAt mfn
    m_attributes = fmap nodeAttributes mfn

allLinkSamples :: Weaver n na la -> [LinkSample n la]
allLinkSamples w = Unify.toLinkSamples =<< (concat $ HM.elems $ visitedNodes w)

-- | Same as 'getSnapshot', but it also returns logs.
getSnapshot' :: (Ord n, Hashable n, Show n)
             => LinkSampleUnifier n na fla sla
             -> Weaver n na fla
             -> (SnapshotGraph n na sla, [LogLine])
getSnapshot' unifier weaver = ((nodes, links), logs)
  where
    nodes = visited_nodes ++ boundary_nodes
    visited_nodes = map (makeSnapshotNode weaver) $ HM.keys $ visitedNodes weaver
    boundary_nodes = map (makeSnapshotNode weaver) $ getBoundaryNodes weaver
    (links, logs) = runWriterLoggingM $ fmap mconcat
                    $ mapM (makeSnapshotLinks unifier weaver)
                    $ groupWith linkSampleId $ allLinkSamples weaver

-- | The input 'LinkSample's must be for the equivalent
-- 'LinkSampleID'. The output is list of 'SnapshotLink's, each of
-- which corresponds to a subgroup of 'LinkSample's.
makeSnapshotLinks :: (Eq n, Hashable n, Show n)
                  => LinkSampleUnifier n na fla sla
                  -> Weaver n na fla
                  -> [LinkSample n fla]
                  -> WriterLoggingM [SnapshotLink n sla]
makeSnapshotLinks _ _ [] = return []
makeSnapshotLinks unifier weaver link_samples@(head_sample : _) = do
  unified <- doUnify link_samples
  logUnified unified
  return $ mapMaybe makeSnapshotLink unified
  where
    makeEndNode getter = makeSnapshotNode weaver $ getter $ head_sample
    doUnify = unifier (makeEndNode lsSubjectNode) (makeEndNode lsTargetNode)
    logUnified unified = logDebugW ( "Unify link [" <> (spack $ lsSubjectNode head_sample) <> "]-["
                                     <> (spack $ lsTargetNode head_sample) <> "]: "
                                     <> "from " <> (spack $ length link_samples) <> " samples "
                                     <> "to " <> (spack $ length unified) <> " samples"
                                   )
    makeSnapshotLink unified_sample = do
      case lsLinkState unified_sample of
       LinkUnused -> Nothing
       LinkToTarget -> Just $ sampleToLink unified_sample True True
       LinkToSubject -> Just $ sampleToLink unified_sample False True
       LinkBidirectional -> Just $ sampleToLink unified_sample True False
    sampleToLink sample to_target is_directed = 
      SnapshotLink { _sourceNode = (if to_target then lsSubjectNode else lsTargetNode) sample,
                     _destinationNode = (if to_target then lsTargetNode else lsSubjectNode) sample,
                     _isDirected = is_directed,
                     _linkTimestamp = lsTimestamp sample,
                     _linkAttributes = lsLinkAttributes sample
                   }
