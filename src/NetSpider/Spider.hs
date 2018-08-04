{-# LANGUAGE OverloadedStrings, TypeFamilies, DeriveGeneric #-}
-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         connectWS,
         Host,
         Port,
         close,
         addNeighbors,
         getLatestSnapshot,
         clearAll
       ) where

import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Foldable (foldr')
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity
  )
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Greskell.WebSocket
  ( Host, Port
  )
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Neighbors (Neighbors(..), FoundLink(..), LinkState(..))
import NetSpider.Snapshot (SnapshotElement)
import NetSpider.Snapshot.Internal (SnapshotNode(..), SnapshotLink(..))
import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Spider.Internal.Graph
  ( EID, gMakeNeighbors, gAllNodes, gHasNodeID, gHasNodeEID, gNodeEID, gNodeID, gMakeNode, gClearAll,
    gLatestNeighbors, gSelectNeighbors, gFinds, gHasNeighborsEID, gAllNeighbors,
    VNeighbors(..), EFinds(..)
  )

-- | An IO agent of the NetSpider database.
--
-- - type @n@: node ID.
-- - type @p@: port ID.
data Spider n p =
  Spider
  { spiderClient :: Gr.Client
  }

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Host -> Port -> IO (Spider n p)
connectWS host port = fmap Spider $ Gr.connect host port

-- | Close and release the 'Spider' object.
close :: Spider n p -> IO ()
close sp = Gr.close $ spiderClient sp

submitB :: (ToGreskell g, r ~ GreskellReturn g, AsIterator r, v ~ IteratorItem r, FromGraphSON v)
        => Spider n p -> Binder g -> IO (Gr.ResultHandle v)
submitB sp b = Gr.submit (spiderClient sp) script mbs
  where
    (script, bs) = runBinder b
    mbs = Just bs

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider n p -> IO ()
clearAll spider = Gr.drainResults =<< submitB spider (return gClearAll)

-- | Add an observation of 'Neighbors' to the NetSpider database.
addNeighbors :: (ToJSON n, ToJSON p) => Spider n p -> Neighbors n p -> IO ()
addNeighbors spider nbs = do
  subject_vid <- getOrMakeNode spider $ subjectNode nbs
  link_pairs <- traverse linkAndTargetVID $ neighborLinks nbs
  makeNeighborsVertex spider subject_vid link_pairs $ observedTime nbs
  where
    linkAndTargetVID link = do
      target_vid <- getOrMakeNode spider $ targetNode link
      return (link, target_vid)

makeNeighborsVertex :: (ToJSON p)
                    => Spider n p
                    -> EID -- ^ subject node vertex ID
                    -> Vector (FoundLink n p, EID) -- ^ (link, target node vertex ID)
                    -> Timestamp
                    -> IO ()
makeNeighborsVertex spider subject_vid link_pairs timestamp =
  Gr.drainResults =<< submitB spider (fmap void $ gMakeNeighbors subject_vid link_pairs timestamp)

vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider n p -> n -> IO (Maybe EID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider gt
  where
    gt = gNodeEID <$.> gHasNodeID nid <*.> pure gAllNodes

getOrMakeNode :: (ToJSON n) => Spider n p -> n -> IO EID
getOrMakeNode spider nid = do
  mvid <- getNode spider nid
  case mvid of
   Just vid -> return vid
   Nothing -> makeNode
  where
    makeNode = expectOne =<< Gr.slurpResults =<< submitB spider (liftWalk gNodeEID <$.> gMakeNode nid)
    expectOne v = case vToMaybe v of
      Just e -> return e
      Nothing -> throwString "Expects at least single result, but got nothing."
      -- TODO: make decent exception spec.

-- | Get the latest snapshot graph from the NetSpider database. It
-- builds the snapshot graph by traversing the history graph from the
-- given starting node.
--
-- This function is very simple, and should be used only for testing.
-- This function starts from an arbitrary node, traverses the history
-- graph using the latest links with unlimited number of hops.
getLatestSnapshot :: (FromGraphSON n, FromGraphSON p, ToJSON n, Eq n, Eq p, Hashable n, Hashable p)
                  => Spider n p
                  -> n -- ^ ID of the node where it starts traversing.
                  -> IO (Vector (SnapshotElement n p))
getLatestSnapshot spider start_nid = do
  ref_state <- newIORef $ initSnapshotState $ return start_nid
  recurseVisitNodesForSnapshot spider ref_state
  fmap makeSnapshot $ readIORef ref_state

recurseVisitNodesForSnapshot :: (ToJSON n, Eq n, Hashable n, FromGraphSON n, Eq p, Hashable p, FromGraphSON p)
                             => Spider n p
                             -> IORef (SnapshotState n p)
                             -> IO ()
recurseVisitNodesForSnapshot spider ref_state = go
  where
    go = do
      mnext_visit <- getNextVisit
      case mnext_visit of
       Nothing -> return ()
       Just next_visit -> do
         visitNodeForSnapshot spider ref_state next_visit
         go
    getNextVisit = atomicModifyIORef' ref_state popUnvisitedNode
    -- TODO: limit number of steps.

visitNodeForSnapshot :: (ToJSON n, Eq n, Hashable n, FromGraphSON n, Eq p, Hashable p, FromGraphSON p)
                     => Spider n p
                     -> IORef (SnapshotState n p)
                     -> n
                     -> IO ()
visitNodeForSnapshot spider ref_state visit_nid = do
  markAsVisited
  mnode_eid <- getVisitedNodeEID
  case mnode_eid of
   Nothing -> return ()
   Just node_eid -> do
     mnext_neighbors <- getNextNeighbors node_eid
     case mnext_neighbors of
      Nothing -> return ()
      Just next_neighbors -> do
        slink_entries <- makeSnapshotLinks spider visit_nid next_neighbors
        modifyIORef ref_state $ addSnapshotLinks slink_entries
  where
    markAsVisited = modifyIORef ref_state $ addVisitedNode visit_nid
    getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gNodeEID <$.> gHasNodeID visit_nid <*.> pure gAllNodes
    getNextNeighbors node_eid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gLatestNeighbors
                 <$.> gSelectNeighbors gIdentity -- TODO: select Neighbors to consider
                 <$.> gHasNodeEID node_eid
                 <*.> pure gAllNodes

makeSnapshotLinks :: (FromGraphSON n, FromGraphSON p)
                  => Spider n p
                  -> n -- ^ subject node ID.
                  -> VNeighbors
                  -> IO (Vector (SnapshotLinkID n p, SnapshotLinkSample))
makeSnapshotLinks spider subject_nid vneighbors = do
  finds_edges <- getFinds $ vnID vneighbors
  traverse toSnapshotLinkEntry finds_edges
  where
    getFinds neighbors_eid = Gr.slurpResults =<< submitB spider binder
      where
        binder = gFinds <$.> gHasNeighborsEID neighbors_eid <*.> pure gAllNeighbors
    toSnapshotLinkEntry efinds = do
      target_nid <- getNodeID $ efTargetEID $ efinds
      return ( SnapshotLinkID { sliSubjectNode = subject_nid,
                                sliSubjectPort = efSubjectPort efinds,
                                sliTargetNode = target_nid,
                                sliTargetPort = efTargetPort efinds
                              },
               SnapshotLinkSample { slsLinkState = efLinkState efinds,
                                    slsTimestamp = vnTimestamp vneighbors
                                  }
             )
    getNodeID node_eid = expectOne =<< (fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder)
    -- TODO: Using .as and .select steps, we can get EFinds and its destination vertex simultaneously.
      where
        binder = gNodeID <$.> gHasNodeEID node_eid <*.> pure gAllNodes
        expectOne (Just r) = return r
        expectOne Nothing = throwString "Expects a Vertex for a NodeID, but nothing found."
        -- TODO: better exception spec.
    

-- We can create much more complex function to query snapshot graphs,
-- but at least we need 'getLatestSnapshot'.

-- | Identitfy of link while making the snapshot graph.
data SnapshotLinkID n p =
  SnapshotLinkID
  { sliSubjectNode :: !n,
    sliSubjectPort :: !p,
    sliTargetNode :: !n,
    sliTargetPort :: !p
  }
  deriving (Show,Eq,Ord,Generic)

instance (Hashable n, Hashable p) => Hashable (SnapshotLinkID n p)

-- | Observation sample of a link while making the snapshot graph.
data SnapshotLinkSample =
  SnapshotLinkSample
  { slsLinkState :: !LinkState,
    slsTimestamp :: !Timestamp
  }
  deriving (Show,Eq)

-- | The state kept while making the snapshot graph.
data SnapshotState n p =
  SnapshotState
  { ssUnvisitedNodes :: !(Vector n),
    ssVisitedNodes :: !(HashSet n),
    ssVisitedLinks :: !(HashMap (SnapshotLinkID n p) (Vector SnapshotLinkSample))
  }

emptySnapshotState :: (Eq n, Eq p, Hashable n, Hashable p) => SnapshotState n p
emptySnapshotState = SnapshotState
                     { ssUnvisitedNodes = mempty,
                       ssVisitedNodes = mempty,
                       ssVisitedLinks = mempty
                     }

initSnapshotState :: (Eq n, Eq p, Hashable n, Hashable p) => Vector n -> SnapshotState n p
initSnapshotState init_unvisited_nodes = emptySnapshotState { ssUnvisitedNodes = init_unvisited_nodes }

addVisitedNode :: (Eq n, Hashable n) => n -> SnapshotState n p -> SnapshotState n p
addVisitedNode nid state = state { ssVisitedNodes = HS.insert nid $ ssVisitedNodes state }

addSnapshotLink :: (Eq n, Hashable n, Eq p, Hashable p)
                => SnapshotLinkID n p -> SnapshotLinkSample -> SnapshotState n p -> SnapshotState n p
addSnapshotLink lid ls state = state { ssVisitedLinks = updatedLinks,
                                       ssUnvisitedNodes = updatedUnvisited
                                     }
  where
    updatedLinks = HM.insertWith (V.++) lid (return ls) $ ssVisitedLinks state
    target_nid = sliTargetNode lid
    target_already_visited = HS.member target_nid $ ssVisitedNodes state
    updatedUnvisited = if target_already_visited
                       then ssUnvisitedNodes state
                       else V.snoc (ssUnvisitedNodes state) target_nid

addSnapshotLinks :: (Eq n, Hashable n, Eq p, Hashable p)
                 => Vector (SnapshotLinkID n p, SnapshotLinkSample) -> SnapshotState n p -> SnapshotState n p
addSnapshotLinks links orig_state = foldr' (uncurry addSnapshotLink) orig_state links

popHeadV :: Vector a -> (Maybe a, Vector a)
popHeadV v = let mh = v V.!? 0
             in case mh of
                 Just _ -> (mh, V.tail v)
                 Nothing -> (mh, v)

popUnvisitedNode :: SnapshotState n p -> (SnapshotState n p, Maybe n)
popUnvisitedNode state = (updated, popped)
  where
    updated = state { ssUnvisitedNodes = updatedUnvisited }
    (popped, updatedUnvisited) = popHeadV $ ssUnvisitedNodes state

makeSnapshot :: SnapshotState n p -> Vector (SnapshotElement n p)
makeSnapshot state = (fmap Left nodes) V.++ (fmap Right links)
  where
    nodes = visited_nodes V.++ boundary_nodes
    makeSnapshotNode on_boundary nid =
      SnapshotNode { _nodeId = nid,
                     _isOnBoundary = on_boundary
                   }
    visited_nodes = fmap (makeSnapshotNode False) $ foldr' V.cons mempty $ ssVisitedNodes state
    boundary_nodes = fmap (makeSnapshotNode True) $ ssUnvisitedNodes state
    links = V.fromList $ catMaybes $ map (uncurry makeSnapshotLink) $ HM.toList $ ssVisitedLinks state

makeSnapshotLink :: SnapshotLinkID n p -> Vector SnapshotLinkSample -> Maybe (SnapshotLink n p)
makeSnapshotLink link_id link_samples = do
  agg_sample <- aggregateSnapshotLinkSamples link_id link_samples
  case slsLinkState agg_sample of
   LinkUnused -> Nothing
   LinkToTarget -> Just $ aggSampleToLink agg_sample True True
   LinkToSubject -> Just $ aggSampleToLink agg_sample False True
   LinkBidirectional -> Just $ aggSampleToLink agg_sample True False
  where
    aggSampleToLink agg_sample to_target is_directed = 
      SnapshotLink { _sourceNode = (if to_target then sliSubjectNode else sliTargetNode) link_id,
                     _destinationNode = (if to_target then sliTargetNode else sliSubjectNode) link_id,
                     _sourcePort = (if to_target then sliSubjectPort else sliTargetPort) link_id,
                     _destinationPort = (if to_target then sliTargetPort else sliSubjectPort) link_id,
                     _isDirected = is_directed,
                     _linkTimestamp = slsTimestamp agg_sample
                   }

-- | Aggregate 'SnapshotLinkSample's into one.
--
-- Input 'SnapshotLinkSample's are already filtered in terms of
-- 'Timestamp', so implementation should consider all samples
-- sufficiently recent. Note that the input 'SnapshotLinkSample's can
-- be inconsistent with each other.
aggregateSnapshotLinkSamples :: SnapshotLinkID n p -> Vector SnapshotLinkSample -> Maybe SnapshotLinkSample
aggregateSnapshotLinkSamples _ samples = let (mhead, samples_tail) = popHeadV samples
                                         in fmap (aggregate samples_tail) mhead
  where
    aggregate samples_tail sample_head = foldr' f sample_head samples_tail
      where
        f :: SnapshotLinkSample -> SnapshotLinkSample -> SnapshotLinkSample
        f ls rs = if slsTimestamp ls >= slsTimestamp rs -- simply trust the latest sample.
                  then ls
                  else rs
