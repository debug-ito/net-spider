{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         connectWS,
         connectWith,
         Host,
         Port,
         Config(..),
         defConfig,
         close,
         addFoundNode,
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
import Network.Greskell.WebSocket
  ( Host, Port
  )
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (EID, LinkAttributes, NodeAttributes)
import NetSpider.Graph.Internal (VFoundNode(..), EFinds(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Snapshot (SnapshotElement)
import NetSpider.Snapshot.Internal (SnapshotNode(..), SnapshotLink(..))
import NetSpider.Spider.Internal.Graph
  ( gMakeFoundNode, gAllNodes, gHasNodeID, gHasNodeEID, gNodeEID, gNodeID, gMakeNode, gClearAll,
    gLatestFoundNode, gSelectFoundNode, gFinds, gHasFoundNodeEID, gAllFoundNode
  )
import NetSpider.Spider.Internal.Type
  ( Spider(..), Config(..), defConfig,
    SnapshotLinkID(..), SnapshotLinkSample(..)
  )

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Host -> Port -> IO (Spider n na la)
connectWS host port = connectWith $ defConfig { wsHost = host,
                                                wsPort = port
                                              }

-- | Connect to the server with the given 'Config'.
connectWith :: Config n na la -> IO (Spider n na la)
connectWith conf = do
  client <- Gr.connect (wsHost conf) (wsPort conf)
  return $ Spider { spiderConfig = conf,
                    spiderClient = client
                  }
  
-- | Close and release the 'Spider' object.
close :: Spider n na la -> IO ()
close sp = Gr.close $ spiderClient sp

submitB :: (ToGreskell g, r ~ GreskellReturn g, AsIterator r, v ~ IteratorItem r, FromGraphSON v)
        => Spider n na la -> Binder g -> IO (Gr.ResultHandle v)
submitB sp b = Gr.submit (spiderClient sp) script mbs
  where
    (script, bs) = runBinder b
    mbs = Just bs

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider n na la -> IO ()
clearAll spider = Gr.drainResults =<< submitB spider (return gClearAll)

-- | Add a 'FoundNode' (observation of a node) to the NetSpider
-- database.
addFoundNode :: (ToJSON n, LinkAttributes la, NodeAttributes na) => Spider n na la -> FoundNode n na la -> IO ()
addFoundNode spider found_node = do
  subject_vid <- getOrMakeNode spider $ subjectNode found_node
  link_pairs <- traverse linkAndTargetVID $ neighborLinks found_node
  makeFoundNodeVertex subject_vid link_pairs
  where
    linkAndTargetVID link = do
      target_vid <- getOrMakeNode spider $ targetNode link
      return (link, target_vid)
    makeFoundNodeVertex subject_vid link_pairs =
      Gr.drainResults =<< submitB spider (fmap void $ gMakeFoundNode subject_vid link_pairs found_node)
  
vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider n na la -> n -> IO (Maybe EID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider gt
  where
    gt = gNodeEID <$.> gHasNodeID spider nid <*.> pure gAllNodes

getOrMakeNode :: (ToJSON n) => Spider n na la -> n -> IO EID
getOrMakeNode spider nid = do
  mvid <- getNode spider nid
  case mvid of
   Just vid -> return vid
   Nothing -> makeNode
  where
    makeNode = expectOne =<< Gr.slurpResults =<< submitB spider (liftWalk gNodeEID <$.> gMakeNode spider nid)
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
getLatestSnapshot :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, LinkAttributes la, NodeAttributes na)
                  => Spider n na la
                  -> n -- ^ ID of the node where it starts traversing.
                  -> IO (Vector (SnapshotElement n na la))
getLatestSnapshot spider start_nid = do
  ref_state <- newIORef $ initSnapshotState $ return start_nid
  recurseVisitNodesForSnapshot spider ref_state
  -- print =<< readIORef ref_state
  fmap (makeSnapshot spider) $ readIORef ref_state

-- TODO: We can create much more complex function to query snapshot
-- graphs, but at least we need 'getLatestSnapshot'.



recurseVisitNodesForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, LinkAttributes la, NodeAttributes na)
                             => Spider n na la
                             -> IORef (SnapshotState n na la)
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

visitNodeForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, LinkAttributes la, NodeAttributes na)
                     => Spider n na la
                     -> IORef (SnapshotState n na la)
                     -> n
                     -> IO ()
visitNodeForSnapshot spider ref_state visit_nid = do
  mnode_eid <- getVisitedNodeEID
  case mnode_eid of
   Nothing -> return ()
   Just node_eid -> do
     mnext_found <- getNextFoundNode node_eid
     markAsVisited mnext_found
     case mnext_found of
      Nothing -> return ()
      Just next_found -> do
        link_samples <- makeSnapshotLinkSamples spider visit_nid next_found
        modifyIORef ref_state $ addSnapshotLinkSamples link_samples
  where
    markAsVisited mvfn = modifyIORef ref_state $ addVisitedNode visit_nid mvfn
    getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gNodeEID <$.> gHasNodeID spider visit_nid <*.> pure gAllNodes
    getNextFoundNode node_eid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gLatestFoundNode
                 <$.> gSelectFoundNode gIdentity -- TODO: select FoundNode to consider
                 <$.> gHasNodeEID node_eid
                 <*.> pure gAllNodes

makeSnapshotLinkSamples :: (FromGraphSON n, LinkAttributes la)
                        => Spider n na la
                        -> n -- ^ subject node ID.
                        -> VFoundNode na
                        -> IO (Vector (SnapshotLinkSample n la))
makeSnapshotLinkSamples spider subject_nid vneighbors = do
  finds_edges <- getFinds $ vfnId vneighbors
  traverse toSnapshotLinkEntry finds_edges
  where
    getFinds neighbors_eid = Gr.slurpResults =<< submitB spider binder
      where
        binder = gFinds <$.> gHasFoundNodeEID neighbors_eid <*.> pure gAllFoundNode
    toSnapshotLinkEntry efinds = do
      target_nid <- getNodeID $ efTargetId $ efinds
      let lid = SnapshotLinkID { sliSubjectNode = subject_nid,
                                 sliTargetNode = target_nid
                               }
          lsample = SnapshotLinkSample { slsLinkId = lid,
                                         slsLinkState = efLinkState efinds,
                                         slsTimestamp = vfnTimestamp vneighbors,
                                         slsLinkAttributes = efLinkAttributes efinds
                                       }
      return lsample
    getNodeID node_eid = expectOne =<< tryGetNodeID spider node_eid
    -- TODO: Using .as and .select steps, we can get EFinds and its destination vertex simultaneously.
      where
        expectOne (Just r) = return r
        expectOne Nothing = throwString "Expects a Vertex for a NodeID, but nothing found."
        -- TODO: better exception spec.

tryGetNodeID :: FromGraphSON n => Spider n na la -> EID -> IO (Maybe n)
tryGetNodeID spider node_eid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
  where
    binder = gNodeID spider <$.> gHasNodeEID node_eid <*.> pure gAllNodes

-- | The state kept while making the snapshot graph.
data SnapshotState n na la =
  SnapshotState
  { ssUnvisitedNodes :: !(Vector n),
    ssVisitedNodes :: !(HashMap n (Maybe (VFoundNode na))),
    -- ^ If the visited node has no observation yet, its node
    -- attributes 'Nothing'.
    ssVisitedLinks :: !(HashMap (SnapshotLinkID n) (Vector (SnapshotLinkSample n la)))
  }
  deriving (Show)

emptySnapshotState :: (Ord n, Hashable n) => SnapshotState n na la
emptySnapshotState = SnapshotState
                     { ssUnvisitedNodes = mempty,
                       ssVisitedNodes = mempty,
                       ssVisitedLinks = mempty
                     }

initSnapshotState :: (Ord n, Hashable n) => Vector n -> SnapshotState n na la
initSnapshotState init_unvisited_nodes = emptySnapshotState { ssUnvisitedNodes = init_unvisited_nodes }

addVisitedNode :: (Eq n, Hashable n) => n -> Maybe (VFoundNode na) -> SnapshotState n na la -> SnapshotState n na la
addVisitedNode nid mv state = state { ssVisitedNodes = HM.insert nid mv $ ssVisitedNodes state }

addSnapshotLinkSample :: (Ord n, Hashable n)
                      => SnapshotLinkSample n la -> SnapshotState n na la -> SnapshotState n na la
addSnapshotLinkSample ls state = state { ssVisitedLinks = updatedLinks,
                                     ssUnvisitedNodes = updatedUnvisited
                                   }
  where
    link_id = slsLinkId ls
    updatedLinks = HM.insertWith (V.++) link_id (return ls) $ ssVisitedLinks state
    target_nid = sliTargetNode link_id
    target_already_visited = HM.member target_nid $ ssVisitedNodes state
    updatedUnvisited = if target_already_visited
                       then ssUnvisitedNodes state
                       else V.snoc (ssUnvisitedNodes state) target_nid

addSnapshotLinkSamples :: (Ord n, Hashable n)
                       => Vector (SnapshotLinkSample n la) -> SnapshotState n na la -> SnapshotState n na la
addSnapshotLinkSamples links orig_state = foldr' addSnapshotLinkSample orig_state links

popHeadV :: Vector a -> (Maybe a, Vector a)
popHeadV v = let mh = v V.!? 0
             in case mh of
                 Just _ -> (mh, V.tail v)
                 Nothing -> (mh, v)

popUnvisitedNode :: SnapshotState n na la -> (SnapshotState n na la, Maybe n)
popUnvisitedNode state = (updated, popped)
  where
    updated = state { ssUnvisitedNodes = updatedUnvisited }
    (popped, updatedUnvisited) = popHeadV $ ssUnvisitedNodes state

makeSnapshot :: Spider n na la -> SnapshotState n na la -> Vector (SnapshotElement n na la)
makeSnapshot spider state = (fmap Left nodes) V.++ (fmap Right links)
  where
    nodes = visited_nodes V.++ boundary_nodes
    makeSnapshotNode on_boundary nid mvfn =
      SnapshotNode { _nodeId = nid,
                     _isOnBoundary = on_boundary,
                     _nodeTimestamp = fmap vfnTimestamp $ mvfn,
                     _nodeAttributes = fmap vfnAttributes $ mvfn
                   }
    visited_nodes = foldr' V.cons mempty $ map (uncurry $ makeSnapshotNode False) $ HM.toList $ ssVisitedNodes state
    boundary_nodes = fmap (\nid -> makeSnapshotNode True nid Nothing) $ ssUnvisitedNodes state
    links = mconcat $ map (makeSnapshotLinks spider) $ HM.elems $ ssVisitedLinks state

-- | The input 'SnapshotLinkSample's must be for the equivalent
-- 'SnapshotLinkID'. The output is list of 'SnapshotLink's, each of
-- which corresponds to a subgroup of 'SnapshotLinkSample's.
makeSnapshotLinks :: Spider n na la -> Vector (SnapshotLinkSample n la) -> Vector (SnapshotLink n la)
makeSnapshotLinks spider link_samples =
  V.mapMaybe makeSnapshotLink $ makeSubgroups link_samples
  where
    makeSubgroups = subgroupSnapshotLinkSamples $ spiderConfig spider
    makeSnapshotLink link_subgroup = do
      agg_sample <- latestSnapshotLinkSample link_subgroup
      case slsLinkState agg_sample of
       LinkUnused -> Nothing
       LinkToTarget -> Just $ aggSampleToLink agg_sample True True
       LinkToSubject -> Just $ aggSampleToLink agg_sample False True
       LinkBidirectional -> Just $ aggSampleToLink agg_sample True False
    aggSampleToLink agg_sample to_target is_directed = 
      SnapshotLink { _sourceNode = (if to_target then sliSubjectNode else sliTargetNode) link_id,
                     _destinationNode = (if to_target then sliTargetNode else sliSubjectNode) link_id,
                     _isDirected = is_directed,
                     _linkTimestamp = slsTimestamp agg_sample,
                     _linkAttributes = slsLinkAttributes agg_sample
                   }
      where
        link_id = slsLinkId agg_sample

-- | Get the 'SnapshotLinkSample' that has the latest (biggest)
-- timestamp.
latestSnapshotLinkSample :: Vector (SnapshotLinkSample n la) -> Maybe (SnapshotLinkSample n la)
latestSnapshotLinkSample samples = let (mhead, samples_tail) = popHeadV samples
                                   in fmap (aggregate samples_tail) mhead
  where
    aggregate samples_tail sample_head = foldr' f sample_head samples_tail
      where
        f :: SnapshotLinkSample n la -> SnapshotLinkSample n la -> SnapshotLinkSample n la
        f ls rs = if slsTimestamp ls >= slsTimestamp rs
                  then ls
                  else rs
