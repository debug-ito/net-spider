{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( -- * Spider type
         Spider,
         -- * Make Spider
         connectWS,
         connectWith,
         -- * Close Spider
         close,
         -- * Graph operations
         addFoundNode,
         getSnapshotSimple,
         getSnapshot,
         clearAll
       ) where

import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Foldable (foldr', toList)
import Data.Hashable (Hashable)
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity, gSelectN, gAs,
    lookupAsM, newAsLabel
  )
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (mempty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Greskell.WebSocket (Host, Port)
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (EID, LinkAttributes, NodeAttributes)
import NetSpider.Graph.Internal (VFoundNode(..), EFinds(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Pair (Pair)
import NetSpider.Queue (Queue, newQueue, popQueue, pushQueue)
import NetSpider.Query (Query, defQuery, startsFrom, unifyLinkSamples)
import NetSpider.Snapshot.Internal (SnapshotNode(..), SnapshotLink(..))
import NetSpider.Spider.Config (Spider(..), Config(..), defConfig)
import NetSpider.Spider.Internal.Graph
  ( gMakeFoundNode, gAllNodes, gHasNodeID, gHasNodeEID, gNodeEID, gNodeID, gMakeNode, gClearAll,
    gLatestFoundNode, gSelectFoundNode, gFinds, gFindsTarget, gHasFoundNodeEID, gAllFoundNode
  )
import NetSpider.Unify (LinkSampleUnifier, LinkSample(..), LinkSampleID, linkSampleId)

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Eq n => Host -> Port -> IO (Spider n na la)
connectWS host port = connectWith $ defConfig { wsHost = host,
                                                wsPort = port
                                              }

-- | Connect to the server with the given 'Config'.
connectWith :: Config n na fla -> IO (Spider n na fla)
connectWith conf = do
  client <- Gr.connect (wsHost conf) (wsPort conf)
  return $ Spider { spiderConfig = conf,
                    spiderClient = client
                  }
  
-- | Close and release the 'Spider' object.
close :: Spider n na fla -> IO ()
close sp = Gr.close $ spiderClient sp

submitB :: (ToGreskell g, r ~ GreskellReturn g, AsIterator r, v ~ IteratorItem r, FromGraphSON v)
        => Spider n na fla -> Binder g -> IO (Gr.ResultHandle v)
submitB sp b = Gr.submit (spiderClient sp) script mbs
  where
    (script, bs) = runBinder b
    mbs = Just bs

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider n na fla -> IO ()
clearAll spider = Gr.drainResults =<< submitB spider (return gClearAll)

-- | Add a 'FoundNode' (observation of a node) to the NetSpider
-- database.
addFoundNode :: (ToJSON n, LinkAttributes fla, NodeAttributes na) => Spider n na fla -> FoundNode n na fla -> IO ()
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
  
getNode :: (ToJSON n) => Spider n na fla -> n -> IO (Maybe EID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider gt
  where
    gt = gNodeEID <$.> gHasNodeID spider nid <*.> pure gAllNodes

getOrMakeNode :: (ToJSON n) => Spider n na fla -> n -> IO EID
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

-- | Simple version of 'getSnapshot'. It builds the snapshot graph by
-- traversing the history graph from the given starting node.
--
-- This function is very simple, and should be used only for small
-- graphs.
getSnapshotSimple :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, LinkAttributes fla, NodeAttributes na)
                  => Spider n na fla
                  -> n -- ^ ID of the node where it starts traversing.
                  -> IO ([SnapshotNode n na], [SnapshotLink n fla])
getSnapshotSimple spider start_nid = getSnapshot spider $ defQuery [start_nid]


-- | Get the snapshot graph from the history graph as specified by the
-- 'Query'.
getSnapshot :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, LinkAttributes fla, NodeAttributes na)
            => Spider n na fla
            -> Query n na fla sla
            -> IO ([SnapshotNode n na], [SnapshotLink n sla])
getSnapshot spider query = do
  ref_state <- newIORef $ initSnapshotState $ startsFrom query
  recurseVisitNodesForSnapshot spider ref_state
  -- print =<< readIORef ref_state
  fmap (makeSnapshot $ unifyLinkSamples query) $ readIORef ref_state


recurseVisitNodesForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, LinkAttributes fla, NodeAttributes na)
                             => Spider n na fla
                             -> IORef (SnapshotState n na fla)
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

-- TODO: これを使ってvisitを実装する。
traverseEFindsOneHop :: (ToJSON n, FromGraphSON n, NodeAttributes na, LinkAttributes fla)
                     => Spider n na fla -> n -> IO (Vector (VFoundNode na, EFinds fla, n))
traverseEFindsOneHop spider visit_nid = traverse extractFromSMap =<< Gr.slurpResults =<< submitQuery
  where
    submitQuery = Gr.submit (spiderClient spider) query (Just bindings)
    ((query, label_vfn, label_ef, label_target_nid), bindings) = runBinder $ do
      lvfn <- newAsLabel
      lef <- newAsLabel
      ln <- newAsLabel
      gt <- gSelectN lvfn lef [ln]
            <$.> gAs ln <$.> gNodeID spider <$.> gFindsTarget
            <$.> gAs lef <$.> gFinds
            <$.> gAs lvfn <$.> gSelectFoundNode gIdentity -- TODO: select FoundNode to consider
            <$.> gHasNodeID spider visit_nid <*.> pure gAllNodes
      return (gt, lvfn, lef, ln)
    extractFromSMap smap =
      (,,)
      <$> lookupAsM label_vfn smap 
      <*> lookupAsM label_ef smap 
      <*> lookupAsM label_target_nid smap 

visitNodeForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, LinkAttributes fla, NodeAttributes na)
                     => Spider n na fla
                     -> IORef (SnapshotState n na fla)
                     -> n
                     -> IO ()
visitNodeForSnapshot spider ref_state visit_nid = do
  mnode_eid <- getVisitedNodeEID -- TODO: ここでEIDを取ってくる意味あるか？一気にtraverseできるのでは？
  case mnode_eid of
   Nothing -> return ()
   Just node_eid -> do
     -- TODO: (2018-09-08) Final VFoundNode is the latest of those in
     -- the query range. however, there should be an option to
     -- traverse ALL "finds" edges in the query range, instead of
     -- those with the latest timestamp. To do that, we may have to
     -- use .as and .select steps to get "finds" edge and both of its
     -- end nodes at once.
     mnext_found <- getNextFoundNode node_eid
     markAsVisited mnext_found
     case mnext_found of
      Nothing -> return ()
      Just next_found -> do
        link_samples <- makeLinkSamples spider visit_nid next_found
        modifyIORef ref_state $ addLinkSamples $ toList link_samples
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

makeLinkSamples :: (FromGraphSON n, LinkAttributes fla)
                => Spider n na fla
                -> n -- ^ subject node ID.
                -> VFoundNode na
                -> IO (Vector (LinkSample n fla))
makeLinkSamples spider subject_nid vneighbors = do
  finds_targets <- getFindsAndTargetID $ vfnId vneighbors
  return $ fmap (uncurry toSnapshotLinkEntry) finds_targets
  where
    getFindsAndTargetID neighbors_eid = do
      traverse extract =<< Gr.slurpResults =<< Gr.submit (spiderClient spider) gtrav (Just binding)
      where
        ((gtrav, label_finds, label_target), binding) = runBinder $ do
          lfinds <- newAsLabel
          ltarget <- newAsLabel
          gt <- gSelectN lfinds ltarget []
                <$.> (gAs ltarget) <$.> gNodeID spider <$.> gFindsTarget <$.> (gAs lfinds)
                <$.> gFinds <$.> gHasFoundNodeEID neighbors_eid <*.> pure gAllFoundNode
          return (gt, lfinds, ltarget)
        extract smap = (,) <$> lookupAsM label_finds smap <*> lookupAsM label_target smap
    toSnapshotLinkEntry efinds target_nid =
      LinkSample { lsSubjectNode = subject_nid,
                   lsTargetNode = target_nid,
                   lsLinkState = efLinkState efinds,
                   lsTimestamp = vfnTimestamp vneighbors,
                   lsLinkAttributes = efLinkAttributes efinds
                 }

-- | The state kept while making the snapshot graph.
data SnapshotState n na fla =
  SnapshotState
  { ssUnvisitedNodes :: !(Queue n),
    ssVisitedNodes :: !(HashMap n (Maybe (VFoundNode na))),
    -- ^ If the visited node has no observation yet, its node
    -- attributes 'Nothing'.
    ssVisitedLinks :: !(HashMap (LinkSampleID n) [LinkSample n fla])
  }
  deriving (Show)

emptySnapshotState :: (Ord n, Hashable n) => SnapshotState n na fla
emptySnapshotState = SnapshotState
                     { ssUnvisitedNodes = mempty,
                       ssVisitedNodes = mempty,
                       ssVisitedLinks = mempty
                     }

initSnapshotState :: (Ord n, Hashable n) => [n] -> SnapshotState n na fla
initSnapshotState init_unvisited_nodes = emptySnapshotState { ssUnvisitedNodes = newQueue init_unvisited_nodes }

addVisitedNode :: (Eq n, Hashable n) => n -> Maybe (VFoundNode na) -> SnapshotState n na fla -> SnapshotState n na fla
addVisitedNode nid mv state = state { ssVisitedNodes = HM.insert nid mv $ ssVisitedNodes state }

addLinkSample :: (Ord n, Hashable n)
              => LinkSample n fla -> SnapshotState n na fla -> SnapshotState n na fla
addLinkSample ls state = state { ssVisitedLinks = updatedLinks,
                                 ssUnvisitedNodes = updatedUnvisited
                               }
  where
    link_id = linkSampleId ls
    updatedLinks = HM.insertWith (++) link_id (return ls) $ ssVisitedLinks state
    target_nid = lsTargetNode ls
    target_already_visited = HM.member target_nid $ ssVisitedNodes state
    updatedUnvisited = if target_already_visited
                       then ssUnvisitedNodes state
                       else pushQueue target_nid $ ssUnvisitedNodes state

addLinkSamples :: (Ord n, Hashable n)
               => [LinkSample n fla] -> SnapshotState n na fla -> SnapshotState n na fla
addLinkSamples links orig_state = foldr' addLinkSample orig_state links

popUnvisitedNode :: SnapshotState n na fla -> (SnapshotState n na fla, Maybe n)
popUnvisitedNode state = (updated, popped)
  where
    updated = state { ssUnvisitedNodes = updatedUnvisited }
    (popped, updatedUnvisited) = popQueue $ ssUnvisitedNodes state

makeSnapshot :: (Eq n, Hashable n)
             => LinkSampleUnifier n na fla sla
             -> SnapshotState n na fla
             -> ([SnapshotNode n na], [SnapshotLink n sla])
makeSnapshot unifier state = (nodes, links)
  where
    nodes = visited_nodes ++ boundary_nodes
    visited_nodes = map (makeSnapshotNode state) $ HM.keys $ ssVisitedNodes state
    boundary_nodes = map (makeSnapshotNode state) $ toList $ ssUnvisitedNodes state
    links = mconcat $ map (makeSnapshotLinks unifier state) $ HM.elems $ ssVisitedLinks state

makeSnapshotNode :: (Eq n, Hashable n) => SnapshotState n na fla -> n -> SnapshotNode n na
makeSnapshotNode state nid =
  SnapshotNode { _nodeId = nid,
                 _isOnBoundary = on_boundary,
                 _nodeTimestamp = fmap vfnTimestamp $ mvfn,
                 _nodeAttributes = fmap vfnAttributes $ mvfn
               }
  where
    (on_boundary, mvfn) =
      case HM.lookup nid $ ssVisitedNodes state of
       Nothing -> (True, Nothing)
       Just mv -> (False, mv)

-- | The input 'LinkSample's must be for the equivalent
-- 'LinkSampleID'. The output is list of 'SnapshotLink's, each of
-- which corresponds to a subgroup of 'LinkSample's.
makeSnapshotLinks :: (Eq n, Hashable n)
                  => LinkSampleUnifier n na fla sla
                  -> SnapshotState n na fla
                  -> [LinkSample n fla]
                  -> [SnapshotLink n sla]
makeSnapshotLinks _ _ [] = []
makeSnapshotLinks unifier state link_samples@(head_sample : _) =
  mapMaybe makeSnapshotLink $ doUnify link_samples
  where
    makeEndNode getter = makeSnapshotNode state $ getter $ head_sample
    doUnify = unifier (makeEndNode lsSubjectNode) (makeEndNode lsTargetNode)
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

