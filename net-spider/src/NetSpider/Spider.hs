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
         -- * Bracket form
         withSpider,
         -- * Graph operations
         addFoundNode,
         getSnapshotSimple,
         getSnapshot,
         clearAll
       ) where

import Control.Category ((<<<))
import Control.Exception.Safe (throwString, bracket)
import Control.Monad (void, mapM_, mapM)
import Data.Aeson (ToJSON)
import Data.Foldable (foldr', toList, foldl')
import Data.List (intercalate)
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity, gSelect1, gAs, gProject, gByL, gIdentity, gFold,
    gRepeat, gEmitAlwaysHead, gSimplePath,
    lookupAsM, newAsLabel,
    Transform, Walk
  )
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import Data.Monoid (mempty, (<>))
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Greskell.WebSocket (Host, Port)
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (EID, LinkAttributes, NodeAttributes)
import NetSpider.Graph.Internal
  ( VFoundNode, EFinds, VNode,
    VFoundNodeData(..), EFindsData(..),
    gVFoundNodeData, gEFindsData,
    makeFoundNode, makeFoundLink
  )
import NetSpider.Found
  (FoundNode(..), FoundLink(..), LinkState(..), allTargetNodes)
import qualified NetSpider.Found as Found
import NetSpider.Log (runWriterLoggingM, WriterLoggingM, logDebugW, LogLine, spack)
import NetSpider.Pair (Pair)
import NetSpider.Queue (Queue, newQueue, popQueue, pushQueue)
import NetSpider.Query
  ( Query, defQuery, startsFrom, unifyLinkSamples, timeInterval,
    foundNodePolicy,
    Interval
  )
import NetSpider.Query.Internal (FoundNodePolicy(..))
import NetSpider.Snapshot.Internal (SnapshotGraph, SnapshotNode(..), SnapshotLink(..))
import NetSpider.Spider.Config (Config(..), defConfig)
import NetSpider.Spider.Internal.Graph
  ( gMakeFoundNode, gAllNodes, gHasNodeID, gHasNodeEID, gNodeEID, gNodeID, gMakeNode, gClearAll,
    gLatestFoundNode, gSelectFoundNode, gFinds, gFindsTarget, gHasFoundNodeEID, gAllFoundNode,
    gFilterFoundNodeByTime, gSubjectNodeID, gTraverseViaFinds, 
  )
import NetSpider.Spider.Internal.Log
  ( runLogger, logDebug, logWarn, logLine
  )
import NetSpider.Spider.Internal.Spider (Spider(..))
import NetSpider.Timestamp (Timestamp, showEpochTime)
import NetSpider.Unify (LinkSampleUnifier, LinkSample(..), LinkSampleID, linkSampleId)
import NetSpider.Weaver (Weaver, newWeaver)
import qualified NetSpider.Weaver as Weaver

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

-- | Connect the spider, run the given action and close the
-- connection.
--
-- @since 0.3.2.0
withSpider :: Config n na fla -> (Spider n na fla -> IO a) -> IO a
withSpider conf = bracket (connectWith conf) close

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
  
getNode :: (ToJSON n) => Spider n na fla -> n -> IO (Maybe (EID VNode))
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider gt
  where
    gt = gNodeEID <$.> gHasNodeID spider nid <*.> pure gAllNodes

getOrMakeNode :: (ToJSON n) => Spider n na fla -> n -> IO (EID VNode)
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
getSnapshotSimple :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na)
                  => Spider n na fla
                  -> n -- ^ ID of the node where it starts traversing.
                  -> IO (SnapshotGraph n na fla)
getSnapshotSimple spider start_nid = getSnapshot spider $ defQuery [start_nid]


-- | Get the snapshot graph from the history graph as specified by the
-- 'Query'.
getSnapshot :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na)
            => Spider n na fla
            -> Query n na fla sla
            -> IO (SnapshotGraph n na sla)
getSnapshot spider query = do
  ref_state <- newIORef $ initSnapshotState (startsFrom query) (foundNodePolicy query)
  recurseVisitNodesForSnapshot spider query ref_state
  (nodes, links, logs) <- fmap (makeSnapshot $ unifyLinkSamples query) $ readIORef ref_state
  mapM_ (logLine spider) logs
  return (nodes, links)

-- | Recursively traverse the history graph based on the query to get
-- the FoundNodes. It returns an action to get the new 'FoundNode' via
-- the response stream.
traverseFoundNodes :: (ToJSON n, NodeAttributes na, LinkAttributes fla, FromGraphSON n)
                   => Spider n na fla
                   -> Interval Timestamp -- ^ query time interval.
                   -> FoundNodePolicy n na -- ^ query found node policy
                   -> n -- ^ the starting node
                   -> IO (IO (Maybe (FoundNode n na fla)))
traverseFoundNodes spider time_interval fn_policy start_nid = do
  rhandle <- Gr.submit (spiderClient spider) gr_query (Just gr_binding)
  return $ do
    msmap <- Gr.nextResult rhandle
    maybe (return Nothing) (fmap Just . toFoundNode) msmap
  where
    toFoundNode smap = fmap makeFoundNodesFromHops $ extractFromSMap smap
    sourceVNode = gHasNodeID spider start_nid <*.> pure gAllNodes
    walkLatestFoundNodeIfOverwrite =
      case fn_policy of
        PolicyOverwrite -> gLatestFoundNode
        PolicyAppend -> gIdentity
    walkSelectFoundNode = (<<<) walkLatestFoundNodeIfOverwrite
                          <$> fmap gSelectFoundNode (gFilterFoundNodeByTime time_interval)
    repeat_until = Nothing -- TODO: specify the maximum number of traversals.
    ((gr_query, label_subject, label_vfnd, label_efs, label_efd, label_target), gr_binding) = runBinder $ do
      walk_select_fnode <- walkSelectFoundNode
      lsubject <- newAsLabel
      lefd <- newAsLabel
      ltarget <- newAsLabel
      lvfnd <- newAsLabel
      lefs <- newAsLabel
      let walk_finds_and_target =
            gProject
            ( gByL lefd gEFindsData )
            [ gByL ltarget (gNodeID spider <<< gFindsTarget)
            ]
            <<< gFinds
          walk_construct_result =
            gProject
            ( gByL lsubject (gSubjectNodeID spider) )
            [ gByL lvfnd gVFoundNodeData,
              gByL lefs (gFold <<< walk_finds_and_target)
            ]
      gt <- walk_construct_result
            <$.> gRepeat Nothing (walk_select_fnode <<< gSimplePath <<< gTraverseViaFinds)
                 repeat_until gEmitAlwaysHead
            <$.> walk_select_fnode
            <$.> sourceVNode
      return (gt, lsubject, lvfnd, lefs, lefd, ltarget)
    extractFromSMap smap = do
      subject <- lookupAsM label_subject smap
      vfnd <- lookupAsM label_vfnd smap
      efs <- lookupAsM label_efs smap
      parsed_efs <- mapM extractHopFromSMap efs
      return (subject, vfnd, parsed_efs)
    extractHopFromSMap smap =
      (,)
      <$> lookupAsM label_efd smap
      <*> lookupAsM label_target smap


recurseVisitNodesForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, Show n, LinkAttributes fla, NodeAttributes na)
                             => Spider n na fla
                             -> Query n na fla sla
                             -> IORef (SnapshotState n na fla)
                             -> IO ()
recurseVisitNodesForSnapshot spider query ref_state = go
  where
    go = do
      mnext_visit <- getNextVisit
      case mnext_visit of
       Nothing -> return ()
       Just next_visit -> do
         visitNodeForSnapshot spider query ref_state next_visit
         go
    getNextVisit = atomicModifyIORef' ref_state popUnvisitedNode
    -- TODO: limit number of steps.

-- | Traverse one hop from the visited 'VNode'.
--
-- Returns: list of (VFoundNode of the VNode, EFinds links), where an
-- EFinds link = (EFindsData, target Node Id)
traverseEFindsOneHop :: (FromGraphSON n, NodeAttributes na, LinkAttributes fla)
                     => Spider n na fla
                     -> Interval Timestamp
                     -> FoundNodePolicy n na
                     -> EID VNode
                     -> IO [(VFoundNodeData na, [(EFindsData fla, n)])]
traverseEFindsOneHop spider time_interval fn_policy visit_eid = getTraversedEdges
  where
    foundNodeTraversal = pure latestFoundNodeIfOverwrite
                         <*.> fmap gSelectFoundNode (gFilterFoundNodeByTime time_interval)
                         <*.> gHasNodeEID visit_eid
                         <*.> pure gAllNodes
    latestFoundNodeIfOverwrite =
      case fn_policy of
        PolicyOverwrite -> gLatestFoundNode
        PolicyAppend -> gIdentity

    getTraversedEdges = fmap V.toList $ traverse extractFromSMap =<< Gr.slurpResults =<< submitQuery
      where
        submitQuery = Gr.submit (spiderClient spider) query (Just bindings)
        ((query, label_vfnd, label_efs, label_efd, label_target_nid), bindings) = runBinder $ do
          lvfnd <- newAsLabel
          lefs <- newAsLabel
          lefd <- newAsLabel
          ltarget <- newAsLabel
          let gEFindsAndTarget =
                gProject
                ( gByL lefd gEFindsData )
                [ gByL ltarget (gNodeID spider <<< gFindsTarget)
                ]
                <<< gFinds
          gt <- gProject
                ( gByL lvfnd gVFoundNodeData )
                [ gByL lefs (gFold <<< gEFindsAndTarget)
                ]
                <$.> foundNodeTraversal
          return (gt, lvfnd, lefs, lefd, ltarget)
        extractFromSMap smap = do
          vfnd <- lookupAsM label_vfnd smap
          efs <- lookupAsM label_efs smap
          parsed_efs <- mapM extractHopFromSMap efs
          return (vfnd, parsed_efs)
        extractHopFromSMap smap =
          (,)
          <$> lookupAsM label_efd smap
          <*> lookupAsM label_target_nid smap

makeFoundNodesFromHops :: (n, VFoundNodeData na, [(EFindsData fla, n)]) -- ^ (Subject node ID, FoundNode data, hops)
                       -> FoundNode n na fla
makeFoundNodesFromHops (subject_nid, vfnd, efs) =
  makeFoundNode subject_nid vfnd $ map toFoundLink efs
  where
    toFoundLink (ef, target_nid) = makeFoundLink target_nid ef

visitNodeForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, Show n, LinkAttributes fla, NodeAttributes na)
                     => Spider n na fla
                     -> Query n na fla sla
                     -> IORef (SnapshotState n na fla)
                     -> n
                     -> IO ()
visitNodeForSnapshot spider query ref_state visit_nid = do
  logDebug spider ("Visiting node " <> spack visit_nid <> " ...")
  cur_state <- readIORef ref_state
  if isAlreadyVisited cur_state visit_nid
    then logAndQuit
    else doVisit
  where
    logAndQuit = do
      logDebug spider ("Node " <> spack visit_nid <> " is already visited. Skip.")
      return ()
    doVisit = do
      mvisit_eid <- getVisitedNodeEID
      case mvisit_eid of
       Nothing -> do
         logWarn spider ("Node " <> spack visit_nid <> " does not exist.")
         return ()
       Just visit_eid -> do
         found_nodes <- fmap ( map (\(vfnd, efs) -> makeFoundNodesFromHops (visit_nid, vfnd, efs)) )
                        $ traverseEFindsOneHop spider (timeInterval query) (foundNodePolicy query) visit_eid
         logFoundNodes found_nodes
         modifyIORef ref_state $ addFoundNodes visit_nid found_nodes
    getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gNodeEID <$.> gHasNodeID spider visit_nid <*.> pure gAllNodes
    logFoundNodes [] = logDebug spider ("No local finding is found for node " <> spack visit_nid)
    logFoundNodes fns = mapM_ logFoundNode $ Found.sortByTime fns
    logFoundNode fn = do
      let neighbors = neighborLinks fn
      logDebug spider
        ( "Node " <> (spack $ subjectNode fn)
          <> ": local finding at "  <> (showEpochTime $ foundAt fn)
          <> ", " <> (spack $ length neighbors) <> " neighbors"
        )
      mapM_ logFoundLink neighbors
    logFoundLink fl =
      logDebug spider
      ( "  Link is found to " <> (spack $ targetNode fl)
      )


-- | The state kept while making the snapshot graph.
data SnapshotState n na fla =
  SnapshotState
  { ssUnvisitedNodes :: Queue n,
    ssWeaver :: Weaver n na fla
  }
  deriving (Show)

-- debugShowState :: Show n => SnapshotState n na fla -> String
-- debugShowState state = unvisited ++ visited_nodes ++ visited_links
--   where
--     unvisited = "unvisitedNodes: "
--                 ++ (intercalate ", " $ map show $ toList $ ssUnvisitedNodes state)
--                 ++ "\n"
--     visited_nodes = "visitedNodes: "
--                     ++ (intercalate ", " $ map (uncurry showVisitedNode) $ HM.toList $ ssVisitedNodes state)
--                     ++ "\n"
--     showVisitedNode nid mvfn = show nid ++ "(" ++ (show $ fmap vfnTimestamp mvfn )  ++  ")"
--     visited_links = "visitedLinks: "
--                     ++ (intercalate ", " $ map show $ HM.keys $ ssVisitedLinks state)

emptySnapshotState :: FoundNodePolicy n na -> SnapshotState n na fla
emptySnapshotState p =
  SnapshotState
  { ssUnvisitedNodes = mempty,
    ssWeaver = newWeaver p
  }

initSnapshotState :: [n] -> FoundNodePolicy n na -> SnapshotState n na fla
initSnapshotState init_unvisited_nodes p =
  (emptySnapshotState p) { ssUnvisitedNodes = newQueue init_unvisited_nodes }

isAlreadyVisited :: (Eq n, Hashable n) => SnapshotState n na fla -> n -> Bool
isAlreadyVisited state nid = Weaver.isVisited nid $ ssWeaver state

popUnvisitedNode :: SnapshotState n na fla -> (SnapshotState n na fla, Maybe n)
popUnvisitedNode state = (updated, popped)
  where
    updated = state { ssUnvisitedNodes = updatedUnvisited }
    (popped, updatedUnvisited) = popQueue $ ssUnvisitedNodes state

makeSnapshot :: (Ord n, Hashable n, Show n)
             => LinkSampleUnifier n na fla sla
             -> SnapshotState n na fla
             -> ([SnapshotNode n na], [SnapshotLink n sla], [LogLine])
makeSnapshot unifier state = (nodes, links, logs)
  where
    ((nodes, links), logs) = Weaver.getSnapshot' unifier $ ssWeaver state

addFoundNodes :: (Eq n, Hashable n)
              => n -> [FoundNode n na fla] -> SnapshotState n na fla -> SnapshotState n na fla
addFoundNodes visited_nid [] state = state { ssWeaver = new_weaver }
  where
    new_weaver = Weaver.markAsVisited visited_nid $ ssWeaver state
addFoundNodes _ fns state = foldl' (\s fn -> addOneFoundNode fn s) state fns

addOneFoundNode :: (Eq n, Hashable n)
                => FoundNode n na fla -> SnapshotState n na fla -> SnapshotState n na fla
addOneFoundNode fn state = state { ssUnvisitedNodes = new_queue, ssWeaver = new_weaver }
  where
    new_weaver = Weaver.addFoundNode fn $ ssWeaver state
    new_boundary_nodes = filter (\n -> not $ Weaver.isVisited n new_weaver) $ allTargetNodes fn
    new_queue = foldl' (\q n -> pushQueue n q) (ssUnvisitedNodes state) new_boundary_nodes
