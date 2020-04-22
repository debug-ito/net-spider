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
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit as Con
import Data.Foldable (foldr', toList, foldl')
import Data.List (intercalate, reverse)
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Greskell, Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity, gSelect1, gAs, gProject, gByL, gIdentity, gFold,
    gRepeat, gEmitHead, gSimplePath, gConstant,
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
    gNodeMix, gFoundNodeOnly, gEitherNodeMix
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
  let fn_policy = foundNodePolicy query
  ref_weaver <- newIORef $ newWeaver fn_policy
  mapM_ (traverseFromOneNode spider (timeInterval query) fn_policy ref_weaver) $ startsFrom query
  (graph, logs) <- fmap (Weaver.getSnapshot' $ unifyLinkSamples query) $ readIORef ref_weaver
  mapM_ (logLine spider) logs
  return graph

traverseFromOneNode :: (Eq n, Hashable n, ToJSON n, NodeAttributes na, LinkAttributes fla, FromGraphSON n)
                    => Spider n na fla
                    -> Interval Timestamp
                    -> FoundNodePolicy n na
                    -> IORef (Weaver n na fla)
                    -> n -- ^ starting node
                    -> IO ()
traverseFromOneNode spider time_interval fn_policy ref_weaver start_nid = do
  visited_node_stream <- traverseFoundNodes spider time_interval fn_policy start_nid
  go $ Con.connect visited_node_stream Con.await
  where
    go getNext = do
      mvisited_node <- getNext
      case mvisited_node of
        Nothing -> return ()
        Just (sub_nid, fnodes) -> modifyIORef ref_weaver $ tryAddToWeaver sub_nid fnodes
    tryAddToWeaver sub_nid fnodes weaver =
      if Weaver.isVisited sub_nid weaver
      then weaver
      else case fnodes of
        [] -> Weaver.markAsVisited sub_nid weaver
        _ -> foldl' (\w fn -> Weaver.addFoundNode fn w ) weaver fnodes

resultHandleConduit :: Gr.ResultHandle a -> ConduitT () a IO ()
resultHandleConduit handle = go
  where
    go = do
      mret <- liftIO $ Gr.nextResult handle
      case mret of
        Nothing -> return ()
        Just ret -> do
          Con.yield ret
          go

-- | Recursively traverse the history graph based on the query to get
-- the FoundNodes.
--
-- It returns a Conduit source that emits visited Node ID and
-- 'FoundNode's the node has.
traverseFoundNodes :: (ToJSON n, NodeAttributes na, LinkAttributes fla, FromGraphSON n)
                   => Spider n na fla
                   -> Interval Timestamp -- ^ query time interval.
                   -> FoundNodePolicy n na -- ^ query found node policy
                   -> n -- ^ the starting node
                   -> IO (ConduitT () (n, [FoundNode n na fla]) IO ())
traverseFoundNodes spider time_interval fn_policy start_nid = do
  rhandle <- Gr.submit (spiderClient spider) gr_query (Just gr_binding)
  return $ resultHandleConduit rhandle .| parseSMapStream

  -- Implementation note: the Gremlin query makes the server output
  -- mixed stream of VNode and VFoundNode. It first outputs a visited
  -- VNode, then it outputs zero or more VFoundNode found from the
  -- visited VNode, and it outputs a VNode it visits next, and so on.
  --
  -- [VNode, VFoundNode, VFoundNode, VNode, VNode, VFoundNode, ...]
  --
  -- The reason why we do this is that we need to make a list of
  -- visited nodes, but it's possible that a VNode has no
  -- VFoundNodes. In addition, number of VFoundNodes can be big (it
  -- depends on the query), so we don't want to let the server fold
  -- VFoundNodes into a list.
  
  where
    sourceVNode = gHasNodeID spider start_nid <*.> pure gAllNodes
    walkLatestFoundNodeIfOverwrite =
      case fn_policy of
        PolicyOverwrite -> gLatestFoundNode -- TODO(?): maybe we need to use .local step???
        PolicyAppend -> gIdentity
    walkSelectFoundNode = (<<<) walkLatestFoundNodeIfOverwrite
                          <$> fmap gSelectFoundNode (gFilterFoundNodeByTime time_interval)
    repeat_until = Nothing -- TODO: specify the maximum number of traversals.
    ((gr_query, label_smap_type, label_subject, label_vfnd, label_efs, label_efd, label_target), gr_binding) = runBinder $ do
      walk_select_fnode <- walkSelectFoundNode
      lsmap_type <- newAsLabel
      lsubject <- newAsLabel
      lefd <- newAsLabel
      ltarget <- newAsLabel
      lvfnd <- newAsLabel
      lefs <- newAsLabel
      let walk_select_mixed = gNodeMix walk_select_fnode
          walk_finds_and_target =
            gProject
            ( gByL lefd gEFindsData )
            [ gByL ltarget (gNodeID spider <<< gFindsTarget)
            ]
            <<< gFinds
          walk_construct_result = gEitherNodeMix walk_construct_vnode walk_construct_vfnode
          walk_construct_vnode =
            gProject
            ( gByL lsmap_type (gConstant ("vn" :: Greskell Text)) )
            [ gByL lsubject (gNodeID spider)
            ]
          walk_construct_vfnode =
            gProject
            ( gByL lsmap_type (gConstant ("vfn" :: Greskell Text)) )
            [ gByL lvfnd gVFoundNodeData,
              gByL lefs (gFold <<< walk_finds_and_target)
            ]
      gt <- walk_construct_result
            <$.> gRepeat Nothing repeat_until gEmitHead
                 (walk_select_mixed <<< gSimplePath <<< gTraverseViaFinds <<< gFoundNodeOnly)
            <$.> walk_select_mixed
            <$.> sourceVNode
      return (gt, lsmap_type, lsubject, lvfnd, lefs, lefd, ltarget)
    extractSMap smap = do
      got_type <- lookupAsM label_smap_type smap
      case got_type of
        "vn" -> fmap Left $ extractSubjectNodeID smap
        "vfn" -> fmap Right $ extractFoundNodeData smap
        _ -> throwString ("Unknow type of traversal result: " ++ show got_type)
        -- TODO: make decent exception type
    extractSubjectNodeID smap = lookupAsM label_subject smap
    extractFoundNodeData smap = do
      vfnd <- lookupAsM label_vfnd smap
      efs <- lookupAsM label_efs smap
      parsed_efs <- mapM extractHopFromSMap efs
      return (vfnd, parsed_efs)
    extractHopFromSMap smap =
      (,)
      <$> lookupAsM label_efd smap
      <*> lookupAsM label_target smap
    parseSMapStream = go Nothing []
      where
        go mcur_sub_nid cur_fnodes = do
          msmap <- Con.await
          case msmap of
            Nothing -> yieldResult mcur_sub_nid cur_fnodes
            Just smap -> do
              eret <- liftIO $ extractSMap smap
              case eret of
                Left next_sub_nid -> do
                  yieldResult mcur_sub_nid cur_fnodes
                  go (Just next_sub_nid) []
                Right fnode_data -> do
                  case mcur_sub_nid of
                    Nothing -> unexpectedVFNode
                    Just cur_sub_nid ->
                      go mcur_sub_nid (makeFoundNodeFromHops cur_sub_nid fnode_data : cur_fnodes)
        yieldResult msub_nid fnodes =
          case msub_nid of
            Nothing -> return ()
            Just sub_nid -> Con.yield (sub_nid, reverse fnodes)
        unexpectedVFNode =
          throwString ("Unexpected VFoundNode result received before receiving VNode result.")
          -- TODO: make decent exception type


---- recurseVisitNodesForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, Show n, LinkAttributes fla, NodeAttributes na)
----                              => Spider n na fla
----                              -> Query n na fla sla
----                              -> IORef (SnapshotState n na fla)
----                              -> IO ()
---- recurseVisitNodesForSnapshot spider query ref_state = go
----   where
----     go = do
----       mnext_visit <- getNextVisit
----       case mnext_visit of
----        Nothing -> return ()
----        Just next_visit -> do
----          visitNodeForSnapshot spider query ref_state next_visit
----          go
----     getNextVisit = atomicModifyIORef' ref_state popUnvisitedNode
----     -- TODO: limit number of steps.

---- -- | Traverse one hop from the visited 'VNode'.
---- --
---- -- Returns: list of (VFoundNode of the VNode, EFinds links), where an
---- -- EFinds link = (EFindsData, target Node Id)
---- traverseEFindsOneHop :: (FromGraphSON n, NodeAttributes na, LinkAttributes fla)
----                      => Spider n na fla
----                      -> Interval Timestamp
----                      -> FoundNodePolicy n na
----                      -> EID VNode
----                      -> IO [(VFoundNodeData na, [(EFindsData fla, n)])]
---- traverseEFindsOneHop spider time_interval fn_policy visit_eid = getTraversedEdges
----   where
----     foundNodeTraversal = pure latestFoundNodeIfOverwrite
----                          <*.> fmap gSelectFoundNode (gFilterFoundNodeByTime time_interval)
----                          <*.> gHasNodeEID visit_eid
----                          <*.> pure gAllNodes
----     latestFoundNodeIfOverwrite =
----       case fn_policy of
----         PolicyOverwrite -> gLatestFoundNode
----         PolicyAppend -> gIdentity
---- 
----     getTraversedEdges = fmap V.toList $ traverse extractFromSMap =<< Gr.slurpResults =<< submitQuery
----       where
----         submitQuery = Gr.submit (spiderClient spider) query (Just bindings)
----         ((query, label_vfnd, label_efs, label_efd, label_target_nid), bindings) = runBinder $ do
----           lvfnd <- newAsLabel
----           lefs <- newAsLabel
----           lefd <- newAsLabel
----           ltarget <- newAsLabel
----           let gEFindsAndTarget =
----                 gProject
----                 ( gByL lefd gEFindsData )
----                 [ gByL ltarget (gNodeID spider <<< gFindsTarget)
----                 ]
----                 <<< gFinds
----           gt <- gProject
----                 ( gByL lvfnd gVFoundNodeData )
----                 [ gByL lefs (gFold <<< gEFindsAndTarget)
----                 ]
----                 <$.> foundNodeTraversal
----           return (gt, lvfnd, lefs, lefd, ltarget)
----         extractFromSMap smap = do
----           vfnd <- lookupAsM label_vfnd smap
----           efs <- lookupAsM label_efs smap
----           parsed_efs <- mapM extractHopFromSMap efs
----           return (vfnd, parsed_efs)
----         extractHopFromSMap smap =
----           (,)
----           <$> lookupAsM label_efd smap
----           <*> lookupAsM label_target_nid smap

makeFoundNodeFromHops :: n -- ^ Subject node ID
                       -> (VFoundNodeData na, [(EFindsData fla, n)]) -- ^ (FoundNode data, hops)
                       -> FoundNode n na fla
makeFoundNodeFromHops subject_nid (vfnd, efs) =
  makeFoundNode subject_nid vfnd $ map toFoundLink efs
  where
    toFoundLink (ef, target_nid) = makeFoundLink target_nid ef

---- visitNodeForSnapshot :: (ToJSON n, Ord n, Hashable n, FromGraphSON n, Show n, LinkAttributes fla, NodeAttributes na)
----                      => Spider n na fla
----                      -> Query n na fla sla
----                      -> IORef (SnapshotState n na fla)
----                      -> n
----                      -> IO ()
---- visitNodeForSnapshot spider query ref_state visit_nid = do
----   logDebug spider ("Visiting node " <> spack visit_nid <> " ...")
----   cur_state <- readIORef ref_state
----   if isAlreadyVisited cur_state visit_nid
----     then logAndQuit
----     else doVisit
----   where
----     logAndQuit = do
----       logDebug spider ("Node " <> spack visit_nid <> " is already visited. Skip.")
----       return ()
----     doVisit = do
----       mvisit_eid <- getVisitedNodeEID
----       case mvisit_eid of
----        Nothing -> do
----          logWarn spider ("Node " <> spack visit_nid <> " does not exist.")
----          return ()
----        Just visit_eid -> do
----          found_nodes <- fmap ( map (\(vfnd, efs) -> makeFoundNodesFromHops (visit_nid, vfnd, efs)) )
----                         $ traverseEFindsOneHop spider (timeInterval query) (foundNodePolicy query) visit_eid
----          logFoundNodes found_nodes
----          modifyIORef ref_state $ addFoundNodes visit_nid found_nodes
----     getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
----       where
----         binder = gNodeEID <$.> gHasNodeID spider visit_nid <*.> pure gAllNodes
----     logFoundNodes [] = logDebug spider ("No local finding is found for node " <> spack visit_nid)
----     logFoundNodes fns = mapM_ logFoundNode $ Found.sortByTime fns
----     logFoundNode fn = do
----       let neighbors = neighborLinks fn
----       logDebug spider
----         ( "Node " <> (spack $ subjectNode fn)
----           <> ": local finding at "  <> (showEpochTime $ foundAt fn)
----           <> ", " <> (spack $ length neighbors) <> " neighbors"
----         )
----       mapM_ logFoundLink neighbors
----     logFoundLink fl =
----       logDebug spider
----       ( "  Link is found to " <> (spack $ targetNode fl)
----       )


---- -- | The state kept while making the snapshot graph.
---- data SnapshotState n na fla =
----   SnapshotState
----   { ssUnvisitedNodes :: Queue n,
----     ssWeaver :: Weaver n na fla
----   }
----   deriving (Show)

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

---- emptySnapshotState :: FoundNodePolicy n na -> SnapshotState n na fla
---- emptySnapshotState p =
----   SnapshotState
----   { ssUnvisitedNodes = mempty,
----     ssWeaver = newWeaver p
----   }
---- 
---- initSnapshotState :: [n] -> FoundNodePolicy n na -> SnapshotState n na fla
---- initSnapshotState init_unvisited_nodes p =
----   (emptySnapshotState p) { ssUnvisitedNodes = newQueue init_unvisited_nodes }
---- 
---- isAlreadyVisited :: (Eq n, Hashable n) => SnapshotState n na fla -> n -> Bool
---- isAlreadyVisited state nid = Weaver.isVisited nid $ ssWeaver state
---- 
---- popUnvisitedNode :: SnapshotState n na fla -> (SnapshotState n na fla, Maybe n)
---- popUnvisitedNode state = (updated, popped)
----   where
----     updated = state { ssUnvisitedNodes = updatedUnvisited }
----     (popped, updatedUnvisited) = popQueue $ ssUnvisitedNodes state
---- 
---- makeSnapshot :: (Ord n, Hashable n, Show n)
----              => LinkSampleUnifier n na fla sla
----              -> SnapshotState n na fla
----              -> ([SnapshotNode n na], [SnapshotLink n sla], [LogLine])
---- makeSnapshot unifier state = (nodes, links, logs)
----   where
----     ((nodes, links), logs) = Weaver.getSnapshot' unifier $ ssWeaver state
---- 
---- addFoundNodes :: (Eq n, Hashable n)
----               => n -> [FoundNode n na fla] -> SnapshotState n na fla -> SnapshotState n na fla
---- addFoundNodes visited_nid [] state = state { ssWeaver = new_weaver }
----   where
----     new_weaver = Weaver.markAsVisited visited_nid $ ssWeaver state
---- addFoundNodes _ fns state = foldl' (\s fn -> addOneFoundNode fn s) state fns
---- 
---- addOneFoundNode :: (Eq n, Hashable n)
----                 => FoundNode n na fla -> SnapshotState n na fla -> SnapshotState n na fla
---- addOneFoundNode fn state = state { ssUnvisitedNodes = new_queue, ssWeaver = new_weaver }
----   where
----     new_weaver = Weaver.addFoundNode fn $ ssWeaver state
----     new_boundary_nodes = filter (\n -> not $ Weaver.isVisited n new_weaver) $ allTargetNodes fn
---- 
