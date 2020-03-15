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
    liftWalk, gLimit, gIdentity, gSelect1, gAs, gProject, gByL, gIdentity,
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
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
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
    gFilterFoundNodeByTime
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

-- | Returns the result of query: (latest VFoundNode, list of traversed edges)
traverseEFindsOneHop :: (FromGraphSON n, NodeAttributes na, LinkAttributes fla)
                     => Spider n na fla
                     -> Interval Timestamp
                     -> EID VNode
                     -> IO [(VFoundNodeData na, EFindsData fla, n)]
traverseEFindsOneHop spider time_interval visit_eid = getTraversedEdges
  where
    foundNodeTraversal = fmap gSelectFoundNode (gFilterFoundNodeByTime time_interval)
                         <*.> gHasNodeEID visit_eid
                         <*.> pure gAllNodes
    -- getLatestVFoundNode = fmap vToMaybe $ Gr.slurpResults =<< submitQuery
    --   where
    --     submitQuery = submitB spider binder
    --     binder = gVFoundNodeData <$.> gLatestFoundNode <$.> foundNodeTraversal
    getTraversedEdges = fmap V.toList $ traverse extractFromSMap =<< Gr.slurpResults =<< submitQuery
      where
        submitQuery = Gr.submit (spiderClient spider) query (Just bindings)
        ((query, label_vfn, label_ef, label_target_nid), bindings) = runBinder $ do
          lvfn <- newAsLabel
          lvfnd <- newAsLabel
          lef <- newAsLabel
          lefd <- newAsLabel
          ln <- newAsLabel
          gt <- gProject
                ( gByL lvfnd (gVFoundNodeData <<< gSelect1 lvfn) )
                [ gByL lefd (gEFindsData <<< gSelect1 lef),
                  gByL ln (gIdentity :: Walk Transform n n)
                ] <$.> gNodeID spider <$.> gFindsTarget
                <$.> gAs lef <$.> gFinds <$.> gAs lvfn <$.> foundNodeTraversal
          return (gt, lvfnd, lefd, ln)
        extractFromSMap smap =
          (,,)
          <$> lookupAsM label_vfn smap 
          <*> lookupAsM label_ef smap 
          <*> lookupAsM label_target_nid smap 

-- | Group hops based on 'VFoundNodeData'. Equality of
-- 'VFoundNodeData' is based on 'vfnId'.
groupHopsOnVFN :: [(VFoundNodeData na, EFindsData fla, n)]
               -> [(VFoundNodeData na, [(EFindsData fla, n)])]
groupHopsOnVFN = undefined
-- TODO: use GHC.Exts.groupWith. However, 'EID' doesn't implement 'Ord'. What should we do?

makeFoundNodesFromHops :: n -- ^ Subject node ID
                       -> [(VFoundNodeData na, EFindsData fla, n)] -- ^ Hops
                       -> [FoundNode n na fla]
makeFoundNodesFromHops subject_nid hops = map toFoundNode $ groupHopsOnVFN hops
  where
    toFoundNode (vfn, edges) = makeFoundNode subject_nid vfn $ map toFoundLink edges
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
         found_nodes <- fmap (makeFoundNodesFromHops visit_nid) $ traverseEFindsOneHop spider (timeInterval query) visit_eid
         logFoundNodes found_nodes
         modifyIORef ref_state $ addFoundNodes visit_nid found_nodes
         
         ---- markAsVisited $ mlatest_vfn
         ---- logLatestVFN mlatest_vfn
         ---- let next_hops = filterHops mlatest_vfn $ hops
         
         -- putStrLn ("-- visit " ++ show visit_nid)
         -- putStrLn ("   latest vfn: " ++ (show $ fmap vfnTimestamp mlatest_vfn))
         -- forM_ next_hops $ \(vfn, _, next_nid) -> do
         --   putStrLn ("   next: " ++ (show $ vfnTimestamp vfn) ++ " -> " ++ show next_nid)

         -- TODO: adapt logHop
         
         -- mapM_ logHop next_hops
    getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gNodeEID <$.> gHasNodeID spider visit_nid <*.> pure gAllNodes
    -- markAsVisited mvfn = modifyIORef ref_state $ addVisitedNode visit_nid mvfn
    logFoundNodes fns = logLatestFN $ listToMaybe $ Found.sortByTime fns
    logLatestFN Nothing = logDebug spider ("No local finding is found for node " <> spack visit_nid)
    logLatestFN (Just fn) = logDebug spider ( "Node " <> spack visit_nid
                                              <> ": latest local finding is at "
                                              <> (showEpochTime $ foundAt fn)
                                            )
    -- uncurry3 f (a,b,c) = f a b c
    
    -- filterHops mvfn =
    --   case foundNodePolicy query of
    --    PolicyOverwrite -> filter (hopHasVFN mvfn)
    --    PolicyAppend -> id
    -- hopHasVFN Nothing _ = True
    -- hopHasVFN (Just vfn1) (vfn2, _, _) = vfnId vfn1 == vfnId vfn2
    
    -- logHop (vfn, _, next_nid) = logDebug spider ( "Link " <> spack visit_nid
    --                                               <> " -> " <> spack next_nid
    --                                               <> " at " <> (showEpochTime $ vfnTimestamp vfn)
    --                                             )

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
addFoundNodes _ fns state = state { ssWeaver = new_weaver }
  where
    old_weaver = ssWeaver state
    new_weaver = foldl' (\w fn -> Weaver.addFoundNode fn w) old_weaver fns

  -- TODO: how to deal with unvisited nodes queue??
    

--------------------

---- addLinkSample :: (Ord n, Hashable n)
----               => LinkSample n fla -> SnapshotState n na fla -> SnapshotState n na fla
---- addLinkSample ls state = state { ssVisitedLinks = updatedLinks,
----                                  ssUnvisitedNodes = updatedUnvisited
----                                }
----   where
----     link_id = linkSampleId ls
----     updatedLinks = HM.insertWith (++) link_id (return ls) $ ssVisitedLinks state
----     target_nid = lsTargetNode ls
----     target_already_visited = isAlreadyVisited state target_nid
----     updatedUnvisited = if target_already_visited
----                        then ssUnvisitedNodes state
----                        else pushQueue target_nid $ ssUnvisitedNodes state
---- 
---- addLinkSamples :: (Ord n, Hashable n)
----                => [LinkSample n fla] -> SnapshotState n na fla -> SnapshotState n na fla
---- addLinkSamples links orig_state = foldr' addLinkSample orig_state links

