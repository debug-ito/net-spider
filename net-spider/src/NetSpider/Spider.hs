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
import Control.Monad (void, mapM_, mapM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (foldr', toList, foldl')
import Data.List (intercalate, reverse)
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Greskell, Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity, gSelect1, gAs, gProject, gByL, gIdentity, gFold,
    gRepeat, gEmitHead, gSimplePath, gConstant, gLocal,
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
  init_weaver <- readIORef ref_weaver
  get_next <- traverseFoundNodes spider time_interval fn_policy start_nid
  doTraverseWith init_weaver get_next
  where
    doTraverseWith init_weaver getNext = go
      where
        go = do
          mvisited_node <- getNext
          case mvisited_node of
            Nothing -> return ()
            Just (Left sub_nid) -> tryAdd sub_nid Nothing >> go
            Just (Right fnode) -> tryAdd (subjectNode fnode) (Just fnode) >> go
        tryAdd sub_nid mfnode = do
          when (not $ Weaver.isVisited sub_nid init_weaver) $ do
            modifyIORef ref_weaver $ \w ->
              case mfnode of
                Nothing -> Weaver.markAsVisited sub_nid w
                Just fnode -> Weaver.addFoundNode fnode w

-- | Recursively traverse the history graph based on the query to get
-- the FoundNodes.
--
-- It returns an action that emits the visited Node IDs and
-- 'FoundNode's the node has. Those items are emitted as a single
-- stream, in a mixed and unordered fashion. If it reaches to the end
-- of the stream, the action returns 'Nothing'.
traverseFoundNodes :: (ToJSON n, NodeAttributes na, LinkAttributes fla, FromGraphSON n)
                   => Spider n na fla
                   -> Interval Timestamp -- ^ query time interval.
                   -> FoundNodePolicy n na -- ^ query found node policy
                   -> n -- ^ the starting node
                   -> IO (IO (Maybe (Either n (FoundNode n na fla))))
traverseFoundNodes spider time_interval fn_policy start_nid = do
  rhandle <- Gr.submit (spiderClient spider) gr_query (Just gr_binding)
  return $ do
    msmap <- Gr.nextResult rhandle
    maybe (return Nothing) (fmap Just . extractSMap) msmap
  where
    sourceVNode = gHasNodeID spider start_nid <*.> pure gAllNodes
    walkLatestFoundNodeIfOverwrite =
      case fn_policy of
        PolicyOverwrite -> gLatestFoundNode
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
      let walk_select_mixed = gLocal $ gNodeMix walk_select_fnode -- gLocal is necessary because we may have .limit() step inside.
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
            [ gByL lsubject (gSubjectNodeID spider),
              gByL lvfnd gVFoundNodeData,
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
        "vfn" -> fmap Right $ extractFoundNode smap
        _ -> throwString ("Unknow type of traversal result: " ++ show got_type)
        -- TODO: make decent exception type
    extractSubjectNodeID smap = lookupAsM label_subject smap
    extractFoundNode smap = do
      sub_nid <- lookupAsM label_subject smap
      vfnd <- lookupAsM label_vfnd smap
      efs <- lookupAsM label_efs smap
      parsed_efs <- mapM extractHopFromSMap efs
      return $ makeFoundNodeFromHops sub_nid (vfnd, parsed_efs)
    extractHopFromSMap smap =
      (,)
      <$> lookupAsM label_efd smap
      <*> lookupAsM label_target smap

makeFoundNodeFromHops :: n -- ^ Subject node ID
                       -> (VFoundNodeData na, [(EFindsData fla, n)]) -- ^ (FoundNode data, hops)
                       -> FoundNode n na fla
makeFoundNodeFromHops subject_nid (vfnd, efs) =
  makeFoundNode subject_nid vfnd $ map toFoundLink efs
  where
    toFoundLink (ef, target_nid) = makeFoundLink target_nid ef


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

