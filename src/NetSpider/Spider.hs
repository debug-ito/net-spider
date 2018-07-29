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
import Data.Greskell
  ( runBinder, ($.), (<$.>), (<*.>),
    Binder, ToGreskell(GreskellReturn), AsIterator(IteratorItem), FromGraphSON,
    liftWalk, gLimit, gIdentity
  )
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
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
import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Spider.Internal.Graph
  ( EID, gMakeNeighbors, gAllNodes, gHasNodeID, gHasNodeEID, gNodeEID, gNodeID, gMakeNode, gClearAll,
    gLatestNeighbors, gSelectNeighbors
  )

-- | An IO agent of the NetSpider database.
data Spider =
  Spider
  { spiderClient :: Gr.Client
  }

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Host -> Port -> IO Spider
connectWS host port = fmap Spider $ Gr.connect host port

-- | Close and release the 'Spider' object.
close :: Spider -> IO ()
close sp = Gr.close $ spiderClient sp

submitB :: (ToGreskell g, r ~ GreskellReturn g, AsIterator r, v ~ IteratorItem r, FromGraphSON v)
        => Spider -> Binder g -> IO (Gr.ResultHandle v)
submitB sp b = Gr.submit (spiderClient sp) script mbs
  where
    (script, bs) = runBinder b
    mbs = Just bs

-- | Add an observation of 'Neighbors' to the NetSpider database.
addNeighbors :: (ToJSON n, ToJSON p) => Spider -> Neighbors n p -> IO ()
addNeighbors spider nbs = do
  subject_vid <- getOrMakeNode spider $ subjectNode nbs
  link_pairs <- traverse linkAndTargetVID $ neighborLinks nbs
  makeNeighborsVertex spider subject_vid link_pairs $ observedTime nbs
  where
    linkAndTargetVID link = do
      target_vid <- getOrMakeNode spider $ targetNode link
      return (link, target_vid)

makeNeighborsVertex :: (ToJSON p)
                    => Spider
                    -> EID -- ^ subject node vertex ID
                    -> Vector (FoundLink n p, EID) -- ^ (link, target node vertex ID)
                    -> Timestamp
                    -> IO ()
makeNeighborsVertex spider subject_vid link_pairs timestamp =
  Gr.drainResults =<< submitB spider (fmap void $ gMakeNeighbors subject_vid link_pairs timestamp)

vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider -> n -> IO (Maybe EID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider gt
  where
    gt = gNodeEID <$.> (fmap liftWalk $ gHasNodeID nid) <*.> pure gAllNodes

getOrMakeNode :: (ToJSON n) => Spider -> n -> IO EID
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

-- | Get the latest snapshot graph from the NetSpider database.
--
-- This function is very simple, and should be used only for testing.
-- This function starts from an arbitrary node, traverses the history
-- graph using the latest links with unlimited number of hops.
getLatestSnapshot :: (FromGraphSON n, ToJSON n, Eq n, Eq p, Hashable n, Hashable p)
                  => Spider -> IO (Vector (SnapshotElement n p))
getLatestSnapshot spider = do
  mstart_nid <- getStartNode
  case mstart_nid of
   Nothing -> return mempty
   Just start_nid -> do
     ref_state <- newIORef emptySnapshotState
     visitNodeForSnapshot spider ref_state start_nid
     -- TODO: recursively update the state.
     fmap makeSnapshot $ readIORef ref_state
  where
    getStartNode = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = return $ gNodeID $. gLimit 1 $. gAllNodes

visitNodeForSnapshot :: (ToJSON n, Eq n, Hashable n)
                     => Spider
                     -> IORef (SnapshotState n p)
                     -> n
                     -> IO ()
visitNodeForSnapshot spider ref_state visit_nid = do
  markAsVisited
  mnode_eid <- getVisitedNodeEID
  case mnode_eid of
   Nothing -> return ()
   Just node_eid -> undefined -- TODO
  where
    markAsVisited = modifyIORef ref_state $ addVisitedNode visit_nid
    getVisitedNodeEID = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = gNodeEID <$.> (fmap liftWalk $ gHasNodeID visit_nid) <*.> pure gAllNodes
    getNextNeighbors node_vid = fmap vToMaybe $ Gr.slurpResults =<< submitB spider binder
      where
        binder = fmap void
                 $ gLatestNeighbors
                 <$.> gSelectNeighbors gIdentity -- TODO: select Neighbors to consider
                 <$.> (fmap liftWalk $ gHasNodeEID node_vid)
                 <*.> pure gAllNodes
  

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider -> IO ()
clearAll spider = Gr.drainResults =<< submitB spider (return gClearAll)

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

addVisitedNode :: (Eq n, Hashable n) => n -> SnapshotState n p -> SnapshotState n p
addVisitedNode nid state = state { ssVisitedNodes = HS.insert nid $ ssVisitedNodes state }

addUnvisitedNode :: n -> SnapshotState n p -> SnapshotState n p
addUnvisitedNode nid state = state { ssUnvisitedNodes = V.snoc (ssUnvisitedNodes state) nid }

makeSnapshot :: SnapshotState n p -> Vector (SnapshotElement n p)
makeSnapshot = undefined -- TODO
