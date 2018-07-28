{-# LANGUAGE OverloadedStrings #-}
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

import Control.Category ((<<<))
import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Foldable (fold)
import Data.Greskell
  ( Walk, SideEffect, Vertex,
    AVertex,
    source, sV', sAddV',
    ($.), (<*.>), gDrop, liftWalk, gHas2, gId, gProperty, gPropertyV, gHasLabel,
    gSideEffect, gAddE', gFrom, gTo, gV',
    GValue,
    runBinder, newBind, Binder
  )
import Data.Traversable (traverse)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Greskell.WebSocket
  ( Host, Port
  )
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Neighbors (Neighbors(..), FoundLink(..))
import NetSpider.Snapshot (SnapshotElement)
import NetSpider.Timestamp (Timestamp(..))

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
                    -> VertexID -- ^ subject node VID
                    -> Vector (FoundLink n p, VertexID) -- ^ (link, target node VID)
                    -> Timestamp
                    -> IO ()
makeNeighborsVertex spider subject_vid link_pairs timestamp =
  Gr.drainResults =<< Gr.submit (spiderClient spider) script mbindings
  where
    (script, bindings) = runBinder
                         $ mAddFindsEdges
                         <*.> setTimestamp timestamp
                         <*.> mAddObservesEdge
                         <*.> pure $ sAddV' "neighbors" $ source "g"
    mbindings = Just bindings
    mAddObservesEdge = do
      v <- mVertexById subject_vid
      return $ gSideEffect $ gAddE' "observes" $ gFrom v
    mVertexById vid = do
      var_vid <- newBind vid
      return $ gV' [var_vid]
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: (ToJSON p) => (FoundLink n p, VertexID) -> Binder (Walk SideEffect AVertex AVertex)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- mVertexById target_vid
      var_sp <- newBind $ subjectPort link
      var_tp <- newBind $ targetPort link
      return $ gSideEffect ( gProperty "@target_port" var_tp
                             <<< gProperty "@subject_port" var_sp
                             <<< gAddE' "finds" (gTo v)
                             -- TODO: encode LinkState
                           )

setTimestamp :: Timestamp -> Binder (Walk SideEffect AVertex AVertex)
setTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  return $ gPropertyV Nothing "@timestamp" var_epoch []
  -- TODO: set timezone.

-- | ID of the Vertex kept internally in the graph DB.
type VertexID = GValue

-- TODO: is it OK to use GValue naively for ID? It's bad for the code
-- to depend on GraphSON encoding..



vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider -> n -> IO (Maybe VertexID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
  where
    (script, bindings) = runBinder $ do
      var_nid <- newBind nid
      return $ gId $. gHas2 "@node_id" var_nid $. gHasLabel "node" $. sV' [] $ source "g"
    mbindings = Just bindings

getOrMakeNode :: (ToJSON n) => Spider -> n -> IO VertexID
getOrMakeNode spider nid = do
  mvid <- getNode spider nid
  case mvid of
   Just vid -> return vid
   Nothing -> makeNode
  where
    makeNode = expectOne =<< Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
    (script, bindings) = runBinder $ do
      var_nid <- newBind nid
      return $ liftWalk gId $. gProperty "@node_id" var_nid $. sAddV' "node" $ source "g"
    mbindings = Just bindings
    expectOne v = case vToMaybe v of
      Just e -> return e
      Nothing -> throwString "Expects at least single result, but got nothing."
      -- TODO: make decent exception spec.

-- | Get the latest snapshot graph from the NetSpider database.
--
-- This function is very simple, and should be used only for testing.
-- This function starts from an arbitrary node, traverses the history
-- graph using the latest links with unlimited number of hops.
getLatestSnapshot :: Spider -> IO (Vector (SnapshotElement n p))
getLatestSnapshot = undefined

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider -> IO ()
clearAll spider = Gr.drainResults =<< Gr.submit (spiderClient spider) script Nothing
  where
    script = void $ gDrop $. liftWalk $ sV' [] $ source "g"

-- We can create much more complex function to query snapshot graphs,
-- but at least we need 'getLatestSnapshot'.
