{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Internal.Graph
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Graph
       ( EID,
         VNode,
         VNeighbors,
         gAllNodes,
         gHasNodeID,
         gHasNodeEID,
         gNodeEID,
         gNodeID,
         gMakeNode,
         gMakeNeighbors,
         gClearAll,
         gSelectNeighbors,
         gLatestNeighbors,
         gFoundNodes
       ) where

import Control.Category ((<<<))
import Control.Monad (void)
import Data.Aeson (ToJSON, FromJSON)
import Data.Foldable (fold)
import Data.Greskell
  ( FromGraphSON,
    Element(..), Vertex,
    AVertexProperty, AEdge,
    GTraversal, Filter, Transform, SideEffect, Walk, liftWalk,
    Binder, newBind,
    source, sV, sV', sAddV, gHasLabel, gHasId, gHas2, gId, gProperty, gPropertyV, gV,
    gAddE, gSideEffect, gTo, gFrom, gDrop, gOut, gOrder, gBy2, gValues,
    ($.), (<*.>),
    ToGTraversal,
    Key, oDecr, gLimit
  )
import Data.Int (Int64)
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Vector (Vector)

import NetSpider.Neighbors (FoundLink(..))
import NetSpider.Timestamp (Timestamp(..))

-- | Generic element ID used in the graph DB.
newtype EID = EID (Either Int Text)
            deriving (Show,Eq,Ord,FromGraphSON,ToJSON,FromJSON)


-- | The \"node\" vertex.
data VNode

instance Element VNode where
  type ElementID VNode = EID
  type ElementProperty VNode = AVertexProperty

instance Vertex VNode

gNodeEID :: Walk Transform VNode EID
gNodeEID = gId

gNodeID :: Walk Transform VNode n
gNodeID = gValues ["@node_id"]

gAllNodes :: GTraversal Transform () VNode
gAllNodes = gHasLabel "node" $. sV [] $ source "g"

gHasNodeID :: ToJSON n => n -> Binder (Walk Filter VNode VNode)
gHasNodeID nid = do
  var_nid <- newBind nid
  return $ gHas2 "@node_id" var_nid

gHasNodeEID :: EID -> Binder (Walk Filter VNode VNode)
gHasNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeNode :: ToJSON n => n -> Binder (GTraversal SideEffect () VNode)
gMakeNode nid = do
  var_nid <- newBind nid
  return $ gProperty "@node_id" var_nid $. sAddV "node" $ source "g"

-- | The \"neighbors\" vertex.
data VNeighbors

instance Element VNeighbors where
  type ElementID VNeighbors = EID
  type ElementProperty VNeighbors = AVertexProperty

instance Vertex VNeighbors

gGetNodeByEID :: EID -> Binder (Walk Transform s VNode)
gGetNodeByEID vid = do
  f <- gHasNodeEID vid
  return (liftWalk f <<< gV [])

gMakeNeighbors :: (ToJSON p)
               => EID -- ^ subject node EID
               -> Vector (FoundLink n p, EID) -- ^ (link, target node EID)
               -> Timestamp
               -> Binder (GTraversal SideEffect () VNeighbors)
gMakeNeighbors subject_vid link_pairs timestamp = 
  mAddFindsEdges <*.> gSetTimestamp timestamp <*.> mAddObservesEdge <*.> pure $ sAddV "neighbors" $ source "g"
  where
    mAddObservesEdge :: Binder (Walk SideEffect VNeighbors VNeighbors)
    mAddObservesEdge = do
      v <- gGetNodeByEID subject_vid
      return $ gSideEffect $ emitsAEdge $ gAddE "observes" $ gFrom v
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: (ToJSON p) => (FoundLink n p, EID) -> Binder (Walk SideEffect VNeighbors VNeighbors)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_sp <- newBind $ subjectPort link
      var_tp <- newBind $ targetPort link
      return $ gSideEffect ( emitsAEdge
                             $ gProperty "@target_port" var_tp
                             <<< gProperty "@subject_port" var_sp
                             <<< gAddE "finds" (gTo v)
                             -- TODO: encode LinkState
                           )

keyTimestamp :: Key VNeighbors Int64
keyTimestamp = "@timestamp"
  
gSetTimestamp :: Timestamp -> Binder (Walk SideEffect VNeighbors VNeighbors)
gSetTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  return $ gPropertyV Nothing keyTimestamp var_epoch []
  -- TODO: set timezone.

emitsAEdge :: ToGTraversal g => g c s AEdge -> g c s AEdge
emitsAEdge = id

gClearAll :: GTraversal SideEffect () ()
gClearAll = void $ gDrop $. liftWalk $ sV' [] $ source "g"

gSelectNeighbors :: Walk Filter VNeighbors VNeighbors -> Walk Transform VNode VNeighbors
gSelectNeighbors filterNeighbors = liftWalk filterNeighbors <<< gOut ["observes"]

gLatestNeighbors :: Walk Transform VNeighbors VNeighbors
gLatestNeighbors = gLimit 1 <<< gOrder [gBy2 keyTimestamp oDecr]

gFoundNodes :: Walk Transform VNeighbors VNode
gFoundNodes = gOut ["finds"]
