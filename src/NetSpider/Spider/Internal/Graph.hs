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
         gGetNode,
         gMakeNode,
         gMakeNeighbors
       ) where

import Control.Category ((<<<))
import Control.Monad (void)
import Data.Aeson (ToJSON, FromJSON)
import Data.Foldable (fold)
import Data.Greskell
  ( FromGraphSON,
    Element(..), Vertex,
    AVertexProperty,
    GTraversal, Transform, SideEffect, Walk, liftWalk,
    Binder, newBind,
    source, sV, sAddV, gHasLabel, gHas2, gId, gProperty, gPropertyV, gV,
    gAddE, gSideEffect, gTo, gFrom,
    ($.), (<*.>)
  )
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

gNodeId :: Walk Transform VNode EID
gNodeId = gId

gGetNode :: ToJSON n => n -> Binder (GTraversal Transform () EID)
gGetNode nid = do
  var_nid <- newBind nid
  return $ gNodeId $. gHas2 "@node_id" var_nid $. gHasLabel "node" $. sV [] $ source "g"

gMakeNode :: ToJSON n => n -> Binder (GTraversal SideEffect () EID)
gMakeNode nid = do
  var_nid <- newBind nid
  return $ liftWalk gNodeId $. gProperty "@node_id" var_nid $. sAddV "node" $ source "g"

-- | The \"neighbors\" vertex.
data VNeighbors

instance Element VNeighbors where
  type ElementID VNeighbors = EID
  type ElementProperty VNeighbors = AVertexProperty

instance Vertex VNeighbors

gGetNodeByEID :: EID -> Binder (Walk Transform s VNode)
gGetNodeByEID vid = do
  var_vid <- newBind vid
  return $ gV [var_vid]

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
      return $ gSideEffect $ gAddE "observes" $ gFrom v
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: (ToJSON p) => (FoundLink n p, EID) -> Binder (Walk SideEffect VNeighbors VNeighbors)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_sp <- newBind $ subjectPort link
      var_tp <- newBind $ targetPort link
      return $ gSideEffect ( gProperty "@target_port" var_tp
                             <<< gProperty "@subject_port" var_sp
                             <<< gAddE "finds" (gTo v)
                             -- TODO: encode LinkState
                           )
  

gSetTimestamp :: Timestamp -> Binder (Walk SideEffect VNeighbors VNeighbors)
gSetTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  return $ gPropertyV Nothing "@timestamp" var_epoch []
  -- TODO: set timezone.
