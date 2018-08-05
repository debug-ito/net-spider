{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Internal.Graph
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Graph
       ( EID,
         gClearAll,
         -- * VNode
         VNode,
         gAllNodes,
         gHasNodeID,
         gHasNodeEID,
         gNodeEID,
         gNodeID,
         gMakeNode,
         -- * VObservedNode
         VObservedNode(..),
         gAllObservedNode,
         gHasObservedNodeEID,
         gMakeObservedNode,
         gSelectObservedNode,
         gLatestObservedNode,
         -- * EFinds
         EFinds(..),
         gFinds
       ) where

import Control.Category ((<<<))
import Control.Monad (void)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Foldable (fold)
import Data.Greskell
  ( WalkType, AEdge,
    GTraversal, Filter, Transform, SideEffect, Walk, liftWalk,
    Binder, newBind,
    source, sV, sV', sAddV, gHasLabel, gHasId, gHas2, gId, gProperty, gPropertyV, gV,
    gAddE, gSideEffect, gTo, gFrom, gDrop, gOut, gOrder, gBy2, gValues, gOutE,
    ($.), (<*.>),
    ToGTraversal,
    Key, oDecr, gLimit
  )
import Data.Int (Int64)
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Vector (Vector)

import NetSpider.Graph (EID, VNode, VObservedNode(..), EFinds(..))
import NetSpider.ObservedNode (FoundLink(..), LinkState(..), linkStateToText)
import NetSpider.Timestamp (Timestamp(..), fromEpochSecond)


gNodeEID :: Walk Transform VNode EID
gNodeEID = gId

gNodeID :: Walk Transform VNode n
gNodeID = gValues ["@node_id"]

gAllNodes :: GTraversal Transform () VNode
gAllNodes = gHasLabel "node" $. sV [] $ source "g"

gHasNodeID :: (ToJSON n, WalkType c) => n -> Binder (Walk c VNode VNode)
gHasNodeID nid = do
  var_nid <- newBind nid
  return $ gHas2 "@node_id" var_nid

gHasNodeEID :: (WalkType c) => EID -> Binder (Walk c VNode VNode)
gHasNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeNode :: ToJSON n => n -> Binder (GTraversal SideEffect () VNode)
gMakeNode nid = do
  var_nid <- newBind nid
  return $ gProperty "@node_id" var_nid $. sAddV "node" $ source "g"

gGetNodeByEID :: EID -> Binder (Walk Transform s VNode)
gGetNodeByEID vid = do
  f <- gHasNodeEID vid
  return (f <<< gV [])


gAllObservedNode :: GTraversal Transform () VObservedNode
gAllObservedNode = gHasLabel "observed_node" $. sV [] $ source "g"

gHasObservedNodeEID :: WalkType c => EID -> Binder (Walk c VObservedNode VObservedNode)
gHasObservedNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeObservedNode :: EID -- ^ subject node EID
                  -> Vector (FoundLink n la, EID) -- ^ (link, target node EID)
                  -> Timestamp
                  -> Binder (GTraversal SideEffect () VObservedNode)
gMakeObservedNode subject_vid link_pairs timestamp = 
  mAddFindsEdges <*.> gSetTimestamp timestamp <*.> mAddObservesEdge <*.> pure $ sAddV "observed_node" $ source "g"
  where
    mAddObservesEdge :: Binder (Walk SideEffect VObservedNode VObservedNode)
    mAddObservesEdge = do
      v <- gGetNodeByEID subject_vid
      return $ gSideEffect $ emitsAEdge $ gAddE "observes" $ gFrom v
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: (FoundLink n la, EID) -> Binder (Walk SideEffect VObservedNode VObservedNode)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_ls <- newBind $ linkStateToText $ linkState link
      return $ gSideEffect ( emitsAEdge
                             $ gProperty "@link_state" var_ls
                             <<< gAddE "finds" (gTo v)
                           )
    -- TODO: save link attributes.

keyTimestamp :: Key VObservedNode Int64
keyTimestamp = "@timestamp"
  
gSetTimestamp :: Timestamp -> Binder (Walk SideEffect VObservedNode VObservedNode)
gSetTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  return $ gPropertyV Nothing keyTimestamp var_epoch []
  -- TODO: set timezone.

emitsAEdge :: ToGTraversal g => g c s AEdge -> g c s AEdge
emitsAEdge = id

gClearAll :: GTraversal SideEffect () ()
gClearAll = void $ gDrop $. liftWalk $ sV' [] $ source "g"

gSelectObservedNode :: Walk Filter VObservedNode VObservedNode -> Walk Transform VNode VObservedNode
gSelectObservedNode filterObservedNode = liftWalk filterObservedNode <<< gOut ["observes"]

gLatestObservedNode :: Walk Transform VObservedNode VObservedNode
gLatestObservedNode = gLimit 1 <<< gOrder [gBy2 keyTimestamp oDecr]

gFinds :: Walk Transform VObservedNode (EFinds la)
gFinds = gOutE ["finds"]

  
        
