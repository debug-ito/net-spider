{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Internal.Graph
-- Description: Graph (greskell) operation for Spider
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Graph
       ( gClearAll,
         -- * VNode
         gAllNodes,
         gHasNodeID,
         gHasNodeEID,
         gNodeEID,
         gNodeID,
         gMakeNode,
         -- * VFoundNode
         gAllFoundNode,
         gHasFoundNodeEID,
         gMakeFoundNode,
         gSelectFoundNode,
         gLatestFoundNode,
         -- * EFinds
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

import NetSpider.Graph
  ( EID, VNode, VFoundNode, EFinds,
    LinkAttributes(..), NodeAttributes(..)
  )
import NetSpider.Found (FoundLink(..), LinkState(..), FoundNode(..), linkStateToText)
import NetSpider.Timestamp (Timestamp(..), fromEpochSecond)
import NetSpider.Spider.Internal.Type (Spider(..))


gNodeEID :: Walk Transform VNode EID
gNodeEID = gId

gNodeID :: Spider n na la -> Walk Transform VNode n
gNodeID _ = gValues ["@node_id"]

gAllNodes :: GTraversal Transform () VNode
gAllNodes = gHasLabel "node" $. sV [] $ source "g"

gHasNodeID :: (ToJSON n, WalkType c) => Spider n na la -> n -> Binder (Walk c VNode VNode)
gHasNodeID _ nid = do
  var_nid <- newBind nid
  return $ gHas2 "@node_id" var_nid

gHasNodeEID :: (WalkType c) => EID -> Binder (Walk c VNode VNode)
gHasNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeNode :: ToJSON n => Spider n na la -> n -> Binder (GTraversal SideEffect () VNode)
gMakeNode _ nid = do
  var_nid <- newBind nid
  return $ gProperty "@node_id" var_nid $. sAddV "node" $ source "g"

gGetNodeByEID :: EID -> Binder (Walk Transform s VNode)
gGetNodeByEID vid = do
  f <- gHasNodeEID vid
  return (f <<< gV [])


gAllFoundNode :: GTraversal Transform () (VFoundNode na)
gAllFoundNode = gHasLabel "found_node" $. sV [] $ source "g"

gHasFoundNodeEID :: WalkType c => EID -> Binder (Walk c (VFoundNode na) (VFoundNode na))
gHasFoundNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeFoundNode :: (LinkAttributes la, NodeAttributes na)
               => EID -- ^ subject node EID
               -> Vector (FoundLink n la, EID) -- ^ (link, target node EID)
               -> FoundNode n na la
               -> Binder (GTraversal SideEffect () (VFoundNode na))
gMakeFoundNode subject_vid link_pairs fnode = 
  mAddFindsEdges
  <*.> writeNodeAttributes (nodeAttributes fnode)
  <*.> gSetTimestamp (observationTime fnode)
  <*.> mAddObservedEdge
  <*.> pure $ sAddV "found_node" $ source "g"
  where
    mAddObservedEdge :: Binder (Walk SideEffect (VFoundNode na) (VFoundNode na))
    mAddObservedEdge = do
      v <- gGetNodeByEID subject_vid
      return $ gSideEffect $ emitsAEdge $ gAddE "is_observed_as" $ gFrom v
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: LinkAttributes la => (FoundLink n la, EID) -> Binder (Walk SideEffect (VFoundNode na) (VFoundNode na))
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_ls <- newBind $ linkStateToText $ linkState link
      addAttrs <- writeLinkAttributes $ linkAttributes link
      return $ gSideEffect ( addAttrs
                             <<< gProperty "@link_state" var_ls
                             <<< gAddE "finds" (gTo v)
                           )

keyTimestamp :: Key (VFoundNode na) Int64
keyTimestamp = "@timestamp"
  
gSetTimestamp :: Timestamp -> Binder (Walk SideEffect (VFoundNode na) (VFoundNode na))
gSetTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  return $ gPropertyV Nothing keyTimestamp var_epoch []
  -- TODO: set timezone.

emitsAEdge :: ToGTraversal g => g c s AEdge -> g c s AEdge
emitsAEdge = id

gClearAll :: GTraversal SideEffect () ()
gClearAll = void $ gDrop $. liftWalk $ sV' [] $ source "g"

gSelectFoundNode :: Walk Filter (VFoundNode na) (VFoundNode na) -> Walk Transform VNode (VFoundNode na)
gSelectFoundNode filterFoundNode = liftWalk filterFoundNode <<< gOut ["is_observed_as"]

gLatestFoundNode :: Walk Transform (VFoundNode na) (VFoundNode na)
gLatestFoundNode = gLimit 1 <<< gOrder [gBy2 keyTimestamp oDecr]

gFinds :: Walk Transform (VFoundNode na) (EFinds la)
gFinds = gOutE ["finds"]

  
        
