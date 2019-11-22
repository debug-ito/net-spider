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
         gFilterFoundNodeByTime,
         -- * EFinds
         gFinds,
         gFindsTarget
       ) where

import Control.Category ((<<<))
import Control.Monad (void)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Foldable (fold)
import Data.Greskell
  ( WalkType, AEdge,
    GTraversal, Filter, Transform, SideEffect, Walk, liftWalk,
    Binder, newBind,
    source, sV, sV', sAddV, gHasLabel, gHasId, gHas2, gHas2P, gId, gProperty, gPropertyV, gV,
    gNot, gIdentity',
    gAddE, gSideEffect, gTo, gFrom, gDrop, gOut, gOrder, gBy2, gValues, gOutE, gInV,
    ($.), (<*.>), (=:),
    ToGTraversal,
    Key, oDecr, gLimit,
    pGt, pGte, pLt, pLte
  )
import Data.Int (Int64)
import Data.Interval (lowerBound', upperBound')
import Data.Text (Text, pack)
import Data.Time.LocalTime (TimeZone(..))
import Data.Traversable (traverse)

import NetSpider.Graph
  ( EID, VNode, VFoundNode, EFinds,
    LinkAttributes(..), NodeAttributes(..)
  )
import NetSpider.Graph.Internal (keyTimestamp, gSetTimestamp)
import NetSpider.Found (FoundLink(..), LinkState(..), FoundNode(..), linkStateToText)
import NetSpider.Query (Interval, Extended(..))
import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Spider.Config (Config(..))
import NetSpider.Spider.Internal.Spider (Spider(..))

spiderNodeIdKey :: Spider n na fla -> Key VNode n
spiderNodeIdKey = nodeIdKey . spiderConfig

gNodeEID :: Walk Transform VNode (EID VNode)
gNodeEID = gId

gNodeID :: Spider n na fla -> Walk Transform VNode n
gNodeID spider = gValues [spiderNodeIdKey spider]

gAllNodes :: GTraversal Transform () VNode
gAllNodes = gHasLabel "node" $. sV [] $ source "g"

gHasNodeID :: (ToJSON n, WalkType c) => Spider n na fla -> n -> Binder (Walk c VNode VNode)
gHasNodeID spider nid = do
  var_nid <- newBind nid
  return $ gHas2 (spiderNodeIdKey spider) var_nid

gHasNodeEID :: (WalkType c) => EID VNode -> Binder (Walk c VNode VNode)
gHasNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeNode :: ToJSON n => Spider n na fla -> n -> Binder (GTraversal SideEffect () VNode)
gMakeNode spider nid = do
  var_nid <- newBind nid
  return $ gProperty (spiderNodeIdKey spider) var_nid $. sAddV "node" $ source "g"

gGetNodeByEID :: EID VNode -> Binder (Walk Transform s VNode)
gGetNodeByEID vid = do
  f <- gHasNodeEID vid
  return (f <<< gV [])


gAllFoundNode :: GTraversal Transform () VFoundNode
gAllFoundNode = gHasLabel "found_node" $. sV [] $ source "g"

gHasFoundNodeEID :: WalkType c => EID VFoundNode -> Binder (Walk c VFoundNode VFoundNode)
gHasFoundNodeEID eid = do
  var_eid <- newBind eid
  return $ gHasId var_eid

gMakeFoundNode :: (LinkAttributes la, NodeAttributes na)
               => EID VNode -- ^ subject node EID
               -> [(FoundLink n la, EID VNode)] -- ^ (link, target node EID)
               -> FoundNode n na la
               -> Binder (GTraversal SideEffect () VFoundNode)
gMakeFoundNode subject_vid link_pairs fnode = 
  mAddFindsEdges
  <*.> writeNodeAttributes (nodeAttributes fnode)
  <*.> gSetTimestamp (foundAt fnode)
  <*.> mAddObservedEdge
  <*.> pure $ sAddV "found_node" $ source "g"
  where
    mAddObservedEdge :: Binder (Walk SideEffect VFoundNode VFoundNode)
    mAddObservedEdge = do
      v <- gGetNodeByEID subject_vid
      return $ gSideEffect $ emitsAEdge $ gAddE "is_observed_as" $ gFrom v
    mAddFindsEdges = fmap fold $ traverse mAddFindsEdgeFor link_pairs
    mAddFindsEdgeFor :: LinkAttributes la => (FoundLink n la, EID VNode) -> Binder (Walk SideEffect VFoundNode VFoundNode)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_ls <- newBind $ linkStateToText $ linkState link
      addAttrs <- writeLinkAttributes $ linkAttributes link
      return $ gSideEffect ( addAttrs
                             <<< gProperty "@link_state" var_ls
                             <<< gAddE "finds" (gTo v)
                           )

emitsAEdge :: ToGTraversal g => g c s AEdge -> g c s AEdge
emitsAEdge = id

gClearAll :: GTraversal SideEffect () ()
gClearAll = void $ gDrop $. liftWalk $ sV' [] $ source "g"

gSelectFoundNode :: Walk Filter VFoundNode VFoundNode -> Walk Transform VNode VFoundNode
gSelectFoundNode filterFoundNode = liftWalk filterFoundNode <<< gOut ["is_observed_as"]

gLatestFoundNode :: Walk Transform VFoundNode VFoundNode
gLatestFoundNode = gLimit 1 <<< gOrder [gBy2 keyTimestamp oDecr]

gFilterFoundNodeByTime :: Interval Timestamp -> Binder (Walk Filter VFoundNode VFoundNode)
gFilterFoundNodeByTime interval = do
  fl <- filterLower
  fh <- filterUpper
  return (fh <<< fl)
  where
    filterLower = case lowerBound' interval of
      (PosInf, _) -> return $ gNot gIdentity'
      (NegInf, _) -> return $ gIdentity'
      (Finite ts, False) -> fmap (gHas2P keyTimestamp) $ fmap pGt  $ newBind $ epochTime ts
      (Finite ts, True)  -> fmap (gHas2P keyTimestamp) $ fmap pGte $ newBind $ epochTime ts
    filterUpper = case upperBound' interval of
      (PosInf, _) -> return $ gIdentity'
      (NegInf, _) -> return $ gNot gIdentity'
      (Finite ts, False) -> fmap (gHas2P keyTimestamp) $ fmap pLt  $ newBind $ epochTime ts
      (Finite ts, True)  -> fmap (gHas2P keyTimestamp) $ fmap pLte $ newBind $ epochTime ts

gFinds :: Walk Transform VFoundNode EFinds
gFinds = gOutE ["finds"]

gFindsTarget :: Walk Transform EFinds VNode
gFindsTarget = gInV
