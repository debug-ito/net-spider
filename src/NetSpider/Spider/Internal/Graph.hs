{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
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
  ( FromGraphSON(..),
    Element(..), Vertex, Edge(..), WalkType,
    AVertexProperty, AProperty, AEdge(..), AVertex(..), parseOneValue,
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

import NetSpider.ObservedNode (FoundLink(..), LinkState(..), linkStateToText)
import NetSpider.Timestamp (Timestamp(..), fromEpochSecond)

-- | Generic element ID used in the graph DB.
newtype EID = EID (Either Int Text)
            deriving (Show,Eq,Ord,FromGraphSON)

instance ToJSON EID where
  toJSON (EID e) = either toJSON toJSON e

instance FromJSON EID where
  parseJSON (String s) = return $ EID $ Right s
  parseJSON v = fmap (EID . Left) $ parseJSON v


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

-- | The \"observed_node\" vertex.
data VObservedNode =
  VObservedNode
  { vnID :: !EID,
    vnTimestamp :: !Timestamp
  }

instance Element VObservedNode where
  type ElementID VObservedNode = EID
  type ElementProperty VObservedNode = AVertexProperty

instance Vertex VObservedNode

instance FromGraphSON VObservedNode where
  parseGraphSON gv = fromAVertex =<< parseGraphSON gv
    where
      fromAVertex av = do
        eid <- parseGraphSON $ avId av
        epoch_ts <- parseOneValue "@timestamp" $ avProperties av
        -- TODO: parse timezone.
        return $ VObservedNode { vnID = eid,
                                 vnTimestamp = fromEpochSecond epoch_ts
                               }

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

gMakeObservedNode :: (ToJSON p)
                  => EID -- ^ subject node EID
                  -> Vector (FoundLink n p, EID) -- ^ (link, target node EID)
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
    mAddFindsEdgeFor :: (ToJSON p) => (FoundLink n p, EID) -> Binder (Walk SideEffect VObservedNode VObservedNode)
    mAddFindsEdgeFor (link, target_vid) = do
      v <- gGetNodeByEID target_vid
      var_sp <- newBind $ subjectPort link
      var_tp <- newBind $ targetPort link
      var_ls <- newBind $ linkStateToText $ linkState link
      return $ gSideEffect ( emitsAEdge
                             $ gProperty "@target_port" var_tp
                             <<< gProperty "@subject_port" var_sp
                             <<< gProperty "@link_state" var_ls
                             <<< gAddE "finds" (gTo v)
                           )

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

gFinds :: Walk Transform VObservedNode (EFinds p)
gFinds = gOutE ["finds"]

-- | \"finds\" edge.
data EFinds p =
  EFinds
  { efEID :: !EID,
    efSubjectPort :: !p,
    efTargetEID :: !EID,
    efTargetPort :: !p,
    efLinkState :: !LinkState
  }

instance Element (EFinds p) where
  type ElementID (EFinds p) = EID
  type ElementProperty (EFinds p) = AProperty

instance Edge (EFinds p) where
  type EdgeVertexID (EFinds p) = EID

instance FromGraphSON p => FromGraphSON (EFinds p) where
  parseGraphSON gv = fromAEdge =<< parseGraphSON gv
    where
      fromAEdge ae = EFinds 
                     <$> (parseGraphSON $ aeId ae)
                     <*> (parseOneValue "@subject_port" ps)
                     <*> (parseGraphSON $ aeInV ae)
                     <*> (parseOneValue "@target_port" ps)
                     <*> (parseOneValue "@link_state" ps)
        where
          ps = aeProperties ae
  
        
