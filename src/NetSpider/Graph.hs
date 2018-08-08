{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
-- |
-- Module: NetSpider.Graph
-- Description: Graph data models
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Graph
       ( -- * EID
         EID,
         -- * VNode
         VNode,
         -- * VObservedNode
         VObservedNode(..),
         -- * EFinds
         EFinds(..)
       ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Greskell
  ( FromGraphSON(..), parseOneValue,
    Element(..), Vertex, Edge(..),
    AVertexProperty, AVertex(..), AProperty, AEdge(..)
  )
import Data.Text (Text)

import NetSpider.Timestamp (Timestamp, fromEpochSecond)
import NetSpider.ObservedNode (LinkState)

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

-- | The \"observed_node\" vertex.
data VObservedNode =
  VObservedNode
  { vonId :: !EID,
    vonTimestamp :: !Timestamp
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
        return $ VObservedNode { vonId = eid,
                                 vonTimestamp = fromEpochSecond epoch_ts
                               }

-- | \"finds\" edge.
data EFinds la =
  EFinds
  { efId :: !EID,
    efTargetId :: !EID,
    efLinkState :: !LinkState
  }

instance Element (EFinds la) where
  type ElementID (EFinds la) = EID
  type ElementProperty (EFinds la) = AProperty

instance Edge (EFinds la) where
  type EdgeVertexID (EFinds la) = EID

instance FromGraphSON (EFinds la) where
  parseGraphSON gv = fromAEdge =<< parseGraphSON gv
    where
      fromAEdge ae = EFinds 
                     <$> (parseGraphSON $ aeId ae)
                     <*> (parseGraphSON $ aeInV ae)
                     <*> (parseOneValue "@link_state" ps)
        where
          ps = aeProperties ae
