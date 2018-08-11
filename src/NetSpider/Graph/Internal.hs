{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
-- |
-- Module: NetSpider.Graph.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Graph.Internal
       ( -- * EID
         EID,
         -- * VNode
         VNode,
         -- * VFoundNode
         VFoundNode(..),
         NodeAttributes(..),
         -- * EFinds
         EFinds(..),
         LinkAttributes(..)
       ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Greskell
  ( FromGraphSON(..), parseOneValue,
    Element(..), Vertex, Edge(..),
    AVertexProperty, AVertex(..), AProperty, AEdge(..),
    Walk, SideEffect,
    Binder, Parser, PropertyMapList, PropertyMapSingle, GValue,
    gIdentity
  )
import Data.Text (Text)

import NetSpider.Timestamp (Timestamp, fromEpochSecond)
import NetSpider.Found (LinkState)

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

-- | The \"found_node\" vertex.
data VFoundNode na =
  VFoundNode
  { vfnId :: !EID,
    vfnTimestamp :: !Timestamp,
    vfnAttributes :: !na
  }

instance Element (VFoundNode na) where
  type ElementID (VFoundNode na) = EID
  type ElementProperty (VFoundNode na) = AVertexProperty

instance Vertex (VFoundNode na)

instance NodeAttributes na => FromGraphSON (VFoundNode na) where
  parseGraphSON gv = fromAVertex =<< parseGraphSON gv
    where
      fromAVertex av = do
        eid <- parseGraphSON $ avId av
        epoch_ts <- parseOneValue "@timestamp" $ avProperties av
        -- TODO: parse timezone.
        attrs <- parseNodeAttributes $ avProperties av
        return $ VFoundNode { vfnId = eid,
                              vfnTimestamp = fromEpochSecond epoch_ts,
                              vfnAttributes = attrs
                            }

-- | \"finds\" edge.
data EFinds la =
  EFinds
  { efId :: !EID,
    efTargetId :: !EID,
    efLinkState :: !LinkState,
    efLinkAttributes :: !la
  }

instance Element (EFinds la) where
  type ElementID (EFinds la) = EID
  type ElementProperty (EFinds la) = AProperty

instance Edge (EFinds la) where
  type EdgeVertexID (EFinds la) = EID

instance LinkAttributes la => FromGraphSON (EFinds la) where
  parseGraphSON gv = fromAEdge =<< parseGraphSON gv
    where
      fromAEdge ae = EFinds 
                     <$> (parseGraphSON $ aeId ae)
                     <*> (parseGraphSON $ aeInV ae)
                     <*> (parseOneValue "@link_state" ps)
                     <*> (parseLinkAttributes ps)
        where
          ps = aeProperties ae

-- | Class of user-defined types for node attributes. Its content is
-- stored in the NetSpider database.
class NodeAttributes ps where
  writeNodeAttributes :: ps -> Binder (Walk SideEffect (VFoundNode ps) (VFoundNode ps))
  parseNodeAttributes :: PropertyMapList AVertexProperty GValue -> Parser ps

-- | No attributes.
instance NodeAttributes () where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return ()

-- | Class of user-defined types for link attributes. Its content is
-- stored in the NetSpider database.
class LinkAttributes ps where
  writeLinkAttributes :: ps -> Binder (Walk SideEffect (EFinds ps) (EFinds ps))
  parseLinkAttributes :: PropertyMapSingle AProperty GValue -> Parser ps

-- | No attributes.
instance LinkAttributes () where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return ()
  
