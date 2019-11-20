{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
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
         VFoundNode,
         VFoundNodeData(..),
         NodeAttributes(..),
         -- * EFinds
         EFinds,
         EFindsData(..),
         LinkAttributes(..)
       ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Greskell
  ( FromGraphSON(..),
    ElementData(..), Element(..), Vertex, Edge,
    ElementID,
    AVertexProperty(..), AVertex(..), AProperty, AEdge(..),
    Walk, SideEffect,
    Binder, Parser, GValue,
    gIdentity, gProperty,
    newBind,
    PMap, Multi, Single
  )
import qualified Data.Greskell as Greskell
import Data.Text (Text, unpack)
import Data.Time.LocalTime (TimeZone(..))

import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Found (LinkState)

-- | Generic element ID used in the graph DB.
type EID = ElementID


-- | The \"node\" vertex.
newtype VNode = VNode AVertex
              deriving (Show,Eq,ElementData,Element,Vertex,FromGraphSON)

-- | The \"found_node\" vertex.
newtype VFoundNode = VFoundNode AVertex
                   deriving (Show,Eq,ElementData,Element,Vertex,FromGraphSON)

-- | Properties of the \"found_node\" vertex.
data VFoundNodeData na =
  VFoundNodeData
  { vfnId :: EID VFoundNode,
    vfnTimestamp :: Timestamp,
    vfnAttributes :: na
  }
  deriving (Show)

instance NodeAttributes na => FromGraphSON (VFoundNodeData na) where
  parseGraphSON gv = undefined -- TODO
  ---- parseGraphSON gv = fromAVertex =<< parseGraphSON gv
  ----   where
  ----     fromAVertex av = do
  ----       eid <- parseGraphSON $ avId av
  ----       ts_prop <- case lookupOne "@timestamp" $ avProperties av of
  ----         Nothing -> fail ("Cannot find property named @timestamp")
  ----         Just p -> return p
  ----       epoch_ts <- parseGraphSON $ avpValue ts_prop
  ----       mtz <- parseTimeZone ts_prop
  ----       attrs <- parseNodeAttributes $ avProperties av
  ----       return $ VFoundNode { vfnId = eid,
  ----                             vfnTimestamp = Timestamp { epochTime = epoch_ts,
  ----                                                        timeZone = mtz
  ----                                                      },
  ----                             vfnAttributes = attrs
  ----                           }
  ----     parseTimeZone ts_prop =
  ----       case (get "@tz_offset_min", get "@tz_summer_only", get "@tz_name") of
  ----        (Left _, Left _, Left _) -> return Nothing
  ----        (eo, es, en) -> do
  ----          offset <- parseE eo
  ----          is_summer_only <- parseE es
  ----          name <- parseE en
  ----          return $ Just $ TimeZone { timeZoneMinutes = offset,
  ----                                     timeZoneSummerOnly = is_summer_only,
  ----                                     timeZoneName = unpack name
  ----                                   }
  ----       where
  ----         get k = maybe (Left ("Cannot find property " ++ unpack k)) Right $ lookupOneValue k $ avpProperties ts_prop
  ----         parseE :: (FromGraphSON a) => Either String GValue -> Parser a
  ----         parseE = either fail parseGraphSON

-- | \"finds\" edge.
newtype EFinds = EFinds AEdge
               deriving (Show,Eq,ElementData,Element,Edge,FromGraphSON)

-- | Properties of \"finds\" edge.
data EFindsData la =
  EFindsData
  { efId :: EID EFinds,
    efTargetId :: EID VNode,
    efLinkState :: LinkState,
    efLinkAttributes :: la
  }
  deriving (Show)

instance LinkAttributes la => FromGraphSON (EFindsData la) where
  parseGraphSON gv = undefined -- TODO
  ---- parseGraphSON gv = fromAEdge =<< parseGraphSON gv
  ----   where
  ----     fromAEdge ae = EFinds 
  ----                    <$> (parseGraphSON $ aeId ae)
  ----                    <*> (parseGraphSON $ aeInV ae)
  ----                    <*> (parseOneValue "@link_state" ps)
  ----                    <*> (parseLinkAttributes ps)
  ----       where
  ----         ps = aeProperties ae

-- | Class of user-defined types for node attributes. Its content is
-- stored in the NetSpider database.
class NodeAttributes ps where
  writeNodeAttributes :: ps -> Binder (Walk SideEffect VFoundNode VFoundNode)
  -- ^ Return 'Walk' to write the attributes to the 'VFoundNode'.
  parseNodeAttributes :: PMap Multi GValue -> Parser ps
  -- ^ Parse the vertex proprerties into the attributes.

-- | No attributes.
instance NodeAttributes () where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return ()

-- TODO.
---- -- | Straightforward implementation. Note that 'writeNodeAttributes'
---- -- does not write meta-properties of the 'AVertexProperty'.
---- --
---- -- @since 0.3.0.0
---- instance (FromGraphSON v, ToJSON v) => NodeAttributes (PropertyMapList AVertexProperty v) where
----   writeNodeAttributes = writeAllProperties
----   parseNodeAttributes = traverse parseGraphSON

-- | Class of user-defined types for link attributes. Its content is
-- stored in the NetSpider database.
class LinkAttributes ps where
  writeLinkAttributes :: ps -> Binder (Walk SideEffect EFinds EFinds)
  -- ^ Return 'Walk' to write the attributes to the 'EFinds'.
  parseLinkAttributes :: PMap Single GValue -> Parser ps
  -- ^ Parse the edge proprerties into the attributes.

-- | No attributes.
instance LinkAttributes () where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return ()

-- TODO
---- -- | Straightforward implementation
---- --
---- -- @since 0.3.0.0
---- instance (FromGraphSON v, ToJSON v) => LinkAttributes (PropertyMapSingle AProperty v) where
----   writeLinkAttributes = writeAllProperties
----   parseLinkAttributes = traverse parseGraphSON
  
