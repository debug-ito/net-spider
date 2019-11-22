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
         keyTimestamp,
         gSetTimestamp,
         gVFoundNodeData,
         -- * EFinds
         EFinds,
         EFindsData(..),
         LinkAttributes(..)
       ) where

import Control.Category ((<<<))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Greskell
  ( FromGraphSON(..),
    ElementData(..), Element(..), Vertex, Edge,
    ElementID,
    AVertexProperty, AVertex, AEdge,
    Walk, SideEffect, Transform, unsafeCastEnd,
    Binder, Parser, GValue,
    gIdentity, gProperty, gPropertyV, (=:), gProperties,
    newBind,
    Key, AsLabel,
    PMap, Multi, Single, lookupAs, PMapLookupException,
    gProject, gValueMap, gByL, gId, Keys(..)
  )
import qualified Data.Greskell as Greskell
import Data.Int (Int64)
import Data.Text (Text, unpack, pack)
import Data.Time.LocalTime (TimeZone(..))

import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Found (LinkState)

-- | Generic element ID used in the graph DB.
type EID = ElementID


-- | The \"node\" vertex.
newtype VNode = VNode AVertex
              deriving (Show,Eq,ElementData,Element,Vertex,FromGraphSON)

type TsEpoch = Int64

keyTimestamp :: Key VFoundNode TsEpoch
keyTimestamp = "@timestamp"

keyTzOffset :: Key (AVertexProperty TsEpoch) Int
keyTzOffset = "@tz_offset_min"

keyTzSummerOnly :: Key (AVertexProperty TsEpoch) Bool
keyTzSummerOnly = "@tz_summer_only"

keyTzName :: Key (AVertexProperty TsEpoch) Text
keyTzName = "@tz_name"
  
gSetTimestamp :: Timestamp -> Binder (Walk SideEffect VFoundNode VFoundNode)
gSetTimestamp ts = do
  var_epoch <- newBind $ epochTime ts
  meta_props <- makeMetaProps $ timeZone ts
  return $ gPropertyV Nothing keyTimestamp var_epoch meta_props
  where
    makeMetaProps Nothing = return []
    makeMetaProps (Just tz) = do
      offset <- newBind $ timeZoneMinutes tz
      summer <- newBind $ timeZoneSummerOnly tz
      name <- newBind $ pack $ timeZoneName tz
      return $ [ keyTzOffset =: offset,
                 keyTzSummerOnly =: summer,
                 keyTzName =: name
               ]

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

eToP :: Show e => Either e a -> Parser a
eToP (Left e) = fail $ show e
eToP (Right a) = return a

labelVProps :: AsLabel (PMap Multi GValue)
labelVProps = "props"

labelVFoundNodeID :: AsLabel (ElementID VFoundNode)
labelVFoundNodeID = "vid"

labelMetaProps :: AsLabel (PMap Single GValue)
labelMetaProps = "mprops"

gVFoundNodeData :: Walk Transform VFoundNode (VFoundNodeData na)
gVFoundNodeData = unsafeCastEnd $ gProject
                  ( gByL labelVFoundNodeID gId)
                  [ gByL labelVProps $ gValueMap KeysNil,
                    gByL labelMetaProps (gValueMap KeysNil <<< gProperties [keyTimestamp])
                  ]

instance NodeAttributes na => FromGraphSON (VFoundNodeData na) where
  parseGraphSON gv = fromPMap =<< parseGraphSON gv
    where
      fromPMap :: NodeAttributes na => PMap Multi GValue -> Parser (VFoundNodeData na)
      fromPMap pm = do
        eid <- eToP $ lookupAs labelVFoundNodeID pm
        props <- eToP $ lookupAs labelVProps pm
        mprops <- eToP $ lookupAs labelMetaProps pm
        attrs <- parseNodeAttributes props
        epoch_ts <- eToP $ lookupAs keyTimestamp props
        mtz <- parseTimeZone mprops
        return $
          VFoundNodeData
          { vfnId = eid,
            vfnTimestamp = Timestamp { epochTime = epoch_ts,
                                       timeZone = mtz
                                     },
            vfnAttributes = attrs
          }
      parseTimeZone ts_prop = do
        case (get keyTzOffset, get keyTzSummerOnly, get keyTzName) of
          (Left _, Left _, Left _) -> return Nothing
          (eo, es, en) -> do
            offset <- eToP eo
            is_summer_only <- eToP es
            name <- eToP en
            return $ Just $ TimeZone { timeZoneMinutes = offset,
                                       timeZoneSummerOnly = is_summer_only,
                                       timeZoneName = unpack name
                                     }
          where
            get :: FromGraphSON b => Key a b -> Either PMapLookupException b
            get k = lookupAs k ts_prop

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
  
