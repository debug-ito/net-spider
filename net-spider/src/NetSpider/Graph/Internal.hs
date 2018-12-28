{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings, FlexibleInstances #-}
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
  ( FromGraphSON(..), parseOneValue, lookupOne, lookupOneValue,
    Element(..), Vertex, Edge(..),
    AVertexProperty(..), AVertex(..), AProperty, AEdge(..),
    Walk, SideEffect,
    Binder, Parser, PropertyMapList, PropertyMapSingle, GValue,
    gIdentity, gProperty,
    newBind, allProperties, propertyKey, propertyValue
  )
import qualified Data.Greskell as Greskell
import Data.Text (Text, unpack)
import Data.Time.LocalTime (TimeZone(..))

import NetSpider.Timestamp (Timestamp(..))
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
  deriving (Show)

instance Element (VFoundNode na) where
  type ElementID (VFoundNode na) = EID
  type ElementProperty (VFoundNode na) = AVertexProperty

instance Vertex (VFoundNode na)

instance NodeAttributes na => FromGraphSON (VFoundNode na) where
  parseGraphSON gv = fromAVertex =<< parseGraphSON gv
    where
      fromAVertex av = do
        eid <- parseGraphSON $ avId av
        ts_prop <- case lookupOne "@timestamp" $ avProperties av of
          Nothing -> fail ("Cannot find property named @timestamp")
          Just p -> return p
        epoch_ts <- parseGraphSON $ avpValue ts_prop
        mtz <- parseTimeZone ts_prop
        attrs <- parseNodeAttributes $ avProperties av
        return $ VFoundNode { vfnId = eid,
                              vfnTimestamp = Timestamp { epochTime = epoch_ts,
                                                         timeZone = mtz
                                                       },
                              vfnAttributes = attrs
                            }
      parseTimeZone ts_prop =
        case (get "@tz_offset_min", get "@tz_summer_only", get "@tz_name") of
         (Left _, Left _, Left _) -> return Nothing
         (eo, es, en) -> do
           offset <- parseE eo
           is_summer_only <- parseE es
           name <- parseE en
           return $ Just $ TimeZone { timeZoneMinutes = offset,
                                      timeZoneSummerOnly = is_summer_only,
                                      timeZoneName = unpack name
                                    }
        where
          get k = maybe (Left ("Cannot find property " ++ unpack k)) Right $ lookupOneValue k $ avpProperties ts_prop
          parseE :: (FromGraphSON a) => Either String GValue -> Parser a
          parseE = either fail parseGraphSON

-- | \"finds\" edge.
data EFinds la =
  EFinds
  { efId :: !EID,
    efTargetId :: !EID,
    efLinkState :: !LinkState,
    efLinkAttributes :: !la
  }
  deriving (Show)

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
  -- ^ Return 'Walk' to write the attributes to the 'VFoundNode'.
  parseNodeAttributes :: PropertyMapList AVertexProperty GValue -> Parser ps
  -- ^ Parse the vertex proprerties into the attributes.

-- | No attributes.
instance NodeAttributes () where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return ()

instance (FromGraphSON v, ToJSON v) => NodeAttributes (PropertyMapList AVertexProperty v) where
  writeNodeAttributes ps = fmap mconcat $ mapM toPropStep $ allProperties ps
    where
      toPropStep prop = do
        bval <- newBind $ propertyValue prop
        return $ gProperty (Greskell.key $ propertyKey prop) bval
  parseNodeAttributes = traverse parseGraphSON

-- | Class of user-defined types for link attributes. Its content is
-- stored in the NetSpider database.
class LinkAttributes ps where
  writeLinkAttributes :: ps -> Binder (Walk SideEffect (EFinds ps) (EFinds ps))
  -- ^ Return 'Walk' to write the attributes to the 'EFinds'.
  parseLinkAttributes :: PropertyMapSingle AProperty GValue -> Parser ps
  -- ^ Parse the edge proprerties into the attributes.

-- | No attributes.
instance LinkAttributes () where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return ()
  
