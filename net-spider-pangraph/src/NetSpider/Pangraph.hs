{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- |
-- Module: NetSpider.Pangraph
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.Pangraph
       ( -- * Converters
         makePangraph,
         makeVertex,
         makeEdge,
         ToAttributes(..),
         -- * Utility
         spackBS,
         timestampAttributes
       ) where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.LocalTime (TimeZone(..))
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink,
    nodeId, nodeAttributes, nodeTimestamp, isOnBoundary,
    sourceNode, destinationNode, linkAttributes, linkTimestamp, isDirected
  )
import NetSpider.Timestamp (Timestamp(..), showEpochTime)
import qualified Pangraph as P

-- | Make Pangraph 'P.Attribute's from 'Timestamp'.
--
-- >>> import NetSpider.Timestamp (fromS)
-- >>> timestampAttributes $ fromS "2018-10-11T11:23:05"
-- [("@timestamp","1539256985000")]
-- >>> timestampAttributes $ fromS "2018-09-23T08:48:52+09:00"
-- [("@timestamp","1537660132000"),("@tz_offset_min","540"),("@tz_summer_only","False"),("@tz_name","")]
-- >>> timestampAttributes $ fromS "2018-12-30T22:00:12.109-04:00"
-- [("@timestamp","1546221612109"),("@tz_offset_min","-240"),("@tz_summer_only","False"),("@tz_name","")]
timestampAttributes :: Timestamp -> [P.Attribute]
timestampAttributes ts = epoch : tz_attrs
  where
    epoch = ("@timestamp", encodeUtf8 $ showEpochTime ts)
    tz_attrs = case timeZone ts of
      Nothing -> []
      Just tz -> [ ("@tz_offset_min", spackBS $ timeZoneMinutes tz),
                   ("@tz_summer_only", spackBS $ timeZoneSummerOnly tz),
                   ("@tz_name", encodeUtf8 $ pack $ timeZoneName tz)
                 ]

maybeList :: Maybe [a] -> [a]
maybeList Nothing = []
maybeList (Just l) = l

-- | Make Pangraph 'P.Vertex' from 'SnapshotNode'.
--
-- Node attributes (@na@) is converted to attributes of
-- 'P.Vertex'. 'nodeTimestamp' is converted by 'timestampAttributes',
-- if present. 'isOnBoundary' is stored as \"\@is_on_boundary\"
-- attribute, if present.  respectively.
makeVertex :: (Show n, ToAttributes na) => SnapshotNode n na -> P.Vertex
makeVertex sn = P.makeVertex vid attrs
  where
    vid = spackBS $ nodeId sn
    attrs = (maybeList $ fmap timestampAttributes $ nodeTimestamp sn)
            ++ [("@is_on_boundary", spackBS $ isOnBoundary sn)]
            ++ (maybeList $ fmap toAttributes $ nodeAttributes sn)

-- | Make Pangraph 'P.Edge' from 'SnapshotLink'.
--
-- Link attributes (@la@) is converted to attributes of
-- 'P.Edge'. 'linkTimestamp' is converted by
-- 'timestampAttributes'. 'isDirected' is stored as \"\@is_directed\"
-- attribute..
makeEdge :: (Show n, ToAttributes la) => SnapshotLink n la -> P.Edge
makeEdge sl = P.makeEdge (src, dest) attrs
  where
    src = spackBS $ sourceNode sl
    dest = spackBS $ destinationNode sl
    attrs = (timestampAttributes $ linkTimestamp sl)
            ++ [("@is_directed", spackBS $ isDirected sl)]
            ++ (toAttributes $ linkAttributes sl)

-- | Make a 'P.Pangraph'.
makePangraph :: (Show n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> Maybe P.Pangraph
makePangraph ns ls = P.makePangraph (map makeVertex ns) (map makeEdge ls)

-- | 'show' the argument, encode it in UTF-8 and 'pack' to
-- 'ByteString'.
spackBS :: Show a => a -> ByteString
spackBS = encodeUtf8 . pack . show

-- | Data types that can be converted into a list of Pangraph
-- 'P.Attribute's.
class ToAttributes a where
  toAttributes :: a -> [P.Attribute]

-- | No attribute.
instance ToAttributes () where
  toAttributes () = []

-- | Show key and value to make 'P.Attribute'.
instance (Show k, Show v) => ToAttributes [(k,v)] where
  toAttributes = map (\(k, v) -> (spackBS k, spackBS v))
