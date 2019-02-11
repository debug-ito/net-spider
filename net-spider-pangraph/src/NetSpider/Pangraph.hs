{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- |
-- Module: NetSpider.Pangraph
-- Description: Conversion between NetSpider and Pangraph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Conversion of NetSpider data model into Pangraph data model. This
-- module allows you to export a snapshot graph ('SnapshotNode's and
-- 'SnapshotLink's) to 'P.Pangraph'. Then you can export it to a
-- GraphML file so that external tools can handle it.
module NetSpider.Pangraph
       ( -- * Converters
         makePangraph,
         makePangraphIO,
         makeVertex,
         makeEdge,
         -- * Types
         Atom,
         ToAtom(..),
         ToAttributes(..),
         -- * Utility
         timestampAttributes,
         writePangraph,
         -- * Re-exports
         Attribute,
         Key,
         Value
       ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Greskell.Graph (PropertyMap(allProperties), Property(..))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.LocalTime (TimeZone(..))
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink,
    nodeId, nodeAttributes, nodeTimestamp, isOnBoundary,
    sourceNode, destinationNode, linkAttributes, linkTimestamp, isDirected
  )
import NetSpider.Timestamp (Timestamp(..), showEpochTime)
import Pangraph (Attribute, Key, Value)
import qualified Pangraph as P
import qualified Pangraph.GraphML.Writer as GraphML
import System.IO.Error (userError, ioError)

import NetSpider.Pangraph.Atom (Atom, ToAtom(..))

-- | Make Pangraph 'Attribute's from 'Timestamp'.
--
-- >>> import NetSpider.Timestamp (fromS)
-- >>> timestampAttributes $ fromS "2018-10-11T11:23:05"
-- [("@timestamp","1539256985000")]
-- >>> timestampAttributes $ fromS "2018-09-23T08:48:52+09:00"
-- [("@timestamp","1537660132000"),("@tz_offset_min","540"),("@tz_summer_only","False"),("@tz_name","")]
-- >>> timestampAttributes $ fromS "2018-12-30T22:00:12.109-04:00"
-- [("@timestamp","1546221612109"),("@tz_offset_min","-240"),("@tz_summer_only","False"),("@tz_name","")]
timestampAttributes :: Timestamp -> [Attribute]
timestampAttributes ts = epoch : tz_attrs
  where
    epoch = ("@timestamp", encodeUtf8 $ showEpochTime ts)
    tz_attrs = case timeZone ts of
      Nothing -> []
      Just tz -> [ ("@tz_offset_min", toAtom $ timeZoneMinutes tz),
                   ("@tz_summer_only", toAtom $ timeZoneSummerOnly tz),
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
makeVertex :: (ToAtom n, ToAttributes na) => SnapshotNode n na -> P.Vertex
makeVertex sn = P.makeVertex vid attrs
  where
    vid = toAtom $ nodeId sn
    attrs = (maybeList $ fmap timestampAttributes $ nodeTimestamp sn)
            ++ [("@is_on_boundary", toAtom $ isOnBoundary sn)]
            ++ (maybeList $ fmap toAttributes $ nodeAttributes sn)

-- | Make Pangraph 'P.Edge' from 'SnapshotLink'.
--
-- Link attributes (@la@) is converted to attributes of
-- 'P.Edge'. 'linkTimestamp' is converted by
-- 'timestampAttributes'. 'isDirected' is stored as \"\@is_directed\"
-- attribute..
makeEdge :: (ToAtom n, ToAttributes la) => SnapshotLink n la -> P.Edge
makeEdge sl = P.makeEdge (src, dest) attrs
  where
    src = toAtom $ sourceNode sl
    dest = toAtom $ destinationNode sl
    attrs = (timestampAttributes $ linkTimestamp sl)
            ++ [("@is_directed", toAtom $ isDirected sl)]
            ++ (toAttributes $ linkAttributes sl)

-- | Make a 'P.Pangraph'.
makePangraph :: (ToAtom n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> Maybe P.Pangraph
makePangraph ns ls = P.makePangraph (map makeVertex ns) (map makeEdge ls)

-- | Data types that can be converted into a list of Pangraph
-- 'Attribute's.
class ToAttributes a where
  toAttributes :: a -> [Attribute]

-- | No attribute.
instance ToAttributes () where
  toAttributes () = []

-- | Make 'Attribute' from key-value pairs.
instance (ToAtom k, ToAtom v) => ToAttributes [(k,v)] where
  toAttributes = map (\(k, v) -> (toAtom k, toAtom v))

instance (PropertyMap m, Property p, ToAtom v) => ToAttributes (m p v) where
  toAttributes = toAttributes . map toPair . allProperties
    where
      toPair p = (propertyKey p, propertyValue p)

-- | 'Nothing' is mapped to empty attributes.
instance ToAttributes a => ToAttributes (Maybe a) where
  toAttributes Nothing = []
  toAttributes (Just a) = toAttributes a

-- | Like 'makePangraph', but result of 'Nothing' is converted to an
-- IO exception.
makePangraphIO :: (ToAtom n, ToAttributes na, ToAttributes la)
               => [SnapshotNode n na] -> [SnapshotLink n la] -> IO P.Pangraph
makePangraphIO ns ls = case makePangraph ns ls of
                         Just p -> return p
                         Nothing -> ioError $ userError ("Malformed graph")

-- | Write 'P.Pangraph' to the given file in GraphML format.
writePangraph :: P.Pangraph -> FilePath -> IO ()
writePangraph p file = BS.writeFile file $ GraphML.write p
