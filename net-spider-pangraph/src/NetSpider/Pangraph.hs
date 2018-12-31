{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module: NetSpider.Pangraph
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.Pangraph
       ( makePangraph,
         makeVertex,
         makeEdge,
         ToAttributes(..)
       ) where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink,
    nodeId, nodeAttributes,
    sourceNode, destinationNode, linkAttributes
  )
import qualified Pangraph as P

makeVertex :: (Show n, ToAttributes na) => SnapshotNode n na -> P.Vertex
makeVertex sn = P.makeVertex vid attrs
  where
    vid = spack $ nodeId sn
    attrs = case nodeAttributes sn of
      Nothing -> []
      Just a -> toAttributes a
    -- how about timestamp and isBoundary?

makeEdge :: (Show n, ToAttributes la) => SnapshotLink n la -> P.Edge
makeEdge sl = P.makeEdge (src, dest) attrs
  where
    src = spack $ sourceNode sl
    dest = spack $ destinationNode sl
    attrs = toAttributes $ linkAttributes sl
    -- how about timestamp and isLinkDirected?

makePangraph :: (Show n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> Maybe P.Pangraph
makePangraph ns ls = P.makePangraph (map makeVertex ns) (map makeEdge ls)

spack :: Show a => a -> ByteString
spack = encodeUtf8 . pack . show

-- | Data types that can be converted into a list of Pangraph
-- 'P.Attribute's.
class ToAttributes a where
  toAttributes :: a -> [P.Attribute]

-- | No attribute.
instance ToAttributes () where
  toAttributes () = []

-- | Show key and value to make 'P.Attribute'.
instance (Show k, Show v) => ToAttributes [(k,v)] where
  toAttributes = map (\(k, v) -> (spack k, spack v))
