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
import NetSpider.Snapshot (SnapshotNode, SnapshotLink)
import qualified Pangraph as P

makeVertex :: SnapshotNode n na -> P.Vertex
makeVertex = undefined

makeEdge :: SnapshotLink n fla -> P.Edge
makeEdge = undefined

makePangraph :: [SnapshotNode n na] -> [SnapshotLink n fla] -> Maybe P.Pangraph
makePangraph = undefined

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
