-- |
-- Module: NetSpider.Pangraph
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.Pangraph
       () where

import qualified Pangraph as P

makeVertex :: SnapshotNode n na -> P.Vertex
makeVertex = undefined

makeEdge :: SnapshotLink n fla -> P.Edge
makeEdge = undefined

makePangraph :: [SnapshotNode n na] -> [SnapshotLink n fla] -> Maybe P.Pangraph
makePangraph = undefined
