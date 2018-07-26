-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         addNeighbors,
         getLatestSnapshot
       ) where

import Data.Vector (Vector)

import NetSpider.Neighbors (Neighbors)
import NetSpider.Snapshot (SnapshotElement)

-- | An IO agent of the NetSpider database.
data Spider = Spider

-- | Add a snapshot of 'Neighbors' to the NetSpider database.
addNeighbors :: Spider -> Neighbors n p -> IO ()
addNeighbors = undefined

-- | Get the latest snapshot graph from the NetSpider databsae.
getLatestSnapshot :: Spider -> IO (Vector (SnapshotElement n p))
getLatestSnapshot = undefined
