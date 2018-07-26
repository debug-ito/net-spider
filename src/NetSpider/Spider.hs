-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         addNeighbors
       ) where

import NetSpider.Neighbors (Neighbors)

-- | 'Spider' is an IO agent of NetSpider database.
data Spider = Spider

-- | Add a snapshot of 'Neighbors' to the NetSpider database.
addNeighbors :: Spider -> Neighbors n p -> IO ()
addNeighbors = undefined
