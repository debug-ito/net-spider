-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         connectWS,
         close,
         addNeighbors,
         getLatestSnapshot
       ) where

import Data.Vector (Vector)
import Network.Greskell.WebSocket
  ( Host, Port
  )

import NetSpider.Neighbors (Neighbors)
import NetSpider.Snapshot (SnapshotElement)

-- | An IO agent of the NetSpider database.
data Spider = Spider

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Host -> Port -> IO Spider
connectWS = undefined

-- | Close and release the 'Spider' object.
close :: Spider -> IO ()
close = undefined

-- | Add an observation of 'Neighbors' to the NetSpider database.
addNeighbors :: Spider -> Neighbors n p -> IO ()
addNeighbors = undefined

-- | Get the latest snapshot graph from the NetSpider databsae.
getLatestSnapshot :: Spider -> IO (Vector (SnapshotElement n p))
getLatestSnapshot = undefined
