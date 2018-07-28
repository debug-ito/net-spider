{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider
-- Description: Spider type.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider
       ( Spider,
         connectWS,
         Host,
         Port,
         close,
         addNeighbors,
         getLatestSnapshot,
         clearAll
       ) where

import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import Data.Greskell
  ( source, sV', sAddV',
    ($.), gDrop, liftWalk, gHas2, gId, gProperty, gHasLabel,
    GValue,
    runBinder, newBind
  )
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Greskell.WebSocket
  ( Host, Port
  )
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Neighbors (Neighbors)
import NetSpider.Snapshot (SnapshotElement)

-- | An IO agent of the NetSpider database.
data Spider =
  Spider
  { spiderClient :: Gr.Client
  }

-- | Connect to the WebSocket endpoint of Tinkerpop Gremlin Server
-- that hosts the NetSpider database.
connectWS :: Host -> Port -> IO Spider
connectWS host port = fmap Spider $ Gr.connect host port

-- | Close and release the 'Spider' object.
close :: Spider -> IO ()
close sp = Gr.close $ spiderClient sp

-- | Add an observation of 'Neighbors' to the NetSpider database.
addNeighbors :: Spider -> Neighbors n p -> IO ()
addNeighbors = undefined


-- | ID of the Vertex kept internally in the graph DB.
type VertexID = GValue

vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider -> n -> IO (Maybe VertexID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
  where
    (script, bindings) = runBinder $ do
      var_nid <- newBind nid
      return $ gId $. gHas2 "@node_id" var_nid $. gHasLabel "node" $. sV' [] $ source "g"
    mbindings = Just bindings

getOrMakeNode :: (ToJSON n) => Spider -> n -> IO VertexID
getOrMakeNode spider nid = do
  mvid <- getNode spider nid
  case mvid of
   Just vid -> return vid
   Nothing -> makeNode
  where
    makeNode = expectOne =<< Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
    (script, bindings) = runBinder $ do
      var_nid <- newBind nid
      return $ liftWalk gId $. gProperty "@node_id" var_nid $. sAddV' "node" $ source "g"
    mbindings = Just bindings
    expectOne v = case vToMaybe v of
      Just e -> return e
      Nothing -> throwString "Expects at least single result, but got nothing."
      -- TODO: make decent exception spec.

-- | Get the latest snapshot graph from the NetSpider database.
--
-- This function is very simple, and should be used only for testing.
-- This function starts from an arbitrary node, traverses the history
-- graph using the latest links with unlimited number of hops.
getLatestSnapshot :: Spider -> IO (Vector (SnapshotElement n p))
getLatestSnapshot = undefined

-- | Clear all content in the NetSpider database. This is mainly for
-- testing.
clearAll :: Spider -> IO ()
clearAll spider = Gr.drainResults =<< Gr.submit (spiderClient spider) script Nothing
  where
    script = void $ gDrop $. liftWalk $ sV' [] $ source "g"

-- We can create much more complex function to query snapshot graphs,
-- but at least we need 'getLatestSnapshot'.
