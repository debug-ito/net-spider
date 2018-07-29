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
  ( runBinder
  )
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.Greskell.WebSocket
  ( Host, Port
  )
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Neighbors (Neighbors(..), FoundLink(..))
import NetSpider.Snapshot (SnapshotElement)
import NetSpider.Timestamp (Timestamp(..))
import NetSpider.Spider.Internal.Graph
  ( EID, gMakeNeighbors, gGetNode, gMakeNode, gClearAll
  )

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
addNeighbors :: (ToJSON n, ToJSON p) => Spider -> Neighbors n p -> IO ()
addNeighbors spider nbs = do
  subject_vid <- getOrMakeNode spider $ subjectNode nbs
  link_pairs <- traverse linkAndTargetVID $ neighborLinks nbs
  makeNeighborsVertex spider subject_vid link_pairs $ observedTime nbs
  where
    linkAndTargetVID link = do
      target_vid <- getOrMakeNode spider $ targetNode link
      return (link, target_vid)

makeNeighborsVertex :: (ToJSON p)
                    => Spider
                    -> EID -- ^ subject node vertex ID
                    -> Vector (FoundLink n p, EID) -- ^ (link, target node vertex ID)
                    -> Timestamp
                    -> IO ()
makeNeighborsVertex spider subject_vid link_pairs timestamp =
  Gr.drainResults =<< Gr.submit (spiderClient spider) script mbindings
  where
    (script, bindings) = runBinder $ fmap void $ gMakeNeighbors subject_vid link_pairs timestamp
    mbindings = Just bindings

vToMaybe :: Vector a -> Maybe a
vToMaybe v = v V.!? 0

getNode :: (ToJSON n) => Spider -> n -> IO (Maybe EID)
getNode spider nid = fmap vToMaybe $ Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
  where
    (script, bindings) = runBinder $ gGetNode nid
    mbindings = Just bindings

getOrMakeNode :: (ToJSON n) => Spider -> n -> IO EID
getOrMakeNode spider nid = do
  mvid <- getNode spider nid
  case mvid of
   Just vid -> return vid
   Nothing -> makeNode
  where
    makeNode = expectOne =<< Gr.slurpResults =<< Gr.submit (spiderClient spider) script mbindings
    (script, bindings) = runBinder $ gMakeNode nid
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
clearAll spider = Gr.drainResults =<< Gr.submit (spiderClient spider) gClearAll Nothing

-- We can create much more complex function to query snapshot graphs,
-- but at least we need 'getLatestSnapshot'.
