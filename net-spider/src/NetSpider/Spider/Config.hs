{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Config
-- Description: Configuration of Spider
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Spider.Config
       ( Spider(..),
         Config(..),
         defConfig,
         Host,
         Port
       ) where

import Data.Greskell (Key)
import Network.Greskell.WebSocket (Host, Port)

import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (VNode)
import NetSpider.Spider.Unify (LinkSampleUnifier, unifyToOne)

-- | An IO agent of the NetSpider database.
--
-- - type @n@: node ID.
-- - type @na@: node attributes
-- - type @la@: link attributes
data Spider n na la =
  Spider
  { spiderConfig :: Config n na la,
    spiderClient :: Gr.Client
  }

-- | Configuration to create a 'Spider' object.
data Config n na la =
  Config
  { wsHost :: Gr.Host,
    -- ^ Host of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: \"localhost\".
    wsPort :: Gr.Port,
    -- ^ Port of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: 8182
    nodeIdKey :: Key VNode n,
    -- ^ Name of graph property that stores the node ID. Default:
    -- \"@node_id\".
    unifyLinkSamples :: LinkSampleUnifier n na la
    -- ^ See the document of 'LinkSampleUnifier'. Default:
    -- 'unifyToOne'.
  }

defConfig :: Config n na la
defConfig =
  Config
  { wsHost = "localhost",
    wsPort = 8182,
    nodeIdKey = "@node_id",
    unifyLinkSamples = unifyToOne
  }

