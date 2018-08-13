{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Internal.Type
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Type
       ( Spider(..),
         Config(..),
         defConfig
       ) where

import Data.Greskell (Key)
import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (VNode)

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
    nodeIdKey :: Key VNode n
    -- ^ Name of graph property that stores the node ID. Default:
    -- \"@node_id\".
  }
  deriving (Show)

defConfig :: Config n na la
defConfig =
  Config
  { wsHost = "localhost",
    wsPort = 8182,
    nodeIdKey = "@node_id"
  }
