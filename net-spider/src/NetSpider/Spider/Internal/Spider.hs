-- |
-- Module: NetSpider.Spider.Internal.Spider
-- Description: Spider type and its internal
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Spider
       ( Spider(..)
       ) where

import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Spider.Config (Config)

-- | An IO agent of the NetSpider database.
--
-- - Type @n@: node ID. Note that type of node ID has nothing to do
--   with type of vertex ID used by Gremlin implementation. Node ID
--   (in net-spider) is stored as a vertex property. See 'nodeIdKey'
--   config field.
-- - Type @na@: node attributes. It should implement
--   'NetSpider.Graph.NodeAttributes' class. You can set this to @()@
--   if you don't need node attributes.
-- - Type @fla@: attributes of found links. It should implement
--   'NetSpider.Graph.LinkAttributes' class. You can set this to @()@
--   if you don't need link attributes.
data Spider n na fla =
  Spider
  { spiderConfig :: Config n na fla,
    spiderClient :: Gr.Client
  }

