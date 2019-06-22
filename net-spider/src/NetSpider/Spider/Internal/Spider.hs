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
data Spider =
  Spider
  { spiderConfig :: Config,
    spiderClient :: Gr.Client
  }

