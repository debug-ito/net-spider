-- |
-- Module: NetSpider.Spider.Internal.Type
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Type
       ( Spider(..)
       ) where

import qualified Network.Greskell.WebSocket as Gr

-- | An IO agent of the NetSpider database.
--
-- - type @n@: node ID.
-- - type @na@: node attributes
-- - type @la@: link attributes
data Spider n na la =
  Spider
  { spiderClient :: Gr.Client
  }

