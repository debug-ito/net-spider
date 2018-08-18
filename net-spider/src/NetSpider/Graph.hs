-- |
-- Module: NetSpider.Graph
-- Description: Graph data models
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module NetSpider.Graph
       ( -- * Attributes classes
         NodeAttributes(..),
         LinkAttributes(..),
         -- * Graph element types
         EID,
         VNode,
         VFoundNode,
         EFinds
       ) where

import NetSpider.Graph.Internal
  ( EID, VNode, VFoundNode, NodeAttributes(..),
    EFinds, LinkAttributes(..)
  )
