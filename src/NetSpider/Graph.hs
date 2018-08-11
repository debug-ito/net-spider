-- |
-- Module: NetSpider.Graph
-- Description: Graph data models
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module NetSpider.Graph
       ( -- * EID
         EID,
         -- * VNode
         VNode,
         -- * VFoundNode
         VFoundNode,
         NodeAttributes(..),
         -- * EFinds
         EFinds,
         LinkAttributes(..)
       ) where

import NetSpider.Graph.Internal
  ( EID, VNode, VFoundNode, NodeAttributes(..),
    EFinds, LinkAttributes(..)
  )
