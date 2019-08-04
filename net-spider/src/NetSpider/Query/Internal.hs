-- |
-- Module: NetSpider.Query.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Query.Internal
       ( FoundNodePolicy(..)
       ) where

-- | Policy to treat 'FoundNode's (local findings) when the spider
-- creates the snapshot graph.
--
-- 'Eq' instance was added in version 0.3.2.0
--
-- @since 0.2.0.0
data FoundNodePolicy n na=
    PolicyOverwrite
  | PolicyAppend
  deriving (Show,Eq)

