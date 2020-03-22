-- |
-- Module: NetSpider.SeqID
-- Description: Sequential node IDs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.SeqID
  ( SeqIDMaker
  ) where

-- | 'SeqIDMaker' converts node ID type @n@ into the new node ID type
-- @i@. The type @i@ is supposed to be an 'Enum', and it generates the
-- node ID of type @i@ sequentially for each node ID of type @n@.
data SeqIDMaker n i
