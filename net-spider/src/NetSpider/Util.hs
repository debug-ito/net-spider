-- |
-- Module: NetSpider.Util
-- Description: general-purpose utilities
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Util
       ( groupWith
       ) where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | 'Vector' version of GHC.Exts.groupWith. Note that the order of
-- subgroups is undefined, but the order of elements within each
-- subgroup is preserved.
groupWith :: Ord b => (a -> b) -> Vector a -> Vector (Vector a)
groupWith getKey = mToV . foldl' f M.empty
  where
    f m a = M.insertWith (\new old -> old <> new) (getKey a) (return a) m
    mToV = V.fromList . M.elems
