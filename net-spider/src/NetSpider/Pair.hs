-- |
-- Module: NetSpider.Pair
-- Description: Swap-insensitive two-element homogeneous tuple
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Pair
       ( Pair(..),
         sortPair
       ) where

import Control.Applicative (Applicative(..))
import Data.Foldable (Foldable(..))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Traversable (Traversable(..))

-- | 'Pair' is a two-element tuple of the same type that is
-- insensitive to swapping. 'Eq', 'Ord' and 'Hashable' instances treat
-- 'Pair's with swapped elements as equivalent.
newtype Pair a = Pair { unPair :: (a,a) }
               deriving (Show)

instance Eq a => Eq (Pair a) where
  (Pair (al, ar)) == (Pair (bl, br)) = (al == bl && ar == br) || (al == br && ar == bl)

instance Ord a => Ord (Pair a) where
  compare l r = compare (unPair $ sortPair l) (unPair $ sortPair r)

instance (Ord a, Hashable a) => Hashable (Pair a) where
  hashWithSalt s p = hashWithSalt s $ unPair $ sortPair p

instance Functor Pair where
  fmap f (Pair (l,r)) = Pair (f l, f r)

instance Applicative Pair where
  pure a = Pair (a,a)
  (Pair (fl,fr)) <*> (Pair (al,ar)) = Pair (fl al, fr ar)

instance Foldable Pair where
  foldr f start (Pair (l,r)) = foldr f start [l,r]

instance Traversable Pair where
  traverse f (Pair (l,r)) = fmap Pair $ (,) <$> (f l) <*> (f r)

-- | Sort the elements in the 'Pair'.
sortPair :: Ord a => Pair a -> Pair a
sortPair p@(Pair (l,r)) = if l <= r
                          then p
                          else Pair (r,l)

