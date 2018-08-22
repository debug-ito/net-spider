{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module: NetSpider.Queue
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Queue
       ( Queue,
         newQueue,
         pushQueue,
         popQueue
       ) where

import Data.Foldable (Foldable)
import Data.Monoid (Monoid)
import Data.Semigroup (Semigroup)
import Data.Sequence (Seq, (|>), viewl, ViewL(..), fromList)
import Data.Traversable (Traversable)

-- | Pure FIFO queue.
newtype Queue a = Queue (Seq a)
                deriving (Show,Eq,Ord,Semigroup,Monoid,Foldable,Functor,Applicative,Monad)

newQueue :: [a] -> Queue a
newQueue = Queue . fromList

pushQueue :: a -> Queue a -> Queue a
pushQueue item (Queue s) = Queue (s |> item)

popQueue :: Queue a -> (Maybe a, Queue a)
popQueue q@(Queue s) =
  case viewl s of
   EmptyL -> (Nothing, q)
   (item :< rest) -> (Just item, Queue rest)
