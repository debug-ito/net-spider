-- |
-- Module: NetSpider.Interval
-- Description: Interval type and Interval of Timestamps
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.Interval
  ( -- * Re-exports
    -- ** Types
    Interval,
    Extended(..),
    -- ** Functions
    (<=..<=), (<..<=), (<=..<), (<..<),
    -- * Utility
    secUpTo
  ) where

import Data.ExtendedReal (Extended(..))
import Data.Int (Int64)
import Data.Interval (Interval, (<=..<=), (<..<=), (<=..<), (<..<))
import qualified Data.Interval as Interval

import NetSpider.Timestamp (Timestamp, addSec)

-- | @s `secUpTo` ts@ returns the time interval of length @s@ (in
-- seconds) and up to @ts@. The interval is inclusive for both ends.
--
-- @since 0.2.0.0
secUpTo :: Int64 -> Timestamp -> Interval Timestamp
secUpTo len end = Finite start <=..<= Finite end
  where
    start = addSec (-len) end

