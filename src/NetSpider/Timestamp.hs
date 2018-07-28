-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( Timestamp(..)
       ) where

import Data.UnixTime (UnixTime)
import Data.Time.LocalTime (TimeZone)


-- | Timestamp when the snapshot is observed.
data Timestamp =
  Timestamp
  { unixTime :: !UnixTime,
    timeZone :: !(Maybe TimeZone)
  }
  deriving (Show,Eq) -- UnixTime's Ord is dangerous.

