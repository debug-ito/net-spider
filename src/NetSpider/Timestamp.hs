-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( Timestamp(..),
         fromEpochSecond
       ) where

import Data.Int (Int64)
import Data.UnixTime (UnixTime, fromEpochTime)
import Data.Time.LocalTime (TimeZone)
import Foreign.C.Types (CTime(CTime))

-- | Timestamp when the snapshot is observed.
data Timestamp =
  Timestamp
  { unixTime :: !UnixTime,
    timeZone :: !(Maybe TimeZone)
  }
  deriving (Show,Eq) -- UnixTime's Ord is dangerous.

-- | Make 'Timestamp' from seconds from the epoch. 'timeZone' is
-- 'Nothing'.
fromEpochSecond :: Int64 -> Timestamp
fromEpochSecond sec = Timestamp (fromEpochTime $ CTime sec) Nothing
