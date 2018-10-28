-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( Timestamp(..),
         fromEpochSecond,
         now,
         addSec
       ) where

import Data.Int (Int64)
import Data.Time.LocalTime
  ( TimeZone, getZonedTime, ZonedTime(..), zonedTimeToUTC
  )
import Data.Time.Clock.System (utcToSystemTime, SystemTime(..))

-- | Timestamp when graph elements are observed.
data Timestamp =
  Timestamp
  { epochTime :: !Int64,
    -- ^ Milliseconds since the epoch. The epoch is usually the
    -- beginning of year 1970.
    timeZone :: !(Maybe TimeZone)
  }
  deriving (Show,Eq)

-- | Compare by 'epochTime' only. 'timeZone' is not used.
instance Ord Timestamp where
  compare l r = compare (epochTime l) (epochTime r)

-- | Make 'Timestamp' from seconds from the epoch. 'timeZone' is
-- 'Nothing'.
fromEpochSecond :: Int64 -> Timestamp
fromEpochSecond sec = Timestamp sec Nothing

-- | Get the current system time.
now :: IO Timestamp
now = do
  zt <- getZonedTime
  return $ Timestamp { epochTime = ztToEpochTime zt,
                       timeZone = Just $ zonedTimeZone zt
                     }
  where
    ztToEpochTime zt = stimeToEpochTime $ utcToSystemTime $ zonedTimeToUTC zt
    stimeToEpochTime stime = (systemSeconds stime * 1000)
                             + fromIntegral (systemNanoseconds stime `div` 1000000)

-- | Add time difference (in seconds) to the 'Timestamp'.
addSec :: Int64 -> Timestamp -> Timestamp
addSec diff ts = ts { epochTime = (+ (diff * 1000)) $ epochTime ts }
