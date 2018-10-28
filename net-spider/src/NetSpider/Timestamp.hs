-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( -- * The type
         Timestamp(..),
         -- * Construction
         fromEpochMillisecond,
         now,
         -- * Manipulation
         addSec,
         -- * Conversion
         fromZonedTime,
         fromUTCTime,
         fromSystemTime,
         fromLocalTime
       ) where

import Data.Int (Int64)
import Data.Time.LocalTime
  ( TimeZone, getZonedTime, ZonedTime(..), zonedTimeToUTC, LocalTime, localTimeToUTC
  )
import qualified Data.Time.LocalTime as LocalTime
import Data.Time.Clock (UTCTime)
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

-- | Make 'Timestamp' from milliseconds from the epoch. 'timeZone' is
-- 'Nothing'.
fromEpochMillisecond :: Int64 -> Timestamp
fromEpochMillisecond msec = Timestamp msec Nothing

-- | Get the current system time.
now :: IO Timestamp
now = fmap fromZonedTime $ getZonedTime

fromZonedTime :: ZonedTime -> Timestamp
fromZonedTime zt =
  (fromUTCTime $ zonedTimeToUTC zt) { timeZone = Just $ zonedTimeZone zt }

fromUTCTime :: UTCTime -> Timestamp
fromUTCTime ut = (fromSystemTime $ utcToSystemTime ut) { timeZone = Just LocalTime.utc }

fromSystemTime :: SystemTime -> Timestamp
fromSystemTime stime = Timestamp { epochTime = epoch_time,
                                   timeZone = Nothing
                                 }
  where
    epoch_time = (systemSeconds stime * 1000)
                 + fromIntegral (systemNanoseconds stime `div` 1000000)

-- | Covert 'LocalTime' to 'Timestamp' assuming it's in UTC time
-- zone. The 'timeZone' field is 'Nothing'.
fromLocalTime :: LocalTime -> Timestamp
fromLocalTime lt = (fromUTCTime $ localTimeToUTC LocalTime.utc lt) { timeZone = Nothing }

-- | Add time difference (in seconds) to the 'Timestamp'.
addSec :: Int64 -> Timestamp -> Timestamp
addSec diff ts = ts { epochTime = (+ (diff * 1000)) $ epochTime ts }

-- -- | Parse a string of ISO8601 format into 'Timestamp'.
-- iso8601parse :: MonadFail m => String -> m Timestamp
-- iso8601parse = 
