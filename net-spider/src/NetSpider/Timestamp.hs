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
         iso8601Parse,
         fromZonedTime,
         fromUTCTime,
         fromSystemTime,
         fromLocalTime
       ) where

import Control.Applicative ((<|>), (<$>))
import Data.Int (Int64)
import Data.Time.LocalTime
  ( TimeZone(timeZoneMinutes), getZonedTime, ZonedTime(..), zonedTimeToUTC, LocalTime, localTimeToUTC
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

-- | Parse a string of ISO8601 format into 'Timestamp'.
--
-- >>> let timeAndOffset ts = (epochTime ts, fmap timeZoneMinutes $ timeZone ts)
-- >>> fmap timeAndOffset $ iso8601Parse "2018-10-11T11:20:10"
-- Just (1539256810000, Nothing)
-- >>> iso8601Parse "2018-10-11 11:20:10"
-- Just (1539256810000, Nothing)
-- >>> iso8601Parse "2015-03-23 03:33Z"
-- Just (1427081580000, Just 0)
-- >>> iso8601Parse "1999-01-05 20:34:44.211+09:00"
-- Just (915536084211, Just 540)
-- >>> iso8601Parse "2007-08-20T22:25-07:00"
-- Just (1187673900000, Just (-420))
iso8601Parse :: String -> Maybe Timestamp
iso8601Parse s = (fromZonedTime <$> parseZoned s)
                 <|> (fromUTCTime <$> parseUTC s)
                 <|> (fromLocalTime <$> parseLocal s)
  where
    parserBase = readPTime True defaultTimeLocale
    parserYMD = parserBase "%Y-%m-%d"
    parserZoned = undefined
    -- TODO: うーん、ReadPを直接使うほうがラクそうな気がする。

