-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( Timestamp(..),
         fromEpochSecond,
         now
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
--
-- 'epochTime' is in milliseconds. Its epoch is usually the beginning
-- of year 1970.
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
