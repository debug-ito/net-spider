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
         parseTimestamp,
         fromZonedTime,
         fromUTCTime,
         fromSystemTime,
         fromLocalTime
       ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), optional)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime
  ( TimeZone(..), getZonedTime, ZonedTime(..), zonedTimeToUTC, LocalTime(LocalTime), localTimeToUTC,
    TimeOfDay(TimeOfDay)
  )
import qualified Data.Time.LocalTime as LocalTime
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (utcToSystemTime, SystemTime(..))
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)

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

-- | Parse a string into 'Timestamp'. The format is like ISO8601 with
-- a little relaxation.
--
-- >>> let timeAndOffset ts = (epochTime ts, fmap timeZoneMinutes $ timeZone ts)
-- >>> fmap timeAndOffset $ parseTimestamp "2018-10-11T11:20:10"
-- Just (1539256810000,Nothing)
-- >>> fmap timeAndOffset $ parseTimestamp "2018-10-11 11:20:10"
-- Just (1539256810000,Nothing)
-- >>> fmap timeAndOffset $ parseTimestamp "2015-03-23 03:33Z"
-- Just (1427081580000,Just 0)
-- >>> fmap timeAndOffset $ parseTimestamp "1999-01-05 20:34:44.211+09:00"
-- Just (915536084211,Just 540)
-- >>> fmap timeAndOffset $ parseTimestamp "2007/08/20T22:25-07:00"
-- Just (1187673900000,Just (-420))
parseTimestamp :: String -> Maybe Timestamp
parseTimestamp s = toTs $ sortByLeftover $ P.readP_to_S parserTimestamp s
  where
    sortByLeftover = sortOn $ \(_, leftover) -> length leftover
    toTs ((ret, _) : _) = Just ret
    toTs [] = Nothing

parserTimestamp :: P.ReadP Timestamp
parserTimestamp = do
  day <- parserDay <* delim
  time <- parserTime
  mtz <- optional (parserUTC P.+++ parserOffset)
  let ltime = LocalTime day time
  case mtz of
   Nothing -> return $ fromLocalTime ltime
   Just tz -> return $ fromZonedTime $ ZonedTime ltime tz
  where
    delim = P.choice $ map P.char " T"

parserRead :: Read a => String -> P.ReadP a
parserRead input = either fail return $ readEither input

parserDec :: Read a => P.ReadP a
parserDec = parserRead =<< P.munch1 isDigit

parserDay :: P.ReadP Day
parserDay = fromGregorian
            <$> (parserDec <* delim)
            <*> (parserDec <* delim)
            <*> parserDec
  where
    delim = P.choice $ map P.char "-/"

parserTime :: P.ReadP TimeOfDay
parserTime = TimeOfDay
             <$> parserDec
             <*> (delim *> parserDec)
             <*> ((delim *> parserDec) P.<++ pure 0)
  where
    delim = P.char ':'


parserUTC :: P.ReadP TimeZone
parserUTC = do
  s <- P.get
  case s of
   'Z' -> return LocalTime.utc
   c -> fail ("Not a UTC symbol: " ++ show c)

data OffsetSign = OffsetPlus
                | OffsetMinus
                deriving (Show,Eq,Ord,Enum,Bounded)

parserOffset :: P.ReadP TimeZone
parserOffset = offsetToTz <$> osign <*> (parserDec <* delim) <*> parserDec
  where
    osign = do
      s <- P.get
      case s of
       '+' -> return OffsetPlus
       '-' -> return OffsetMinus
       c -> fail ("Not a sign symbol: " ++ show c)
    delim = optional $ P.char ':'

offsetToTz :: OffsetSign -> Int -> Int -> TimeZone
offsetToTz osign h m = TimeZone { timeZoneMinutes = intsign * (h * 60 + m),
                                  timeZoneSummerOnly = False,
                                  timeZoneName = ""
                                }
  where
    intsign = case osign of
      OffsetPlus -> 1
      OffsetMinus -> -1

