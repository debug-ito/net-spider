{-# LANGUAGE OverloadedStrings #-}
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
         -- * Convert to Timestamp
         parseTimestamp,
         fromS,
         fromZonedTime,
         fromUTCTime,
         fromSystemTime,
         fromLocalTime,
         -- * Convert from Timestamp
         toTime,
         toSystemTime,
         showTimestamp,
         showEpochTime
       ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), optional, empty)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (utcToSystemTime, SystemTime(..), systemToUTCTime)
import qualified Data.Time.Format as DTFormat
import Data.Time.LocalTime
  ( TimeZone(..), getZonedTime, ZonedTime(..), zonedTimeToUTC, LocalTime(LocalTime), localTimeToUTC,
    TimeOfDay(TimeOfDay), utcToLocalTime, utcToZonedTime
  )
import qualified Data.Time.LocalTime as LocalTime
import qualified Text.ParserCombinators.ReadP as P
import Text.Read (readEither)
import Text.Printf (printf)

import NetSpider.GraphML.Attribute
  ( ToAttributes(..),
    AttributeValue(..)
  )

-- | Timestamp when graph elements are observed.
data Timestamp =
  Timestamp
  { epochTime :: Int64,
    -- ^ Milliseconds since the epoch. The epoch is usually the
    -- beginning of year 1970.
    timeZone :: Maybe TimeZone
  }
  deriving (Show,Eq)

-- | Compare by 'epochTime' only. 'timeZone' is not used.
instance Ord Timestamp where
  compare l r = compare (epochTime l) (epochTime r)


-- | It can parse JSON string or object. If the input is a JSON
-- string, it is parsed by 'parseTimestamp'.
--
-- @since 0.4.1.0
instance FromJSON Timestamp where
  parseJSON (String t) = maybe (fail err_msg) return $ parseTimestamp ts
    where
      ts = unpack t
      err_msg = "Invalid Timestamp string: " ++ ts
  parseJSON (Object o) = Timestamp <$> (o .: "epoch_time") <*> parseTZ o
    where
      parseTZ ob = optional $ TimeZone
                   <$> (ob .: "tz_offset_min")
                   <*> (ob .: "tz_summer_only")
                   <*> (ob .: "tz_name")
  parseJSON _ = empty
  


-- | Convert to a JSON object.
--
-- @since 0.4.1.0
instance ToJSON Timestamp where
  toJSON t = Aeson.object $
             [ "epoch_time" .= epochTime t
             ]
             ++ tz_fields
    where
      tz_fields = (fmap . fmap) toJSON $ map fixKeyPrefix $ toAttributes $ timeZone t
      fixKeyPrefix (k, v) = (Text.tail k, v)


-- | @since 0.4.1.0
instance ToAttributes Timestamp where
  toAttributes t =
    [ ("@timestamp", AttrLong $ toInteger $ epochTime t),
      ("@timestamp_str", AttrString $ showTimestamp t)
    ] ++ timezone_attrs
    where
      timezone_attrs = maybe [] toAttributes $ timeZone t

-- | Make 'Timestamp' from milliseconds from the epoch. 'timeZone' is
-- 'Nothing'.
--
-- @since 0.2.0.0
fromEpochMillisecond :: Int64 -> Timestamp
fromEpochMillisecond msec = Timestamp msec Nothing

-- | Show 'Timestamp' with a basic ISO 8601 format.
--
-- >>> showTimestamp $ fromS "2019-10-20T12:45:00"
-- "2019-10-20T12:45:00.000"
-- >>> showTimestamp $ fromS "1999-03-21T10:11Z"
-- "1999-03-21T10:11:00.000Z"
-- >>> showTimestamp $ fromS "2016-11-30T22:03:00.034+09:00"
-- "2016-11-30T22:03:00.034+09:00"
-- >>> showTimestamp $ fromS "2000-04-07T09:31-05:00"
-- "2000-04-07T09:31:00.000-05:00"
--
-- @since 0.3.1.0
showTimestamp :: Timestamp -> Text
showTimestamp = pack . either simpleFormat formatZT . toTime
  where
    dtFormat :: DTFormat.FormatTime t => String -> t -> String
    dtFormat = DTFormat.formatTime DTFormat.defaultTimeLocale
    simpleFormat :: DTFormat.FormatTime t => t -> String
    simpleFormat = dtFormat "%Y-%m-%dT%H:%M:%S.%03q"
    formatZT zt = simpleFormat zt <> formatZone (zonedTimeZone zt)
    formatZone z = if timeZoneName z == ""
                   then formatOffset $ timeZoneMinutes z
                   else if z == LocalTime.utc
                        then "Z"
                        else dtFormat "%Z" z
    formatOffset o = sign <> hour <> ":" <> minute
      where
        sign = if o < 0 then "-" else "+"
        abo = abs o
        hour = printf "%02d" (abo `div` 60)
        minute = printf "%02d" (abo `mod` 60)

-- | Show 'epochTime' of 'Timestamp' as 'Text'.
--
-- @since 0.2.0.0
showEpochTime :: Timestamp -> Text
showEpochTime = pack . show . epochTime

-- | Convert to 'LocalTime' (if the 'Timestamp' has no time zone) or
-- 'ZonedTime' (otherwise). If it makes the 'LocalTime' as if the time
-- zone was UTC.
--
-- @since 0.3.1.0
toTime :: Timestamp -> Either LocalTime ZonedTime
toTime ts = maybe (Left localtime) (Right . toZT) $ timeZone ts
  where
    utctime = systemToUTCTime $ toSystemTime ts
    localtime = utcToLocalTime LocalTime.utc utctime
    toZT tz = utcToZonedTime tz utctime

-- | Convert 'Timestamp' to 'SystemTime'. It discards 'timeZone'
-- field.
--
-- >>> toSystemTime $ fromEpochMillisecond 1043221
-- MkSystemTime {systemSeconds = 1043, systemNanoseconds = 221000000}
-- >>> toSystemTime $ fromEpochMillisecond (-192332)
-- MkSystemTime {systemSeconds = -193, systemNanoseconds = 668000000}
--
-- @since 0.3.1.0
toSystemTime :: Timestamp -> SystemTime
toSystemTime ts = MkSystemTime sec nsec
  where
    epoch_time = epochTime ts
    sec = epoch_time `div` 1000
    nsec = fromIntegral (epoch_time `mod` 1000) * 1000000

-- | Get the current system time.
--
-- @since 0.2.0.0
now :: IO Timestamp
now = fmap fromZonedTime $ getZonedTime

-- | @since 0.2.0.0
fromZonedTime :: ZonedTime -> Timestamp
fromZonedTime zt =
  (fromUTCTime $ zonedTimeToUTC zt) { timeZone = Just $ zonedTimeZone zt }

-- | @since 0.2.0.0
fromUTCTime :: UTCTime -> Timestamp
fromUTCTime ut = (fromSystemTime $ utcToSystemTime ut) { timeZone = Just LocalTime.utc }

-- | @since 0.2.0.0
fromSystemTime :: SystemTime -> Timestamp
fromSystemTime stime = Timestamp { epochTime = epoch_time,
                                   timeZone = Nothing
                                 }
  where
    epoch_time = (systemSeconds stime * 1000)
                 + fromIntegral (systemNanoseconds stime `div` 1000000)

-- | Covert 'LocalTime' to 'Timestamp' assuming it's in UTC time
-- zone. The 'timeZone' field is 'Nothing'.
--
-- @since 0.2.0.0
fromLocalTime :: LocalTime -> Timestamp
fromLocalTime lt = (fromUTCTime $ localTimeToUTC LocalTime.utc lt) { timeZone = Nothing }

-- | Add time difference (in seconds) to the 'Timestamp'.
--
-- @since 0.2.0.0
addSec :: Int64 -> Timestamp -> Timestamp
addSec diff ts = ts { epochTime = (+ (diff * 1000)) $ epochTime ts }

-- | Unsafe version of 'parseTimestamp'.
--
-- @since 0.2.0.0
fromS :: String -> Timestamp
fromS s = maybe (error msg) id $ parseTimestamp s
  where
    msg = "Fail to parse " ++ s

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
--
-- @since 0.2.0.0
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

parserFracDec :: Read a => P.ReadP a
parserFracDec = do
  int <- P.munch1 isDigit
  frac <- fmap (maybe "" id) $ optional ((:) <$> P.char '.' <*> P.munch1 isDigit)
  return $ read (int ++ frac)

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
             <*> ((delim *> parserFracDec) P.<++ pure 0)
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

