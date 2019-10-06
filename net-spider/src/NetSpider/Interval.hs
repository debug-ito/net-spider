-- |
-- Module: NetSpider.Interval
-- Description: Interval type and Interval of Timestamps
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- Re-exports of 'Interval' type and additional utility.
--
-- @since 0.3.2.0
module NetSpider.Interval
  ( -- * Re-exports
    Interval,
    Extended(..),
    interval, (<=..<=), (<..<=), (<=..<), (<..<),
    -- * Types
    IntervalEnd,
    ErrorMsg,
    -- * Parsers
    parseTimeIntervalEnd,
    parseIntervalEnd,
    -- * Utility
    secUpTo,
    secSince,
    secUntil
  ) where

import Data.ExtendedReal (Extended(..))
import Data.Int (Int64)
import Data.Interval (Interval, interval, (<=..<=), (<..<=), (<=..<), (<..<))
import qualified Data.Interval as Interval

import NetSpider.Timestamp (Timestamp, addSec, parseTimestamp, fromEpochMillisecond)

-- | Upper or lower end of 'Interval'. The 'Bool' field is 'True' if
-- the end is inclusive.
--
-- @since 0.3.2.0
type IntervalEnd a = (Extended a, Bool)

-- | Error message type.
type ErrorMsg = String

-- | Parse the 'String' into 'IntervalEnd' @a@, with the
-- user-supplied parser for @a@. See 'parseTimeIntervalEnd' for
-- example.
--
-- @since 0.3.2.0
parseIntervalEnd :: (String -> Either ErrorMsg a) -- ^ parser for the type variable @a@
                 -> String -- ^ input to be parsed
                 -> Either ErrorMsg (IntervalEnd a)
parseIntervalEnd parseFinite input = do
  (is_inclusive, value_part) <- parseInclusive input
  value <- parseValue value_part
  return (value, is_inclusive)
  where
    parseInclusive "" = Right (True, "")
    parseInclusive ('i' : rest) = Right (True, rest)
    parseInclusive ('x' : rest) = Right (False, rest)
    parseInclusive s = Right (True, s)
    parseValue "+inf" = Right PosInf
    parseValue "-inf" = Right NegInf
    parseValue s = either (Left . makeErr) (Right . Finite) $ parseFinite s
      where
        makeErr e = "Parse error: " ++ s ++ ": " ++ e

-- | Parse the 'String' into an end of time interval. It uses
-- 'parseIntervalEnd'.
--
-- If the 'String' is prefixed with \'i\', the end is inclusive. If
-- the prefix is \'x\', the end is exclusive. Without such prefix,
-- the end is inclusive by default.
--
-- Timestamp is parsed by 'parseTimestamp'. Positive infinity is
-- expressed as \'+inf\' (note that \'+\' is mandatory), and
-- negative infinity as \'-inf\'.
--
-- >>> parseTimeIntervalEnd "2019-10-09T12:03:22"
-- Right (Finite (Timestamp {epochTime = 1570622602000, timeZone = Nothing}),True)
-- >>> parseTimeIntervalEnd "i2019-10-09T12:03:22"
-- Right (Finite (Timestamp {epochTime = 1570622602000, timeZone = Nothing}),True)
-- >>> parseTimeIntervalEnd "x2019-10-09T12:03:22"
-- Right (Finite (Timestamp {epochTime = 1570622602000, timeZone = Nothing}),False)
-- >>> parseTimeIntervalEnd "+inf"
-- Right (PosInf,True)
-- >>> parseTimeIntervalEnd "i+inf"
-- Right (PosInf,True)
-- >>> parseTimeIntervalEnd "x+inf"
-- Right (PosInf,False)
-- >>> parseTimeIntervalEnd "-inf"
-- Right (NegInf,True)
-- >>> parseTimeIntervalEnd "i-inf"
-- Right (NegInf,True)
-- >>> parseTimeIntervalEnd "x-inf"
-- Right (NegInf,False)
--
-- @since 0.3.2.0
parseTimeIntervalEnd :: String -> Either ErrorMsg (IntervalEnd Timestamp)
parseTimeIntervalEnd = parseIntervalEnd parseTimestampE
  where
    parseTimestampE t = maybe (Left err_msg) Right $ parseTimestamp t
      where
        err_msg = "Cannot parse as a Timestamp: " ++ t

-- | @s `secUpTo` ts@ returns the time interval of length @s@ (in
-- seconds) and up to @ts@. The interval is inclusive for both ends.
--
-- @since 0.2.0.0
secUpTo :: Int64 -> Timestamp -> Interval Timestamp
secUpTo len end = Finite start <=..<= Finite end
  where
    start = addSec (-len) end

-- | @d `secSince` ts@ returns the time interval of length @d@ seconds
-- from the timestamp @ts@. If @ts@ is inclusive (exclusive), the end
-- of the interval is exclusive (inclusive), respectively.
--
-- >>> 60 `secSince` (Finite $ fromEpochMillisecond 1000, True)
-- Finite (Timestamp {epochTime = 1000, timeZone = Nothing}) <=..< Finite (Timestamp {epochTime = 61000, timeZone = Nothing})
-- >>> 60 `secSince` (Finite $ fromEpochMillisecond 1000, False)
-- Finite (Timestamp {epochTime = 1000, timeZone = Nothing}) <..<= Finite (Timestamp {epochTime = 61000, timeZone = Nothing})
-- >>> 60 `secSince` (PosInf, False)
-- empty
-- >>> 60 `secSince` (NegInf, False)
-- empty
--
-- @since 0.3.3.0
secSince :: Int64 -- ^ duration in seconds
         -> IntervalEnd Timestamp -- ^ the start of the interval
         -> Interval Timestamp
secSince len start@(Finite start_ts, inc) = interval start (Finite $ addSec len start_ts, not inc)
secSince _ _ = Interval.empty

-- | @d `secUntil` ts@ returns the time interval of length @d@ seconds
-- up to the timestamp @ts@. If @ts@ is inclusive (exclusive), the
-- start of the interval is exclusive (inclusive), respectively.
-- 
-- >>> 60 `secUntil` (Finite $ fromEpochMillisecond 150000, True)
-- Finite (Timestamp {epochTime = 90000, timeZone = Nothing}) <..<= Finite (Timestamp {epochTime = 150000, timeZone = Nothing})
-- >>> 60 `secUntil` (Finite $ fromEpochMillisecond 150000, False)
-- Finite (Timestamp {epochTime = 90000, timeZone = Nothing}) <=..< Finite (Timestamp {epochTime = 150000, timeZone = Nothing})
-- >>> 60 `secUntil` (PosInf, False)
-- empty
-- >>> 60 `secUntil` (NegInf, False)
-- empty
--
-- @since 0.3.3.0
secUntil :: Int64 -- ^ duration in seconds
         -> IntervalEnd Timestamp -- ^ the end of the interval
         -> Interval Timestamp
secUntil len end@(Finite end_ts, inc) = interval (Finite $ addSec (-len) end_ts, not inc) end
secUntil _ _ = Interval.empty
