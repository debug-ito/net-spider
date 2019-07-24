-- |
-- Module: NetSpider.CLI.Timestamp
-- Description: CLI option parser for Timestamp
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Timestamp
  ( parseTimeIntervalEnd,
    IntervalEnd,
    makeInterval,
    ErrorMsg
  ) where

import qualified NetSpider.Query as Q
import NetSpider.Timestamp (Timestamp, parseTimestamp)

type ErrorMsg = String

-- | Upper or lower end of 'Q.Interval'. The 'Bool' field is 'True'
-- if the end is inclusive.
type IntervalEnd a = (Q.Extended a, Bool)

makeInterval :: Ord a => IntervalEnd a -> IntervalEnd a -> Q.Interval a
makeInterval (lv, li) (uv, ui) = construct lv uv
  where
    construct = 
      case (li, ui) of
        (True, True) -> (Q.<=..<=)
        (False, True) -> (Q.<..<=)
        (True, False) -> (Q.<=..<)
        (False, False) -> (Q.<..<)

-- | Parse the 'String' into an end of time interval.
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
parseTimeIntervalEnd :: String -> Either ErrorMsg (IntervalEnd Timestamp)
parseTimeIntervalEnd input = do
  (is_inclusive, value_part) <- parseInclusive input
  value <- parseValue value_part
  return (value, is_inclusive)
  where
    parseInclusive "" = Left "Input is empty."
    parseInclusive ('i' : rest) = Right (True, rest)
    parseInclusive ('x' : rest) = Right (False, rest)
    parseInclusive s = Right (True, s)
    parseValue "+inf" = Right Q.PosInf
    parseValue "-inf" = Right Q.NegInf
    parseValue s = maybe (Left err_msg) (Right . Q.Finite) $ parseTimestamp s
      where
        err_msg = "Cannot parse into Timestamp: " ++ s

