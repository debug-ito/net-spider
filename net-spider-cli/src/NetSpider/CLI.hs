-- |
-- Module: NetSpider.CLI
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.CLI
       ( parserSnapshotQuery,
         Config(..),
         parseTimeIntervalEnd,
         IntervalEnd
       ) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Monoid (mconcat)
import qualified NetSpider.Query as Q
import qualified NetSpider.Spider.Config as SConf
import NetSpider.Timestamp (Timestamp, parseTimestamp)
import qualified Options.Applicative as Opt

-- | Configuration for CLI.
data Config n na fla sla =
  Config
  { nodeIDParser :: Opt.ReadM n,
    -- ^ Parser that reads an command-line option to generate a node
    -- ID.
    basisSnapshotQuery :: Q.Query n na fla sla
    -- ^ Basis for queries for SnapshotGraph
  }

-- | Command-line option parser for 'Q.Query'.
parserSnapshotQuery :: Config n na fla sla
                    -> Opt.Parser (Q.Query n na fla sla)
parserSnapshotQuery conf = fmap fromParsedElement the_parser
  where
    basis = basisSnapshotQuery conf
    fromParsedElement (sf, ti) = basis { Q.startsFrom = sf, Q.timeInterval = ti }
    the_parser = (,) <$> pStartsFrom <*> pTimeInterval
    rNodeID = nodeIDParser conf
    pStartsFrom = many $ Opt.option rNodeID $ mconcat
                  [ Opt.short 's',
                    Opt.long "starts-from",
                    Opt.help "ID of a node from which the Spider starts traversing the history graph. You can specify this option multiple times.",
                    Opt.metavar "NODE-ID"
                  ]
    pTimeInterval = makeInterval <$> pTimeLower <*> pTimeUpper
    pTimeLower = Opt.option (Opt.eitherReader parseTimeIntervalEnd) $ mconcat
                 [ Opt.short 'f',
                   Opt.long "time-from",
                   Opt.help ( "Lower bound of query timestamp. "
                              ++ "Local findings with timestamp newer than this value are used to create the snapshot graph. "
                              ++ "ISO 8601 format is used for timestamps (e.g. `2019-03-22T10:20:12+09:00`). "
                              ++ "The timezone is optional. "
                              ++ "By default, the lower bound is inclusive. "
                              ++ "Add prefix 'x' to make it exclusive (e.g. `x2019-03-22T10:20:12+09:00`). "
                              ++ "Prefix of 'i' explicitly mark the lower bound is inclusive. "
                              ++ "Special value `x-inf` indicates there is no lower bound. " 
                              ++ "Default: x-inf"),
                   Opt.metavar "TIMESTAMP",
                   Opt.value (Q.NegInf, False)
                 ]
    pTimeUpper = Opt.option (Opt.eitherReader parseTimeIntervalEnd) $ mconcat
                 [ Opt.short 't',
                   Opt.long "time-to",
                   Opt.help ( "Upper bound of query timestamp. "
                              ++ "Local findings with timestamp older than this value are used to create the snapshot graph. "
                              ++ "See --time-from for format of timestamps. "
                              ++ "Special value `x+inf` indicates there is no upper bound. " 
                              ++ "Default: x+inf"),
                   Opt.metavar "TIMESTAMP",
                   Opt.value (Q.PosInf, False)
                 ]

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

-- | Command-line option parser for 'SConf.Config' of 'Spider'.
parserSpiderConfig :: Opt.Parser (SConf.Config n na fla)
parserSpiderConfig =
  SConf.Config <$> host <*> port <*> node_id_key <*> log_thresh
  where
    host = undefined
    port = undefined
    node_id_key = undefined
    log_thresh = undefined
