-- |
-- Module: NetSpider.CLI
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.CLI
       ( parserSnapshotQuery,
         Config,
         nodeIDParser,
         parseTimeIntervalEnd,
         IntervalEnd
       ) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Monoid (mconcat)
import qualified NetSpider.Query as Q
import NetSpider.Timestamp (Timestamp, parseTimestamp)
import qualified Options.Applicative as Opt

-- | Configuration for parsers.
data Config n na sla =
  Config
  { nodeIDParser :: Opt.ReadM n
  }

-- | Command-line parser for 'Q.Query'.
parserSnapshotQuery :: Config n na sla
                    -> Q.Query n na fla sla -- ^ Basis for the query.
                    -> Opt.Parser (Q.Query n na fla sla)
parserSnapshotQuery conf basis = fmap fromParsedElement the_parser
  where
    fromParsedElement (sf, m_ti) =
      let q = basis { Q.startsFrom = sf }
      in maybe q (\ti -> q { Q.timeInterval = ti } ) m_ti
    the_parser = (,) <$> pStartsFrom <*> pTimeInterval
    rNodeID = nodeIDParser conf
    pStartsFrom = many $ Opt.option rNodeID $ mconcat
                  [ Opt.short 's',
                    Opt.long "starts-from",
                    Opt.help "ID of a node from which the Spider starts traversing the history graph. You can specify this option multiple times.",
                    Opt.metavar "NODE-ID"
                  ]
    pTimeInterval = undefined -- TODO

type ErrorMsg = String

-- | Upper or lower end of 'Q.Interval'. The 'Bool' field is 'True'
-- if the end is inclusive.
type IntervalEnd a = (Q.Extended a, Bool)

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

