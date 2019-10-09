-- |
-- Module: NetSpider.CLI.Snapshot
-- Description: CLI option parser for Query for snapshot graphs
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- This module defines CLI option parser for 'Q.Query' for snapshot
-- graphs.
module NetSpider.CLI.Snapshot
  ( parserSnapshotQuery,
    makeSnapshotQuery,
    SnapshotConfig(..),
    CLISnapshotQuery
  ) where

import Control.Applicative ((<$>), (<*>), (<|>), many, optional, empty)
import Data.Int (Int64)
import NetSpider.Interval
  ( interval, parseTimeIntervalEnd, secSince, secUntil,
    IntervalEnd, Interval
  )
import qualified NetSpider.Query as Q
import NetSpider.Timestamp (Timestamp)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Types as OptT

-- | Configuration for option parser for Snapshot 'Q.Query'.
--
-- @since 0.2.0.0
data SnapshotConfig n =
  SnapshotConfig
  { nodeIDReader :: Opt.ReadM n,
    -- ^ Parser that reads a CLI option to generate a node ID.
    startsFromAsArguments :: Bool
    -- ^ If 'True', the 'Q.startsFrom' field is read from CLI
    -- arguments. If 'False', arguments are not parsed. In either
    -- case, \"-s\" option is always parsed to generate
    -- 'Q.startsFrom'.
  }

-- | Settings for Snapshot 'Q.Query' parsed from the command-line
-- options. You can make 'Q.Query' by 'makeSnapshotQuery' function.
--
-- @since 0.2.0.0
data CLISnapshotQuery n =
  CLISnapshotQuery
  { startsFrom :: [n],
    timeDurationSec :: Maybe Int64,
    timeFrom :: Maybe (IntervalEnd Timestamp),
    timeTo :: Maybe (IntervalEnd Timestamp)
  }
  deriving (Show,Eq,Ord)

-- | Make a 'Q.Query' by applying 'CLISnapshotQuery' to the base
-- query. The 'CLISnapshotQuery' overwrites 'Q.startsFrom' and
-- 'Q.timeInterval' fields.
--
-- It can fail to convert 'CLISnapshotQuery' to 'Q.Query' fields. In
-- that case, the result is 'Left' with a human-readable error
-- message.
--
-- @since 0.2.0.0
makeSnapshotQuery :: CLISnapshotQuery n
                  -> Q.Query n na fla sla -- ^ base query
                  -> Either String (Q.Query n na fla sla)
                  -- ^ Left: human-readable error message. Right: updated query
makeSnapshotQuery cliq q = do
  ivl <- makeTimeInterval cliq
  return $ q { Q.startsFrom = startsFrom cliq,
               Q.timeInterval = ivl
             }

makeTimeInterval :: CLISnapshotQuery n -> Either String (Interval Timestamp)
makeTimeInterval c =
  case (timeFrom c, timeTo c, timeDurationSec c) of
    (ms, me, Nothing) -> Right $ interval s e
      where
        s = maybe (Q.NegInf, False) id ms
        e = maybe (Q.PosInf, False) id me
    (Just s, Nothing, Just d) -> Right $ secSince d s
    (Nothing, Just e, Just d) -> Right $ secUntil d e
    (Just _, Just _, Just _) -> Left ("Specifying all --time-to, --time-from and --duration is not allowed.")
    (Nothing, Nothing, Just _) -> Left ("Specifying --duration only is not allowed. Specify --time-to or --time-from, too.")

-- | CLI option parser for Snapshot 'Q.Query'. Use 'makeSnapshotQuery'
-- to convert 'CLISnapshotQuery' to 'Q.Query'.
--
-- @since 0.2.0.0
parserSnapshotQuery :: SnapshotConfig n
                    -> Opt.Parser (CLISnapshotQuery n)
parserSnapshotQuery conf =
  CLISnapshotQuery <$> pStartsFromTotal <*> pDuration <*> pTimeLower <*> pTimeUpper
  where
    pStartsFromTotal = (++) <$> pStartsFrom <*> pStartsFromArgs
    rNodeID = nodeIDReader conf
    nodeID_metavar = "NODE-ID"
    pStartsFrom = many $ Opt.option rNodeID $ mconcat
                  [ Opt.short 's',
                    Opt.long "starts-from",
                    Opt.help "ID of a node from which the Spider starts traversing the history graph. You can specify this option multiple times.",
                    Opt.metavar nodeID_metavar
                  ]
    pStartsFromArgs = if not $ startsFromAsArguments conf
                      then pure []
                      else many $ Opt.argument rNodeID $ mconcat
                           [ Opt.help $ "Same as -s option.",
                             Opt.metavar $ nodeID_metavar
                           ]
    pTimeLower =
      optional $ Opt.option (Opt.eitherReader parseTimeIntervalEnd) $ mconcat
      ( [ Opt.short 'f',
          Opt.long "time-from",
          Opt.help ( "Lower bound of query timestamp. "
                     ++ "Local findings with timestamp newer than this value are used to create the snapshot graph. "
                     ++ "ISO 8601 format is used for timestamps (e.g. `2019-03-22T10:20:12+09:00`). "
                     ++ "The timezone is optional. "
                     ++ "By default, the lower bound is inclusive. "
                     ++ "Add prefix 'x' to make it exclusive (e.g. `x2019-03-22T10:20:12+09:00`). "
                     ++ "Prefix of 'i' explicitly mark the lower bound is inclusive. "
                     ++ "Special value `x-inf` indicates there is no lower bound. "
                     ++ "Default: x-inf"
                   ),
          Opt.metavar "TIMESTAMP"
        ]
      )
    pTimeUpper =
      optional $ Opt.option (Opt.eitherReader parseTimeIntervalEnd) $ mconcat
      ( [ Opt.short 't',
          Opt.long "time-to",
          Opt.help ( "Upper bound of query timestamp. "
                     ++ "Local findings with timestamp older than this value are used to create the snapshot graph. "
                     ++ "See --time-from for format of timestamps. "
                     ++ "Special value `x+inf` indicates there is no upper bound. "
                     ++ "Default: x+inf"
                   ),
          Opt.metavar "TIMESTAMP"
        ]
      )
    pDuration = optional $ Opt.option Opt.auto $ mconcat
                [ Opt.short 'd',
                  Opt.long "duration",
                  Opt.help ( "Duration of the query time interval in seconds. "
                             ++ " Use with either --time-to or --time-from option."
                           ),
                  Opt.metavar "SECONDS"
                ]
