-- |
-- Module: NetSpider.CLI.Snapshot
-- Description: CLI option parser for Query for snapshot graphs
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- This module defines CLI option parser for 'Q.Query' for snapshot
-- graphs.
module NetSpider.CLI.Snapshot
  ( parserSnapshotQuery,
    SnapshotConfig(..)
  ) where

import Control.Applicative ((<$>), (<*>), many)
import qualified NetSpider.Query as Q
import qualified Options.Applicative as Opt
import NetSpider.Interval (interval, parseTimeIntervalEnd)

-- | Configuration for option parser for Snapshot 'Q.Query'.
data SnapshotConfig n na fla sla =
  SnapshotConfig
  { nodeIDReader :: Opt.ReadM n,
    -- ^ Parser that reads a CLI option to generate a node ID.
    basisSnapshotQuery :: Q.Query n na fla sla,
    -- ^ Basis for queries for snapshot graphs. Fields in this basis
    -- are overwritten by CLI options.
    startsFromAsArguments :: Bool
    -- ^ If 'True', the 'Q.startsFrom' field is read from CLI
    -- arguments. If 'False', arguments are not parsed. In either
    -- case, \"-s\" option is always parsed to generate
    -- 'Q.startsFrom'.
  }

-- | CLI option parser for 'Q.Query'.
parserSnapshotQuery :: SnapshotConfig n na fla sla
                    -> Opt.Parser (Q.Query n na fla sla)
parserSnapshotQuery conf = fmap fromParsedElement the_parser
  where
    basis = basisSnapshotQuery conf
    fromParsedElement (sf, ti) = basis { Q.startsFrom = sf, Q.timeInterval = ti }
    the_parser = (,) <$> ((++) <$> pStartsFrom <*> pStartsFromArgs) <*> pTimeInterval
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
    pTimeInterval = interval <$> pTimeLower <*> pTimeUpper
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
