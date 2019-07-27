-- |
-- Module: NetSpider.CLI.Snapshot
-- Description: Command to get a snapshot graph
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Snapshot
  ( Config(..),
    parserSnapshotQuery
  ) where

import Control.Applicative ((<$>), (<*>), many)
import qualified NetSpider.Query as Q
import qualified Options.Applicative as Opt
import NetSpider.Interval (interval, parseTimeIntervalEnd)

-- | Configuration for making Snapshot queries.
data Config n na fla sla =
  Config
  { nodeIDReader :: Opt.ReadM n,
    -- ^ Parser that reads an command-line option to generate a node
    -- ID.
    basisSnapshotQuery :: Q.Query n na fla sla,
    -- ^ Basis for queries for SnapshotGraph
    startsFromAsArguments :: Bool
    -- ^ If 'True', the 'Q.startsFrom' field is read from
    -- command-line arguments. If 'False', arguments are not
    -- parsed. Note that the \"-s\" option is always parsed.
  }

-- TODO: how about an option to parse arguments for 'startsFrom'?

-- | Command-line option parser for 'Q.Query'.
parserSnapshotQuery :: Config n na fla sla
                    -> Opt.Parser (Q.Query n na fla sla)
parserSnapshotQuery conf = fmap fromParsedElement the_parser
  where
    basis = basisSnapshotQuery conf
    fromParsedElement (sf, ti) = basis { Q.startsFrom = sf, Q.timeInterval = ti }
    the_parser = (,) <$> ((++) <$> pStartsFrom <*> pStartsFromArgs) <*> pTimeInterval
    rNodeID = nodeIDReader conf
    nodeID_help = "ID of a node from which the Spider starts traversing the history graph. You can specify this option multiple times."
    nodeID_metavar = "NODE-ID"
    pStartsFrom = many $ Opt.option rNodeID $ mconcat
                  [ Opt.short 's',
                    Opt.long "starts-from",
                    Opt.help nodeID_help,
                    Opt.metavar nodeID_metavar
                  ]
    pStartsFromArgs = if not $ startsFromAsArguments conf
                      then pure []
                      else many $ Opt.argument rNodeID $ mconcat
                           [ Opt.help $ nodeID_help,
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
