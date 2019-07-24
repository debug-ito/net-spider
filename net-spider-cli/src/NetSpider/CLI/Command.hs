-- |
-- Module: NetSpider.CLI.Command
-- Description: Command-line parsers
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Command
  ( -- * Top-level parser
    parserCommand,
    Config(..),
    Command(..),
    -- * \"snapshot\" query
    parserSnapshotQuery,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid (mconcat)
import qualified NetSpider.Query as Q
import qualified Options.Applicative as Opt

import NetSpider.CLI.Snapshot (Config, parserSnapshotQuery)
import NetSpider.CLI.Spider (parserSpiderConfig, SpiderConfig)

-- | Subcommand specified in CLI.
data Command n na fla sla =
    CmdClean
  | CmdSnapshot (Q.Query n na fla sla)
  | CmdInput

-- | Top-level command-line parser.
parserCommand :: Config n na fla sla -> Opt.Parser (SpiderConfig n na fla, Command n na fla sla)
parserCommand conf = (,) <$> parserSpiderConfig <*> pCommand
  where
    pCommand = Opt.hsubparser $ mconcat
               [ Opt.command "clean"
                 $ Opt.info (pure CmdClean) (Opt.progDesc "Clean all the content in the database."),
                 Opt.command "snapshot"
                 $ Opt.info (fmap CmdSnapshot $ parserSnapshotQuery conf) (Opt.progDesc "Get a SnapshotGraph from the database."),
                 Opt.command "input"
                 $ Opt.info (pure CmdInput) (Opt.progDesc "Input local findings to the database.")
               ]
