{-# LANGUAGE OverloadedStrings, StrictData #-}
-- |
-- Module: NetSpider.CLI.Spider
-- Description: Types and routines about Spider
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Spider
  ( -- * Parsers
    SpiderConfig,
    parserSpiderConfig
  ) where

import Control.Applicative ((<$>), (<*>), many, pure)
import Data.Monoid (mconcat)
import qualified NetSpider.Spider.Config as SConf
import qualified NetSpider.Spider as Sp
import qualified Options.Applicative as Opt

-- | 'SConf.Config' of the Spider.
type SpiderConfig = SConf.Config

-- | Command-line option parser for 'SConf.Config' of 'Spider'.
parserSpiderConfig :: Opt.Parser (SpiderConfig n na fla)
parserSpiderConfig =
  SConf.Config <$> host <*> port <*> node_id_key <*> log_thresh
  where
    host = Opt.strOption $ mconcat
           [ Opt.long "host",
             Opt.help "Hostname or address of Gremlin Server",
             Opt.metavar "HOSTNAME",
             Opt.value "localhost",
             Opt.showDefault
           ]
    port = Opt.option Opt.auto $ mconcat
           [ Opt.long "port",
             Opt.help "Port number of Gremlin Server WebSocket endpoint",
             Opt.metavar "PORT",
             Opt.value 8182,
             Opt.showDefault
           ]
    node_id_key = Opt.strOption $ mconcat
                  [ Opt.long "node-id-key",
                    Opt.help "Name of vertex attriute that stores Node ID.",
                    Opt.metavar "KEY",
                    Opt.value "@node_id",
                    Opt.showDefault
                  ]
    log_thresh = fmap (logLevelFromVerbosity . length) $ many $ Opt.flag' () $ mconcat
                 [ Opt.short 'v',
                   Opt.long "verbose",
                   Opt.help "Verbose log output. Specify multiple times to make it more verbose."
                 ]

logLevelFromVerbosity :: Int -> SConf.LogLevel
logLevelFromVerbosity 2 = SConf.LevelDebug
logLevelFromVerbosity 1 = SConf.LevelInfo
logLevelFromVerbosity _ = SConf.LevelWarn
