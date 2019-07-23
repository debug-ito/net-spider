-- |
-- Module: NetSpider.CLI.Exec
-- Description: Execution of a command
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Exec
  ( mainBy,
    execCommand
  ) where

import Data.Monoid (mconcat)
import qualified Options.Applicative as Opt

import NetSpider.CLI.Spider
  ( SpiderConfig, clearDatabase
  )
import NetSpider.CLI.Command
  ( Command(..), parserCommand, Config
  )

-- | The main routine controlled by the given 'Config'.
mainBy :: Config n na fla sla i -> IO ()
mainBy conf = do
  (sconf, cmd) <- Opt.execParser $ Opt.info (parserCommand conf) info_mod
  execCommand sconf cmd
  where
    info_mod = mconcat
               [ Opt.fullDesc,
                 Opt.progDesc "Command-line front-end for net-spider"
               ]

-- | Execute the 'Command' obtained from CLI.
execCommand :: SpiderConfig n na fla -> Command n na fla sla i -> IO ()
execCommand conf CmdClean = clearDatabase conf
execCommand conf (CmdSnapshot query) = undefined -- TODO
execCommand conf (CmdInput input) = undefined -- TODO
