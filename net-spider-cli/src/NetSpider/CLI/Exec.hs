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

import qualified Data.Text.Lazy.IO as TLIO
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Monoid (mconcat)
import NetSpider.Graph (LinkAttributes, NodeAttributes)
import NetSpider.GraphML.Writer (writeGraphML, ToNodeID, ToAttributes)
import NetSpider.Spider (getSnapshot)
import qualified Options.Applicative as Opt

import NetSpider.CLI.Spider
  ( SpiderConfig, clearDatabase, withSpider
  )
import NetSpider.CLI.Command
  ( Command(..), parserCommand, Config(..)
  )

-- | The main routine controlled by the given 'Config'.
mainBy :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na, ToNodeID n, ToAttributes na, ToAttributes sla)
       => Config n na fla sla
       -> IO ()
mainBy conf = do
  (sconf, cmd) <- Opt.execParser $ Opt.info (parserCommand conf) info_mod
  execCommand conf sconf cmd
  where
    info_mod = mconcat
               [ Opt.fullDesc,
                 Opt.progDesc "Command-line front-end for net-spider"
               ]

-- | Execute the 'Command' obtained from CLI.
execCommand :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na, ToNodeID n, ToAttributes na, ToAttributes sla)
            => Config n na fla sla
            -> SpiderConfig n na fla
            -> Command n na fla sla
            -> IO ()
execCommand _ sconf CmdClean = clearDatabase sconf
execCommand _ sconf (CmdSnapshot query) = withSpider sconf $ \sp -> do
  graph <- getSnapshot sp query
  TLIO.putStrLn $ writeGraphML graph -- TODO: add option to select output format.
execCommand conf sconf CmdInput = return () -- TODO: make it customizable.
