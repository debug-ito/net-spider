-- |
-- Module: NetSpider.CLI.Command
-- Description: Command-line parsers
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Command
  ( -- * Top-level parser
    parserCommand,
    execCommand,
    -- * Actions
    actionClear,
    actionGetSnapshot,
    actionSnapshotGraphML,
    Action(..),
    afterAction,
    Command(..),
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy.IO as TLIO
import NetSpider.Graph (LinkAttributes, NodeAttributes)
import NetSpider.GraphML.Writer (writeGraphML, ToNodeID, ToAttributes)
import qualified NetSpider.Query as Q
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Spider (getSnapshot, withSpider, clearAll)
import qualified Options.Applicative as Opt

import NetSpider.CLI.Spider
  ( parserSpiderConfig, SpiderConfig
  )
import qualified NetSpider.CLI.Snapshot as Snapshot

newtype Action n na fla a =
  Action { unAction :: Opt.Parser (SpiderConfig n na fla -> IO a) }

instance Functor (Action n na fla) where
  fmap f (Action a) = Action $ (fmap . fmap . fmap) f a

afterAction :: (a -> IO b) -> Action n na fla a -> Action n na fla b
afterAction then_action (Action a) = Action $ fmap modifyAction a
  where
    modifyAction orig_action sconf = then_action =<< orig_action sconf

actionClear :: Action n na fla ()
actionClear = Action $ pure $ \sconf -> withSpider sconf clearAll

actionGetSnapshot :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na)
                  => Snapshot.Config n na fla sla
                  -> Action n na fla (SnapshotGraph n na sla)
actionGetSnapshot conf = Action $ fmap doGetSnapshot $ Snapshot.parserSnapshotQuery conf
  where
    doGetSnapshot query sconf = withSpider sconf $ \sp -> getSnapshot sp query

actionSnapshotGraphML :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na, ToNodeID n, ToAttributes na, ToAttributes sla)
                      => Snapshot.Config n na fla sla
                      -> Action n na fla ()
actionSnapshotGraphML conf = afterAction printGraphML $ actionGetSnapshot conf
  where
    printGraphML = TLIO.putStrLn . writeGraphML

data Command n na fla =
  Command
  { cmdName :: String,
    cmdDescription :: String,
    cmdAction :: Action n na fla ()
  }

parserCommand :: [Command n na fla] -> Opt.Parser (SpiderConfig n na fla, SpiderConfig n na fla -> IO ())
parserCommand commands = (,) <$> parserSpiderConfig <*> pCommand
  where
    pCommand = Opt.hsubparser $ mconcat $ map toCommandMod commands
    toCommandMod cmd =
      Opt.command (cmdName cmd) $ Opt.info (unAction $ cmdAction cmd) (Opt.progDesc $ cmdDescription cmd)

execCommand :: [Command n na fla] -> IO ()
execCommand commands = exec $ parserCommand commands

exec :: Opt.Parser (a, a -> IO ()) -> IO ()
exec p = do
  (a, act) <- Opt.execParser $ Opt.info p info_mod
  act a
  where
    info_mod = mconcat
               [ Opt.fullDesc,
                 Opt.progDesc "Command-line front-end for net-spider"
               ]
