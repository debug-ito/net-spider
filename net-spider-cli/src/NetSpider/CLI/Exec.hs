-- |
-- Module: NetSpider.CLI.Exec
-- Description: Execution of a command
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.Exec
  ( mainBy
  ) where

import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Greskell.GraphSON (FromGraphSON)
import NetSpider.Graph (LinkAttributes, NodeAttributes)
import NetSpider.GraphML.Writer (ToNodeID, ToAttributes)

import NetSpider.CLI.Command
  ( Command(..), execCommand,
    actionClear, actionSnapshotGraphML
  )
import qualified NetSpider.CLI.Snapshot as Snapshot

-- | The main routine controlled by the given 'Config'.
mainBy :: (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n, LinkAttributes fla, NodeAttributes na, ToNodeID n, ToAttributes na, ToAttributes sla)
       => Snapshot.Config n na fla sla
       -> IO ()
mainBy conf = execCommand commands
  where
    commands = [ Command "clear" "Clear the database." actionClear,
                 Command "snapshot" "Get a snapshot graph." $ actionSnapshotGraphML conf
               ]
