-- |
-- Module: NetSpider.RPL.CLI.Analyze
-- Description: Analyze graph attributes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- @since 0.1.3.0
module NetSpider.RPL.CLI.Analyze
  ( DODAGAttributes(..),
    analyzeDIO,
    analyzeDAO
  ) where

import NetSpider.Log (WriterLoggingM)
import NetSpider.Timestamp (Timestamp)
import NetSpider.RPL.DIO (SnapshotGraphDIO)
import NetSpider.RPL.DAO (SnapshotGraphDAO)
import NetSpider.RPL.FindingID (IPv6ID)


-- | Attributes of a DODAG.
data DODAGAttributes =
  DODAGAttributes
  { node_num :: Int,
    edge_num :: Int,
    depth :: Int,
    root :: IPv6ID,
    time :: Timestamp
  }
  deriving (Show,Eq,Ord)

-- | Get analysis on a DIO graph.
analyzeDIO :: SnapshotGraphDIO -> WriterLoggingM (Maybe DODAGAttributes)
analyzeDIO = undefined

-- | Get analysis on a DAO graph.
analyzeDAO :: SnapshotGraphDAO -> WriterLoggingM (Maybe DODAGAttributes)
analyzeDAO = undefined
