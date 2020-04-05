{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Graph.Inductive (LNode, LEdge, Gr)
import qualified Data.Graph.Inductive as FGL
import Data.List (sortOn, reverse)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import NetSpider.Log (WriterLoggingM, logErrorW, spack)
import NetSpider.SeqID
  (SeqIDMaker, newSeqIDMaker, convertGraph, originalIDFor)
import NetSpider.Snapshot
  ( SnapshotNode, SnapshotLink, SnapshotGraph,
    nodeId, nodeAttributes, sourceNode, destinationNode, linkAttributes,
    graphTimestamp
  )
import NetSpider.Timestamp (Timestamp)
import NetSpider.RPL.DIO (SnapshotGraphDIO)
import NetSpider.RPL.DAO (SnapshotGraphDAO)
import NetSpider.RPL.FindingID
  ( IPv6ID, FindingID, FindingType(..), ipv6Only)


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
analyzeDIO = analyzeGeneric RootDest FindingDIO

-- | Get analysis on a DAO graph.
analyzeDAO :: SnapshotGraphDAO -> WriterLoggingM (Maybe DODAGAttributes)
analyzeDAO = analyzeGeneric RootSource FindingDAO

analyzeGeneric :: RootType -> FindingType -> SnapshotGraph FindingID na la -> WriterLoggingM (Maybe DODAGAttributes)
analyzeGeneric rtype ftype graph = runMaybeT $ go
  where
    maybeLog m err_log =
      case m of
        Nothing -> do
          lift $ logErrorW err_log
          empty
        Just v -> return v
    (seqid, gr) = toGr graph
    ft_str =
      case ftype of
        FindingDIO -> "DIO"
        FindingDAO -> "DAO"
    go = do
      root_node <- fmap fst $ maybeLog (getRoot rtype gr) ("The " <> ft_str <> " graph doesn't have the root.")
      -- TODO: print debug log about the root
      graph_ts <- maybeLog (graphTimestamp graph) ("The graph has no timestamp.")
      -- TODO: print debug log about the timestamp
      root_fid <- maybeLog (originalIDFor seqid root_node) ("Cannot find the FindingID for root node " <> spack root_node <> ".")
      return DODAGAttributes { node_num = nodeNum gr,
                               edge_num = edgeNum gr,
                               depth = getDepth root_node rtype gr,
                               root = ipv6Only root_fid,
                               time = graph_ts
                             }

toLNode :: SnapshotNode Int na -> LNode (Maybe na)
toLNode n = (nodeId n, nodeAttributes n)

toLEdge :: SnapshotLink Int la -> LEdge la
toLEdge l = (sourceNode l, destinationNode l, linkAttributes l)

toGr :: SnapshotGraph FindingID na la -> (SeqIDMaker FindingID FGL.Node, Gr (Maybe na) la)
toGr graph = (got_maker, FGL.mkGraph lnodes ledges)
  where
    (got_maker, (new_nodes, new_links)) = convertGraph (newSeqIDMaker 0) graph
    lnodes = map toLNode new_nodes
    ledges = map toLEdge new_links

nodeNum :: Gr na la -> Int
nodeNum = FGL.order

edgeNum :: Gr na la -> Int
edgeNum = FGL.size

data RootType = RootSource -- ^ Node with no incoming edges.
              | RootDest -- ^ Node with no outgoing edges.

getRoot :: RootType -> Gr na la -> Maybe (LNode na)
getRoot rt gr = listToMaybe
                $ reverse
                $ sortOn (\(n, _) -> childNum n)
                $ filter (\(n, _) -> parentNum n == 0)
                $ FGL.labNodes gr
  where
    (parentNum, childNum) =
      case rt of
        RootSource -> (FGL.indeg gr, FGL.outdeg gr)
        RootDest   -> (FGL.outdeg gr, FGL.indeg gr)

getDepth :: FGL.Node -- ^ the root node
         -> RootType -- ^ type of the root
         -> Gr na la
         -> Int
getDepth root_node rtype gr = maximum' $ map toPathLen $ FGL.spTree root_node $ convertGr gr
  where
    convertGr = FGL.gmap setEdgeDir . FGL.emap setEdgeLabel
      where
        setEdgeLabel _ = (1 :: Int)
        setEdgeDir orig@(inedges, n, nlabel, outedges) =
          case rtype of
            RootSource -> orig
            RootDest -> (outedges, n, nlabel, inedges)
    toPathLen (FGL.LP nodes) = length nodes - 1
    maximum' [] = 0
    maximum' l = maximum l

