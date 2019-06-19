{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.GraphML.Writer
-- Description: Serialize a Snapshot graph into GraphML format.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- http://graphml.graphdrawing.org/primer/graphml-primer.html
module NetSpider.GraphML.Writer
  ( -- * Functions
    writeGraphML,
    -- * NodeID
    NodeID,
    ToNodeID(..),
    nodeIDByShow,
    -- * Attributes
    AttributeKey,
    AttributeValue(..),
    ToAttributes(..)
  ) where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL

import NetSpider.Snapshot
  ( SnapshotNode, nodeId,
    SnapshotLink, sourceNode, destinationNode
  )

-- | Node ID in GraphML.
type NodeID = Text

class ToNodeID a where
  toNodeID :: a -> NodeID

nodeIDByShow :: Show a => a -> NodeID
nodeIDByShow = pack . show

instance ToNodeID Text where
  toNodeID = id
  
-- TODO: use ToAtom to define ToNodeID.

-- | Key of attribute.
type AttributeKey = Text

-- | Typed value of attribute.
data AttributeValue = AttrBoolean Bool
                    | AttrInt Int
                    | AttrLong Integer
                    | AttrFloat Float
                    | AttrDouble Double
                    | AttrString Text
                    deriving (Show,Eq,Ord)

-- | Type that can be converted to list of attributes.
class ToAttributes a where
  toAttributes :: a -> [(AttributeKey, AttributeValue)]

instance ToAttributes () where
  toAttributes _ = []

-- TODO: use net-spider-pangraph's ToAttributes to define this
-- ToAttributes.

writeGraphML :: (ToNodeID n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> TL.Text
writeGraphML input_nodes input_links =
  xml_header
  <> graphml_header
  <> keys
  <> graph_header
  <> nodes
  <> edges
  <> graph_footer
  <> graphml_footer
  where
    xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    graphml_header = "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
                     <> " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
                     <> " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
    graphml_footer = "</graphml>\n"
    keys = "" -- TODO
    graph_header = "  <graph edgedefault=\"undirected\">\n"
    graph_footer = "  </graph>\n"
    nodes = mconcat $ map writeNode input_nodes
    edges = mconcat $ map writeLink input_links
    writeNode n = "  <node id=\"" <> (encodeNodeID $ nodeId n) <> "\">\n"
                  <> "  </node>\n"
    writeLink l = "<edge source=\"" <> (encodeNodeID $ sourceNode l)
                  <> "\" target=\"" <> (encodeNodeID $ destinationNode l) <> "\">\n"
                  <> "</edge>\n"

encodeNodeID :: ToNodeID n => n -> TL.Text
encodeNodeID = TL.fromStrict . toNodeID -- TODO
