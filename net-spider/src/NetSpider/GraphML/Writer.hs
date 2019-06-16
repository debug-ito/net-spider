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

import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL

import NetSpider.Snapshot (SnapshotNode, SnapshotLink)

-- | Node ID in GraphML.
type NodeID = Text

class ToNodeID a where
  toNodeID :: a -> NodeID

nodeIDByShow :: Show a => a -> NodeID
nodeIDByShow = pack . show

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

-- TODO: use net-spider-pangraph's ToAttributes to define this
-- ToAttributes.

writeGraphML :: (ToNodeID n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> TL.Text
writeGraphML = undefined
