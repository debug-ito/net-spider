-- |
-- Module: NetSpider.SeqID
-- Description: Sequential node IDs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- @since 0.4.3.0
module NetSpider.SeqID
  ( -- * Type
    SeqIDMaker,
    -- * Construction
    newSeqIDMaker,
    -- * Conversion
    convertGraph,
    convertLink,
    convertNode,
    convertNodeID,
    originalIDFor
  ) where

import Data.Foldable (foldl')
import Data.List (reverse)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import NetSpider.Snapshot.Internal
  ( SnapshotNode(_nodeId),
    SnapshotLink(_sourceNode, _destinationNode),
    SnapshotGraph
  )


-- | 'SeqIDMaker' converts node ID type @n@ into the new node ID type
-- @i@. The type @i@ is supposed to be an 'Enum', and it generates the
-- node ID of type @i@ sequentially for each node ID of type @n@.
--
-- 'SeqIDMaker' is useful to convert the 'SnapshotGraph' into a graph
-- representation of another graph library such as
-- [fgl](https://hackage.haskell.org/package/fgl). Note that the
-- target graph library can provide a better way for conversion. For
-- example, fgl has @NodeMap@ type to do basically the same thing.
data SeqIDMaker n i =
  SeqIDMaker
  { toSeqID :: HashMap n i,
    fromSeqID :: HashMap i n,
    nextID :: i
  }
  deriving (Show,Eq)

-- | Make a 'SeqIDMaker' with the given initial ID.
newSeqIDMaker :: i -- ^ Initial ID
              -> SeqIDMaker n i
newSeqIDMaker init_id = SeqIDMaker HM.empty HM.empty init_id

-- | Convert the given node ID of type @n@ into the sequential ID of
-- type @i@.
convertNodeID :: (Eq n, Hashable n, Enum i, Eq i, Hashable i)
              => SeqIDMaker n i
              -> n -- ^ Old ID
              -> (SeqIDMaker n i, i) -- ^ Updated 'SeqIDMaker' and the new ID.
convertNodeID maker nid =
  case HM.lookup nid $ toSeqID maker of
    Just existing_id -> (maker, existing_id)
    Nothing -> (new_maker, new_id)
  where
    new_id = nextID maker
    new_next_id = succ $ new_id
    new_maker = maker { toSeqID = new_to,
                        fromSeqID = new_from,
                        nextID = new_next_id
                      }
    new_to = HM.insert nid new_id $ toSeqID maker
    new_from = HM.insert new_id nid $ fromSeqID maker
    
-- | Convert node IDs in the 'SnapshotGraph'.
convertGraph :: (Eq n, Hashable n, Enum i, Eq i, Hashable i)
             => SeqIDMaker n i -> SnapshotGraph n na la -> (SeqIDMaker n i, SnapshotGraph i na la)
convertGraph maker (nodes, links) = (new_maker, (new_nodes, new_links))
  where
    (inter_maker, new_nodes_rev) = foldl' nodeFolder (maker, []) nodes
    nodeFolder (m, ns) node = let (step_m, new_node) = convertNode m node
                              in (step_m, new_node : ns)
    new_nodes = reverse new_nodes_rev
    (new_maker, new_links_rev) = foldl' linkFolder (inter_maker, []) links
    linkFolder (m, ls) link = let (step_m, new_link) = convertLink m link
                              in (step_m, new_link : ls)
    new_links = reverse new_links_rev

-- | Convert node IDs in the 'SnapshotLink'.
convertLink :: (Eq n, Hashable n, Enum i, Eq i, Hashable i)
            => SeqIDMaker n i -> SnapshotLink n la -> (SeqIDMaker n i, SnapshotLink i la)
convertLink maker link = (new_maker, new_link)
  where
    (inter_maker, seq_source) = convertNodeID maker $ _sourceNode link
    (new_maker, seq_dest) = convertNodeID inter_maker $ _destinationNode link
    new_link = link { _sourceNode = seq_source, _destinationNode = seq_dest }

-- | Convert node ID in the 'SnapshotNode'.
convertNode :: (Eq n, Hashable n, Enum i, Eq i, Hashable i)
            => SeqIDMaker n i -> SnapshotNode n na -> (SeqIDMaker n i, SnapshotNode i na)
convertNode maker node = (new_maker, new_node)
  where
    (new_maker, seq_id) = convertNodeID maker $ _nodeId node
    new_node = node { _nodeId = seq_id }

-- | Get the original ID of type @n@ for the new ID of type @i@.
originalIDFor :: (Eq i, Hashable i)
              => SeqIDMaker n i -> i -> Maybe n
originalIDFor maker seq_id = HM.lookup seq_id $ fromSeqID maker
