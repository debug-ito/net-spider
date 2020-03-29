-- |
-- Module: NetSpider.SeqID
-- Description: Sequential node IDs
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
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

import NetSpider.Snapshot (SnapshotNode, SnapshotLink, SnapshotGraph)

-- | 'SeqIDMaker' converts node ID type @n@ into the new node ID type
-- @i@. The type @i@ is supposed to be an 'Enum', and it generates the
-- node ID of type @i@ sequentially for each node ID of type @n@.
data SeqIDMaker n i = SeqIDMaker
                    deriving (Show,Eq)

-- | Make a 'SeqIDMaker' with the given initial ID.
newSeqIDMaker :: i -- ^ Initial ID
              -> SeqIDMaker n i
newSeqIDMaker = undefined

-- | Convert the given node ID of type @n@ into the sequential ID of
-- type @i@.
convertNodeID :: SeqIDMaker n i
              -> n -- ^ Old ID
              -> (SeqIDMaker n i, i) -- ^ Updated 'SeqIDMaker' and the new ID.
convertNodeID = undefined

-- | Convert node IDs in the 'SnapshotGraph'.
convertGraph :: SeqIDMaker n i -> SnapshotGraph n na la -> (SeqIDMaker n i, SnapshotGraph i na la)
convertGraph = undefined

-- | Convert node IDs in the 'SnapshotLink'.
convertLink :: SeqIDMaker n i -> SnapshotLink n la -> (SeqIDMaker n i, SnapshotLink i la)
convertLink = undefined

-- | Convert node ID in the 'SnapshotNode'.
convertNode :: SeqIDMaker n i -> SnapshotNode n na -> (SeqIDMaker n i, SnapshotNode i na)
convertNode = undefined

-- | Get the original ID of type @n@ for the new ID of type @i@.
originalIDFor :: SeqIDMaker n i -> i -> Maybe n
originalIDFor = undefined
