-- |
-- Module: NetSpider.Neighbors
-- Description: Neighbors type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Neighbors
       ( Neighbors(..),
         FoundLink(..),
         LinkState(..)
       ) where

import Data.Vector (Vector)
import NetSpider.Timestamp (Timestamp)

-- | State of the found link.
data LinkState =
    LinkUnused
    -- ^ Link is possible, but not used.
  | LinkToTarget
    -- ^ Link is directional. It's from subject to target.
  | LinkToSubject
    -- ^ Link is directional. It's from target to subject.
  | LinkBidirectional
    -- ^ Link is bidirectional.
  deriving (Show,Eq,Ord,Bounded,Enum)

-- | A link found in neighbors. The link connects a port in the
-- subject node with a port in the target node. The link may be
-- directional or non-directional.
--
-- - type @n@: node ID.
-- - type @@@: port ID.
data FoundLink n p =
  FoundLink
  { subjectPort :: !p,
    targetNode :: !n,
    targetPort :: !p,
    linkState :: !LinkState
  }
  deriving (Show,Eq,Ord)

-- | 'Neighbors' is a set of neighbor links connected to a specific
-- node (the subject node) observed at a specific time.
--
-- - type @n@: node ID.
-- - type @p@: port ID.
data Neighbors n p =
  Neighbors
  { subjectNode :: !n,
    observedTime :: !Timestamp,
    neighborLinks :: !(Vector (FoundLink n p))
  }
  deriving (Show,Eq)
