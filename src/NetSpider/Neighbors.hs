-- |
-- Module: NetSpider.Neighbors
-- Description: Neighbors type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Neighbors
       ( Neighbors(..),
         FoundLink(..),
         LinkDirection(..)
       ) where

import Data.Vector (Vector)
import NetSpider.Timestamp (Timestamp)

-- | Direction of a link found in neighbors.
data LinkDirection =
    SubjectToTarget
  | TargetToSubject
  deriving (Show,Eq,Ord,Bounded,Enum)

-- | A link found in neighbors. The link connects a port in the
-- subject node with a port in the target node. The link may be
-- directional or non-directional.
--
-- - type @n@: node ID.
-- - type @@@: port ID.
data FoundLink n p =
  FoundLink
  { subjectPort :: p,
    targetNode :: n,
    targetPort :: p,
    linkDirection :: Maybe LinkDirection
  }
  deriving (Show,Eq,Ord)

-- | 'Neighbors' is a set of neighbor links connected to a specific
-- node (the subject node) observed at a specific time.
--
-- - type @n@: node ID.
-- - type @p@: port ID.
data Neighbors n p =
  Neighbors
  { subjectNode :: n,
    observedTime :: Timestamp,
    neighborLinks :: Vector (FoundLink n p)
  }
  deriving (Show,Eq)
