-- |
-- Module: NetSpider.Neighbors
-- Description: Objects related to neighbors
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Neighbors
       ( Neighbors(..),
         Timestamp(..),
         FoundLink(..),
         LinkDirection(..)
       ) where


import Data.UnixTime (UnixTime)
import Data.Time.LocalTime (TimeZone)

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

-- | Timestamp when the snapshot is observed.
data Timestamp =
  Timestamp
  { unixTime :: UnixTime,
    timeZone :: Maybe TimeZone
  }
  deriving (Show,Eq) -- UnixTime's Ord is dangerous.

-- | 'Neighbors' is a set of neighbor links connected to a specific
-- node (the subject node) observed at a specific time.
--
-- - type @n@: node ID.
-- - type @p@: port ID.
data Neighbors n p =
  Neighbors
  { subjectNode :: n,
    observedTime :: Timestamp,
    neighborLinks :: [FoundLink n p]
  }
  deriving (Show,Eq)
