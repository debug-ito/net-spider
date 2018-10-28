-- |
-- Module: NetSpider.Query
-- Description: Query for snapshot graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Query
       ( -- * Query type
         Query,
         defQuery,
         -- ** accessor functions
         startsFrom,
         unifyLinkSamples,
         timeInterval,
         -- * Utilities
         secUpTo,
         -- * Re-exports
         Interval, (<=..<=), (<..<=), (<=..<), (<..<),
         Extended(..)
       ) where

import Data.ExtendedReal (Extended(..))
import Data.Int (Int64)
import Data.Interval (Interval, (<=..<=), (<..<=), (<=..<), (<..<))
import qualified Data.Interval as Interval

import NetSpider.Timestamp (Timestamp, addSec)
import NetSpider.Unify (LinkSampleUnifier, unifyToOne)

-- | Query for snapshot graph. You can get the default 'Query' by
-- 'defQuery' function, and customize its fields by the accessor
-- functions.
--
-- - Type @n@: node ID
-- - Type @na@: node attributes
-- - Type @fla@: attributes of found links.
-- - Type @sla@: attributes of snapshot links. Converted from @fla@ by
--   'unifyLinkSamples'.
data Query n na fla sla =
  Query
  { startsFrom :: [n],
    -- ^ Nodes from which the Spider starts traversing the history
    -- graph.
    unifyLinkSamples :: LinkSampleUnifier n na fla sla,
    -- ^ See the document of 'LinkSampleUnifier'.
    --
    -- Default: 'unifyToOne'.
    timeInterval :: Interval Timestamp
    -- ^ Time interval to query. Only the local findings observed
    -- during this interval are used to make the snapshot graph.
    --
    -- Default: (-infinity, +infinity)
  }

-- | The default 'Query'.
defQuery :: Eq n
         => [n] -- ^ 'startsFrom' field.
         -> Query n na fla fla
defQuery ns = Query
              { startsFrom = ns,
                unifyLinkSamples = unifyToOne,
                timeInterval = Interval.whole
              }

-- | @s `secUpTo` ts@ returns the time interval of length @s@ (in
-- seconds) and up to @ts@. The interval is inclusive for both ends.
secUpTo :: Int64 -> Timestamp -> Interval Timestamp
secUpTo len end = Finite start <=..<= Finite end
  where
    start = addSec (-len) end
