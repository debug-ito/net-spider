-- |
-- Module: NetSpider.Query
-- Description: Query for snapshot graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Query
       ( -- * Query
         Query,
         defQuery,
         -- ** accessor functions
         startsFrom,
         unifyLinkSamples,
         timeInterval,
         foundNodePolicy,
         -- * FoundNodePolicy
         FoundNodePolicy,
         policyOverwrite,
         policyAppend,
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
import NetSpider.Query.Internal (FoundNodePolicy(..))

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
    timeInterval :: Interval Timestamp,
    -- ^ Time interval to query. Only the local findings observed
    -- during this interval are used to make the snapshot graph.
    --
    -- Default: (-infinity, +infinity)
    foundNodePolicy :: FoundNodePolicy n na
    -- ^ Policy to treat 'FoundNode's (local findings).
    --
    -- Default: 'policyOverwrite'
  }

-- | The default 'Query'.
defQuery :: Eq n
         => [n] -- ^ 'startsFrom' field.
         -> Query n na fla fla
defQuery ns = Query
              { startsFrom = ns,
                unifyLinkSamples = unifyToOne,
                timeInterval = Interval.whole,
                foundNodePolicy = policyOverwrite
              }

-- | @s `secUpTo` ts@ returns the time interval of length @s@ (in
-- seconds) and up to @ts@. The interval is inclusive for both ends.
secUpTo :: Int64 -> Timestamp -> Interval Timestamp
secUpTo len end = Finite start <=..<= Finite end
  where
    start = addSec (-len) end

-- | A 'FoundNode' always overwrites old 'FoundNode's, so only the
-- latest one is valid.
--
-- This policy is appropriate when you can always get the complete
-- neighbor information of a given node at once.
policyOverwrite :: FoundNodePolicy n na
policyOverwrite = PolicyOverwrite

-- | A 'FoundNode' appends neighbor information to old
-- 'FoundNode's. When the spider makes the snapshot graph, it
-- aggregates all 'FoundNode's in the query range.
--
-- This policy is appropriate when you can only get part of neighbor
-- information of a given node at once.
policyAppend :: FoundNodePolicy n na
policyAppend = PolicyAppend
