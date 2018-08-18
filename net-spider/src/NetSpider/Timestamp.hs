-- |
-- Module: NetSpider.Timestamp
-- Description: Timestamp type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Timestamp
       ( Timestamp(..),
         fromEpochSecond
       ) where

import Data.Int (Int64)
import Data.Time.LocalTime (TimeZone)

-- | Timestamp when graph elements are observed.
data Timestamp =
  Timestamp
  { epochTime :: !Int64,
    timeZone :: !(Maybe TimeZone)
  }
  deriving (Show,Eq)

-- | Compare by 'epochTime' only. 'timeZone' is not used.
instance Ord Timestamp where
  compare l r = compare (epochTime l) (epochTime r)

-- | Make 'Timestamp' from seconds from the epoch. 'timeZone' is
-- 'Nothing'.
fromEpochSecond :: Int64 -> Timestamp
fromEpochSecond sec = Timestamp sec Nothing
