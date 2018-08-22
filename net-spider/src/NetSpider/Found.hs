{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Found
-- Description: Types about local findings
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Found
       ( FoundNode(..),
         FoundLink(..),
         LinkState(..),
         linkStateToText,
         linkStateFromText
       ) where

import Data.Greskell (FromGraphSON(..))
import Data.Text (Text, unpack)

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

linkStateToText :: LinkState -> Text
linkStateToText ls = case ls of
  LinkUnused -> "unused"
  LinkToTarget -> "to_target"
  LinkToSubject -> "to_subject"
  LinkBidirectional -> "bidirectional"

linkStateFromText :: Text -> Maybe LinkState
linkStateFromText t = case t of
  "unused" -> Just LinkUnused
  "to_target" -> Just LinkToTarget
  "to_subject" -> Just LinkToSubject
  "bidirectional" -> Just LinkBidirectional
  _ -> Nothing

instance FromGraphSON LinkState where
  parseGraphSON gv = fromText =<< parseGraphSON gv
    where
      fromText t = case linkStateFromText t of
        Just ls -> return ls
        Nothing -> fail ("Unrecognized LinkState: " ++ unpack t)

-- | A link found for a found node. The link connects from the subject
-- node (the found node) to the target node. The link may be
-- directional or non-directional.
--
-- - type @n@: node ID.
-- - type @la@: link attributes.
data FoundLink n la =
  FoundLink
  { targetNode :: !n,
    linkState :: !LinkState,
    linkAttributes :: !la
  }
  deriving (Show,Eq,Ord)

-- | 'FoundNode' is a node (the subject node) observed at a specific
-- time. It has a set of neighbor links found at the moment.
--
-- - type @n@: node ID.
-- - type @na@: node attributes.
-- - type @la@: link attributes.
data FoundNode n na la =
  FoundNode
  { subjectNode :: !n,
    observationTime :: !Timestamp,
    neighborLinks :: ![FoundLink n la],
    nodeAttributes :: !na
  }
  deriving (Show,Eq)
