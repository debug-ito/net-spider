{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: NetSpider.Found
-- Description: Types about local findings
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.Found
       ( -- * Local findings
         FoundNode(..),
         FoundLink(..),
         -- * LinkState
         LinkState(..),
         linkStateToText,
         linkStateFromText,
         -- * Utilities
         sortByTime,
         allTargetNodes
       ) where

import qualified Control.Monad.Fail as Fail
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isUpper, toLower)
import Data.Greskell (FromGraphSON(..))
import Data.List (sortOn, reverse)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import qualified Text.Regex.Applicative as RE

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

linkStateFromTextF :: Fail.MonadFail m => Text -> m LinkState
linkStateFromTextF t = 
  case linkStateFromText t of
    Just ls -> return ls
    Nothing -> Fail.fail ("Unrecognized LinkState: " ++ unpack t)

instance FromGraphSON LinkState where
  parseGraphSON gv = linkStateFromTextF =<< parseGraphSON gv

-- | Parse a JSON string to 'LinkState'.
--
-- @since 0.4.1.0
instance FromJSON LinkState where
  parseJSON v = linkStateFromTextF =<< parseJSON v

-- | Convert 'LinkState' to a JSON string.
--
-- @since 0.4.1.0
instance ToJSON LinkState where
  toJSON = toJSON . linkStateToText

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions
           { Aeson.fieldLabelModifier = modifier
           }
  where
    modifier = RE.replace reSnake . RE.replace reAttr
    reAttr = fmap (const "Attrs") $ RE.string "Attributes"
    reSnake = RE.msym $ \c -> if isUpper c then Just ['_', toLower c] else Nothing


-- | A link found at a 'FoundNode'. The link connects from the subject
-- node (the found node) to the target node. The link may be
-- directional or non-directional.
--
-- - type @n@: node ID.
-- - type @la@: link attributes.
data FoundLink n la =
  FoundLink
  { targetNode :: n,
    linkState :: LinkState,
    linkAttributes :: la
  }
  deriving (Show,Eq,Ord,Generic)

-- | @since 0.3.0.0
instance Functor (FoundLink n) where
  fmap f l = l { linkAttributes = f $ linkAttributes l }

-- | @since 0.3.0.0
instance Bifunctor FoundLink where
  bimap fn fla l = l { targetNode = fn $ targetNode l,
                       linkAttributes = fla $ linkAttributes l
                     }

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON la) => FromJSON (FoundLink n la) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON la) => ToJSON (FoundLink n la) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

-- | 'FoundNode' is a node (the subject node) observed at a specific
-- time. It has a set of neighbor links found at the moment.
--
-- - type @n@: node ID.
-- - type @na@: node attributes.
-- - type @la@: link attributes.
--
-- 'Ord' instance is added in net-spider-0.4.1.0.
data FoundNode n na la =
  FoundNode
  { subjectNode :: n,
    foundAt :: Timestamp,
    neighborLinks :: [FoundLink n la],
    nodeAttributes :: na
  }
  deriving (Show,Eq,Ord,Generic)

-- | @since 0.3.0.0
instance Functor (FoundNode n na) where
  fmap f n = n { neighborLinks = (fmap . fmap) f $ neighborLinks n }

-- | @since 0.3.0.0
instance Bifunctor (FoundNode n) where
  bimap fna fla n = n { neighborLinks = (fmap . fmap) fla $ neighborLinks n,
                        nodeAttributes = fna $ nodeAttributes n
                      }

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON na, FromJSON la) => FromJSON (FoundNode n na la) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON na, ToJSON la) => ToJSON (FoundNode n na la) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

-- | Sort the list of 'FoundNode's by descending order of their
-- timestamps. The latest 'FoundNode' is at the top.
--
-- @since 0.4.2.0
sortByTime :: [FoundNode n na la] -> [FoundNode n na la]
sortByTime fns = reverse $ sortOn foundAt fns

-- | Get all 'targetNode's of the 'FoundNode'.
--
-- @since 0.4.2.0
allTargetNodes :: FoundNode n na la -> [n]
allTargetNodes = map targetNode . neighborLinks
