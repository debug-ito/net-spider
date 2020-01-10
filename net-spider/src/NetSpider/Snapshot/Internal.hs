{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: NetSpider.Snapshot.Internal
-- Description: Implementation of Snapshot graph types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use this.__
--
-- Implementation of Snapshot graph types. This module is for internal
-- and testing purposes only.
--
-- @since 0.3.0.0
module NetSpider.Snapshot.Internal
       ( SnapshotGraph,
         SnapshotLink(..),
         linkNodeTuple,
         linkNodePair,
         SnapshotNode(..)
       ) where

import Control.Applicative (many, (*>))
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor(..))
import Data.Char (isUpper, toLower)
import GHC.Generics (Generic)
import NetSpider.Pair (Pair(..))
import NetSpider.Timestamp (Timestamp)
import qualified Text.Regex.Applicative as RE

-- | The snapshot graph, which is a collection nodes and links.
--
-- @since 0.3.1.0
type SnapshotGraph n na la = ([SnapshotNode n na], [SnapshotLink n la])

-- | A link in the snapshot graph.
--
-- 'SnapshotLink' is summary of one or more link observations by
-- different subject nodes. Basically the latest of these observations
-- is used to make 'SnapshotLink'.
--
-- - type @n@: node ID.
-- - type @la@: link attributes.
data SnapshotLink n la =
  SnapshotLink
  { _sourceNode :: n,
    _destinationNode :: n,
    _isDirected :: Bool,
    _linkTimestamp :: Timestamp,
    _linkAttributes :: la
    
    -- Maybe it's a good idea to include 'observationLogs', which can
    -- contain warnings or other logs about making this SnapshotLink.
  }
  deriving (Show,Eq,Generic)

-- | Comparison by node-tuple (source node, destination node).
instance (Ord n, Eq la) => Ord (SnapshotLink n la) where
  compare l r = compare (linkNodeTuple l) (linkNodeTuple r)

-- | @since 0.3.0.0
instance Functor (SnapshotLink n) where
  fmap f l = l { _linkAttributes = f $ _linkAttributes l }

-- | @since 0.3.0.0
instance Bifunctor SnapshotLink where
  bimap fn fla l = l { _linkAttributes = fla $ _linkAttributes l,
                       _sourceNode = fn $ _sourceNode l,
                       _destinationNode = fn $ _destinationNode l
                     }

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions
           { Aeson.fieldLabelModifier = modifier
           }
  where
    modifier = RE.replace reSnake . RE.replace reAttr . RE.replace reDest . RE.replace reTime
    reDest = fmap (const "dest") $ RE.string "destination"
    reAttr = fmap (const "Attrs") $ RE.string "Attributes"
    reTime = fmap (const "timestamp") (many RE.anySym *> RE.string "Timestamp")
    reSnake = RE.msym $ \c ->
      if c == '_'
      then Just ""
      else if isUpper c
           then Just ['_', toLower c]
           else Nothing

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON la) => FromJSON (SnapshotLink n la) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON la) => ToJSON (SnapshotLink n la) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt
  

-- | Node-tuple (source node, destination node) of the link.
linkNodeTuple :: SnapshotLink n la -> (n, n)
linkNodeTuple link = (_sourceNode link, _destinationNode link)

-- | Like 'linkNodeTuple', but this returns a 'Pair'.
linkNodePair :: SnapshotLink n la -> Pair n
linkNodePair = Pair . linkNodeTuple

-- | A node in the snapshot graph.
data SnapshotNode n na =
  SnapshotNode
  { _nodeId :: n,
    _isOnBoundary :: Bool,
    _nodeTimestamp :: Maybe Timestamp,
    _nodeAttributes :: Maybe na
  }
  deriving (Show,Eq,Generic)

-- | Comparison by node ID.
instance (Ord n, Eq na) => Ord (SnapshotNode n na) where
  compare l r = compare (_nodeId l) (_nodeId r)

-- | @since 0.3.0.0
instance Functor (SnapshotNode n) where
  fmap f n = n { _nodeAttributes = fmap f $ _nodeAttributes n }

-- | @since 0.3.0.0
instance Bifunctor SnapshotNode where
  bimap fn fna n = n { _nodeAttributes = fmap fna $ _nodeAttributes n,
                       _nodeId = fn $ _nodeId n
                     }

-- | @since 0.4.1.0
instance (FromJSON n, FromJSON na) => FromJSON (SnapshotNode n na) where
  parseJSON = Aeson.genericParseJSON aesonOpt

-- | @since 0.4.1.0
instance (ToJSON n, ToJSON na) => ToJSON (SnapshotNode n na) where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

