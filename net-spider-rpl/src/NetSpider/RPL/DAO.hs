{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL.DAO
-- Description: Node and link information based on DAO (Destination Advertisement Object)
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.DAO
  ( -- * Types
    FoundNodeDAO,
    SnapshotGraphDAO,
    DAONode(..),
    DAOLink(..),
    -- * Query
    daoDefQuery,
    daoUnifierConf
  ) where

import Data.Greskell
  ( gIdentity
  )
  
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..))
import Data.Greskell
  ( parseOneValue, parseListValues
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import Data.Maybe (listToMaybe)
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import qualified NetSpider.GraphML.Writer as GraphML
import qualified NetSpider.Pangraph as Pan
import NetSpider.Pangraph.Atom (toAtom, Atom)
import qualified NetSpider.Query as Query
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | 'FoundNode' for a network described by DAOs.
type FoundNodeDAO = FoundNode FindingID DAONode DAOLink

-- | 'SnapshotGraph' for a network described by DAOs. This is what you
-- get by 'daoDefQuery'.
type SnapshotGraphDAO = SnapshotGraph FindingID DAONode DAOLink

-- | Node attributes about DAO.
data DAONode =
  DAONode
  { daoRouteNum :: Maybe Word
    -- ^ Number of DAO routes (downward routes) in the routing table
    -- of the node. Exact meaning of the "number of routes" depends on
    -- implementation.
    --
    -- In Non-Storing mode, 'daoRouteNum' for non-root nodes are
    -- supposed to be 'Nothing', because they don't have a downward
    -- routing table.
  }
  deriving (Show,Eq,Ord)

instance NodeAttributes DAONode where
  writeNodeAttributes dn = writePropertyKeyValues pairs
    where
      pairs =
        case daoRouteNum dn of
          Nothing -> []
          Just n -> [("dao_route_num", toJSON n)]
  parseNodeAttributes ps = fmap (DAONode . listToMaybe) $ parseListValues "dao_route_num" ps

instance Pan.ToAttributes DAONode where
  toAttributes = Pan.attributesFromGraphML

instance GraphML.ToAttributes DAONode where
  toAttributes dn =
    case daoRouteNum dn of
      Nothing -> []
      Just p -> [("dao_route_num", GraphML.AttrInt $ fromIntegral $ p)]


-- | Link attributes about DAO.
--
-- In Storing mode of RPL, a 'DAOLink' represents a link between the
-- sender and the receiver of a DAO. In non-storing mode, it's a link
-- between the sender of the DAO and the node specified in the
-- \"parent address\" field of the Transit Information option
-- contained in the DAO.
data DAOLink =
  DAOLink
  { pathLifetimeSec :: Word
    -- ^ Remaining lifetime of this link in seconds.
    --
    -- Lifetime is advertised in Transit Information Option in DAO,
    -- and managed in the routing table.
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes DAOLink where
  writeLinkAttributes dl = writePropertyKeyValues pairs
    where
      pairs = [ ("path_lifetime_sec", toJSON $ pathLifetimeSec dl)
              ]
  parseLinkAttributes ps = DAOLink <$> parseOneValue "path_lifetime_sec" ps

instance Pan.ToAttributes DAOLink where
  toAttributes = Pan.attributesFromGraphML

instance GraphML.ToAttributes DAOLink where
  toAttributes dl = [ ("path_lifetime_sec", GraphML.AttrInt $ fromIntegral $ pathLifetimeSec dl) ]

-- | Default 'Query.Query' for DAO nodes.
daoDefQuery :: [FindingID] -- ^ 'Query.startsFrom' field.
            -> Query.Query FindingID DAONode DAOLink DAOLink
daoDefQuery start =
  (Query.defQuery start)
  { Query.startsFrom = start,
    Query.unifyLinkSamples = Unify.unifyStd daoUnifierConf
  }

-- | 'UnifyStdConfig' for RPL DAO data. Used in 'defQuery'.
daoUnifierConf :: UnifyStdConfig FindingID DAONode DAOLink DAOLink ()
daoUnifierConf = Unify.defUnifyStdConfig { Unify.negatesLinkSample = \_ _ -> False }
