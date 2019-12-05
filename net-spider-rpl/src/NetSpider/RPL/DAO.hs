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
  ( lookupAs', Key, pMapToFail, lookupAs, keyText
  )
import Data.Greskell.Extra (writeKeyValues, (<=:>), (<=?>))
import Data.Maybe (listToMaybe)
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VFoundNode, EFinds)
import qualified NetSpider.GraphML.Writer as GraphML
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

keyDaoRouteNum :: Key VFoundNode (Maybe Word)
keyDaoRouteNum = "dao_route_num"

instance NodeAttributes DAONode where
  writeNodeAttributes dn = fmap writeKeyValues $ sequence [keyDaoRouteNum <=?> daoRouteNum dn]
  parseNodeAttributes ps = DAONode <$> (pMapToFail $ lookupAs' keyDaoRouteNum ps)

instance GraphML.ToAttributes DAONode where
  toAttributes dn =
    case daoRouteNum dn of
      Nothing -> []
      Just p -> [(keyText keyDaoRouteNum, GraphML.AttrInt $ fromIntegral $ p)]


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

keyPathLifetimeSec :: Key EFinds Word
keyPathLifetimeSec = "path_lifetime_sec"

instance LinkAttributes DAOLink where
  writeLinkAttributes dl = fmap writeKeyValues $ sequence pairs
    where
      pairs = [ keyPathLifetimeSec <=:> pathLifetimeSec dl
              ]
  parseLinkAttributes ps = DAOLink <$> (pMapToFail $ lookupAs keyPathLifetimeSec ps)

instance GraphML.ToAttributes DAOLink where
  toAttributes dl = [ (keyText keyPathLifetimeSec, GraphML.AttrInt $ fromIntegral $ pathLifetimeSec dl) ]

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
