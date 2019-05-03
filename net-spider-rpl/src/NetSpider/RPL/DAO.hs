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
    DAONode(..),
    DAOLink(..),
    -- * Unifier
    daoUnifierConf
  ) where

import Data.Greskell
  ( gIdentity
  )
  
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON(..))
import Data.Greskell
  ( parseOneValue
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import qualified NetSpider.Pangraph as Pan
import NetSpider.Pangraph.Atom (toAtom, Atom)
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | 'FoundNode' for a network described by DAOs.
type FoundNodeDAO = FoundNode FindingID DAONode DAOLink

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
      pairs = [ ("dao_route_num", toJSON $ daoRouteNum dn)
              ]
  parseNodeAttributes ps = DAONode <$> parseOneValue "dao_route_num" ps

instance Pan.ToAttributes DAONode where
  toAttributes dn = 
    case daoRouteNum dn of
      Nothing -> []
      Just p -> [("dao_route_num", toAtom $ p)]

-- | Link attributes about DAO.
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
  toAttributes dl = [ ("path_lifetime_sec", toAtom $ pathLifetimeSec dl) ]

-- | 'UnifyStdConfig' for RPL DAO data.
daoUnifierConf :: UnifyStdConfig FindingID DAONode DAOLink DAOLink ()
daoUnifierConf = Unify.defUnifyStdConfig { Unify.negatesLinkSample = \_ _ -> False }
