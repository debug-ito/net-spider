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
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import qualified NetSpider.Pangraph as Pan
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | 'FoundNode' for a network described by DAOs.
type FoundNodeDAO = FoundNode FindingID DAONode DAOLink

-- | Node attributes about DAO.
data DAONode = DAONode
            deriving (Show,Eq,Ord)

instance NodeAttributes DAONode where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return DAONode

instance Pan.ToAttributes DAONode where
  toAttributes _ = []

-- | Link attributes about DAO.
data DAOLink = DAOLink
            deriving (Show,Eq,Ord)

instance LinkAttributes DAOLink where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return DAOLink

instance Pan.ToAttributes DAOLink where
  toAttributes _ = []

-- | 'UnifyStdConfig' for RPL DAO data.
daoUnifierConf :: UnifyStdConfig FindingID DAONode DAOLink DAOLink ()
daoUnifierConf = Unify.defUnifyStdConfig { Unify.negatesLinkSample = \_ _ -> False }
