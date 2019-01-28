-- |
-- Module: NetSpider.RPL.SR
-- Description: NetSpider data model for source routing (SR) table
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.SR
  ( -- * Types
    FoundNodeSR,
    SRNode(..),
    SRLink(..),
    -- * Unifier
    srUnifierConf
  ) where

import Data.Greskell
  ( gIdentity
  )
import NetSpider.Found (FoundNode)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

import NetSpider.RPL.FindingID (FindingID)

-- | The 'FoundNode' observed in the source routing (SR) table.
type FoundNodeSR = FoundNode FindingID SRNode SRLink

-- | Node attributes observed in the Source-routing (SR)
-- table. Basically only for RPL non-storing mode.
data SRNode = SRNode
            deriving (Show,Eq,Ord)

instance NodeAttributes SRNode where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return SRNode

-- | Link attributes observed in the source-routing (SR) table.
data SRLink = SRLink
            deriving (Show,Eq,Ord)

instance LinkAttributes SRLink where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return SRLink

-- | 'UnifyStdConfig' for RPL source-routing (SR) findings.
srUnifierConf :: UnifyStdConfig FindingID SRNode SRLink SRLink ()
srUnifierConf = Unify.defUnifyStdConfig { Unify.negatesLinkSample = \_ _ -> False }
