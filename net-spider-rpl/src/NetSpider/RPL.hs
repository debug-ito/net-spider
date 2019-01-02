{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.RPL
       ( Rank,
         LocalNode(..),
         SRNode(..),
         RPLNode(..),
         FindingType(..),
         nodeFindingType,
         NeighborType(..),
         RSSI,
         LocalLink(..),
         SRLink(..),
         RPLLink(..),
         linkFindingType,
         MergedRPLLink(..),
         MergedLocalLink(..),
         rplUnifierConf
       ) where

import Data.Aeson (ToJSON(toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import Data.Greskell
  ( PropertyMap, Property, GValue, parseOneValue,
    Binder, Walk, SideEffect, Element, gIdentity
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

-- | RPL rank
type Rank = Word

-- | Node attributes observed locally at an individual node.
data LocalNode =
  LocalNode
  { rank :: !Rank
    -- ^ RPL rank
  }
  deriving (Show,Eq,Ord)

-- | Node attributes observed in the Source-routing (SR)
-- table. Basically only for RPL non-storing mode.
data SRNode = SRNode
            deriving (Show,Eq,Ord)

-- | Node attributes unifying 'LocalNode' and 'SRNode'.
data RPLNode = RPLLocalNode LocalNode
             | RPLSRNode SRNode
             deriving (Show,Eq,Ord)

-- | Type of local finding.
data FindingType = FindingLocal
                   -- ^ Local finding is observed locally at an
                   -- individual node.
                 | FindingSR
                   -- ^ Local finding is observed in the
                   -- source-routing (SR) table. Basically only for
                   -- RPL non-storing mode.
                 deriving (Show,Eq,Ord)

nodeFindingType :: RPLNode -> FindingType
nodeFindingType (RPLLocalNode _) = FindingLocal
nodeFindingType (RPLSRNode _) = FindingSR

parseText :: (PropertyMap m, Property p) => Text -> m p GValue -> Parser Text
parseText key = parseOneValue key

-- | Unsafely convert walk's type signature
adaptWalk :: (Element e1, Element e2) => Walk SideEffect e1 e1 -> Walk SideEffect e2 e2
adaptWalk = bimap undefined undefined

writeFindingTypeProps :: Element e => FindingType -> Binder (Walk SideEffect e e)
writeFindingTypeProps ft = writePropertyKeyValues [("finding_type", Aeson.String ft_str)]
  where
    ft_str = case ft of
               FindingLocal -> "local"
               FindingSR -> "sr"

parseFindingTypeProps :: (PropertyMap m, Property p) => m p GValue -> Parser FindingType
parseFindingTypeProps ps = do
  ft <- parseText "finding_type" ps
  case ft of
    "local" -> return FindingLocal
    "sr" -> return FindingSR
    _ -> fail ("Unknown finding type: " <> unpack ft)

instance NodeAttributes FindingType where
  writeNodeAttributes = writeFindingTypeProps
  parseNodeAttributes = parseFindingTypeProps

instance LinkAttributes FindingType where
  writeLinkAttributes = writeFindingTypeProps
  parseLinkAttributes = parseFindingTypeProps

instance NodeAttributes LocalNode where
  writeNodeAttributes ln = writePropertyKeyValues pairs
    where
      pairs = [ ("rank", toJSON $ rank ln)
              ]
  parseNodeAttributes ps = LocalNode <$> parseOneValue "rank" ps

instance NodeAttributes SRNode where
  writeNodeAttributes _ = return gIdentity
  parseNodeAttributes _ = return SRNode

instance NodeAttributes RPLNode where
  writeNodeAttributes (RPLLocalNode ln) = do
    ft_steps <- writeFindingTypeProps FindingLocal
    ln_steps <- writeNodeAttributes ln
    return (ft_steps <> adaptWalk ln_steps)
  writeNodeAttributes (RPLSRNode sn) = do
    ft_steps <- writeFindingTypeProps FindingSR
    sn_steps <- writeNodeAttributes sn
    return (ft_steps <> adaptWalk sn_steps)
  parseNodeAttributes ps = do
    ft <- parseFindingTypeProps ps
    case ft of
      FindingLocal -> fmap RPLLocalNode $ parseNodeAttributes ps
      FindingSR -> fmap RPLSRNode $ parseNodeAttributes ps

-- | Classification of RPL neighbors.
data NeighborType = PreferredParent
                    -- ^ The neighbor is the preferred parent.
                  | ParentCandidate
                    -- ^ The neighbor is not the preferred parent, but
                    -- is in the parent set.
                  | OtherNeighbor
                    -- ^ The neighbor is not in the parent set.
                  deriving (Show,Eq,Ord,Enum,Bounded)

instance LinkAttributes NeighborType where
  writeLinkAttributes nt = writePropertyKeyValues [("neighbor_type", nt_str)]
    where
      nt_str :: Text
      nt_str = case nt of
        PreferredParent -> "preferred_parent"
        ParentCandidate -> "parent_candidate"
        OtherNeighbor -> "other_neighbor"
  parseLinkAttributes ps = do
    t <- parseText "neighbor_type" ps
    case t of
      "preferred_parent" -> return PreferredParent
      "parent_candidate" -> return ParentCandidate
      "other_neighbor" -> return OtherNeighbor
      _ -> fail ("Unknown neighbor_type: " <> unpack t)

-- | Type of RSSI (Radio Signal Strength Indicator) in dBm.
type RSSI = Int

-- | Link attributes observed locally at an individual node.
data LocalLink =
  LocalLink
  { neighborType :: !NeighborType,
    -- ^ Type of the neighbor at the other end of this link.
    metric :: !Rank,
    -- ^ Link metric of this link.
    rssi :: !(Maybe RSSI)
    -- ^ RSSI observed via this link.
  }
  deriving (Show,Eq,Ord)

-- | Link attributes observed in the source-routing (SR) table.
data SRLink = SRLink
            deriving (Show,Eq,Ord)

-- | Link attributes unifying 'LocalLink' and 'SRLink'.
data RPLLink = RPLLocalLink LocalLink
             | RPLSRLink SRLink
             deriving (Show,Eq,Ord)

linkFindingType :: RPLLink -> FindingType
linkFindingType (RPLLocalLink _) = FindingLocal
linkFindingType (RPLSRLink _) = FindingSR

instance LinkAttributes LocalLink where
  writeLinkAttributes ll = do
    nt_steps <- writeLinkAttributes $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (adaptWalk nt_steps <> other)
    where
      pairs = [ ("metric", toJSON $ metric ll),
                ("rssi", toJSON $ rssi ll)
              ]
  parseLinkAttributes ps =
    LocalLink
    <$> parseLinkAttributes ps
    <*> parseOneValue "metric" ps
    <*> parseOneValue "rssi" ps

instance LinkAttributes SRLink where
  writeLinkAttributes _ = return gIdentity
  parseLinkAttributes _ = return SRLink

instance LinkAttributes RPLLink where
  writeLinkAttributes (RPLLocalLink ll) = do
    ft_steps <- writeFindingTypeProps FindingLocal
    other <- writeLinkAttributes ll
    return (ft_steps <> adaptWalk other)
  writeLinkAttributes (RPLSRLink sl) = do
    ft_steps <- writeFindingTypeProps FindingSR
    other <- writeLinkAttributes sl
    return (ft_steps <> adaptWalk other)
  parseLinkAttributes ps = do
    ft <- parseFindingTypeProps ps
    case ft of
      FindingLocal -> RPLLocalLink <$> parseLinkAttributes ps
      FindingSR -> RPLSRLink <$> parseLinkAttributes ps

-- | Link attributes merging two 'LocalLink's from the two end nodes
-- of the link.
data MergedLocalLink =
  MergedLocalLink
  { fromSource :: !LocalLink,
    fromDest :: !(Maybe LocalLink)
  }
  deriving (Show,Eq,Ord)

-- | Link attributes merging two 'RPLLink's from the two end nodes of
-- the link.
data MergedRPLLink = MergedRPLLocalLink MergedLocalLink
                   | MergedRPLSRLink SRLink
                   deriving (Show,Eq,Ord)

-- | 'UnifyStdConfig' for RPL data model.
rplUnifierConf :: Eq n => UnifyStdConfig n RPLNode RPLLink MergedRPLLink FindingType
rplUnifierConf = Unify.UnifyStdConfig
                 { Unify.makeLinkSubId = makeSubId,
                   Unify.mergeSamples = merger,
                   Unify.negatesLinkSample = \_ _ -> False
                 }
  where
    makeSubId ls = linkFindingType $ lsLinkAttributes ls
    merger llinks rlinks =
      case (latestLinkSample llinks, latestLinkSample rlinks) of
        (Nothing, Nothing) -> Nothing
        (Just ll, Nothing) -> Just $ doMerge ll Nothing
        (Nothing, Just rl) -> Just $ doMerge rl Nothing
        (Just ll, Just rl) -> Just $ doMerge ll $ Just rl
    doMerge main_link msub_link =
      case lsLinkAttributes main_link of
        RPLLocalLink ll -> doMergeLocal main_link ll msub_link
        RPLSRLink sl -> doMergeSR main_link sl msub_link
    doMergeLocal main_link main_ll msub_link =
      case fmap getLsLinkAttrs $ msub_link of
        Nothing -> main_link
                   { lsLinkAttributes = MergedRPLLocalLink $ MergedLocalLink main_ll Nothing }
        Just (_, RPLSRLink _) -> main_link
                                 { lsLinkAttributes = MergedRPLLocalLink $ MergedLocalLink main_ll Nothing }
        Just (sub_link, RPLLocalLink sub_ll) ->
          if neighborType main_ll <= neighborType sub_ll
          then main_link { lsLinkAttributes = MergedRPLLocalLink $ MergedLocalLink main_ll $ Just sub_ll }
          else sub_link { lsLinkAttributes = MergedRPLLocalLink $ MergedLocalLink sub_ll $ Just main_ll }
    getLsLinkAttrs ls = (ls, lsLinkAttributes ls)
    doMergeSR main_link main_sl _ = main_link { lsLinkAttributes = MergedRPLSRLink main_sl }
