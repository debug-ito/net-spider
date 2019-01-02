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
import Data.Greskell
  ( PropertyMap, Property, GValue, parseOneValue,
    Binder, Walk, SideEffect, Element
  )
import Data.Greskell.Extra (writePropertyKeyValues)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Unify (UnifyStdConfig, lsLinkAttributes, latestLinkSample)
import qualified NetSpider.Unify as Unify

type Rank = Word

data LocalNode =
  LocalNode
  { rank :: !Rank
  }
  deriving (Show,Eq,Ord)

data SRNode = SRNode
            deriving (Show,Eq,Ord)

data RPLNode = RPLLocalNode LocalNode
             | RPLSRNode SRNode
             deriving (Show,Eq,Ord)

data FindingType = FindingLocal
                 | FindingSR
                 deriving (Show,Eq,Ord)

nodeFindingType :: RPLNode -> FindingType
nodeFindingType (RPLLocalNode _) = FindingLocal
nodeFindingType (RPLSRNode _) = FindingSR

parseText :: (PropertyMap m, Property p) => Text -> m p GValue -> Parser Text
parseText key = parseOneValue key

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

instance NodeAttributes RPLNode where
  writeNodeAttributes (RPLLocalNode ln) = do
    ft <- writeFindingTypeProps FindingLocal
    other <- writePropertyKeyValues pairs
    return (ft <> other)
    where
      pairs = [ ("rank", toJSON $ rank ln)
              ]
  writeNodeAttributes (RPLSRNode _) = writeFindingTypeProps FindingSR
  parseNodeAttributes ps = do
    ft <- parseFindingTypeProps ps
    case ft of
      FindingLocal -> parseLocal
      FindingSR -> parseSR
    where
      parseLocal = fmap RPLLocalNode $ LocalNode <$> parseOneValue "rank" ps
      parseSR = return $ RPLSRNode SRNode

data NeighborType = PreferredParent
                  | ParentCandidate
                  | OtherNeighbor
                  deriving (Show,Eq,Ord,Enum,Bounded)

writeNeighborTypeProps :: Element e => NeighborType -> Binder (Walk SideEffect e e)
writeNeighborTypeProps nt = writePropertyKeyValues [("neighbor_type", nt_str)]
  where
    nt_str :: Text
    nt_str = case nt of
      PreferredParent -> "preferred_parent"
      ParentCandidate -> "parent_candidate"
      OtherNeighbor -> "other_neighbor"

instance LinkAttributes NeighborType where
  writeLinkAttributes = writeNeighborTypeProps
  parseLinkAttributes ps = do
    t <- parseText "neighbor_type" ps
    case t of
      "preferred_parent" -> return PreferredParent
      "parent_candidate" -> return ParentCandidate
      "other_neighbor" -> return OtherNeighbor
      _ -> fail ("Unknown neighbor_type: " <> unpack t)

type RSSI = Int

data LocalLink =
  LocalLink
  { neighborType :: !NeighborType,
    metric :: !Rank,
    rssi :: !(Maybe RSSI)
  }
  deriving (Show,Eq,Ord)

data SRLink = SRLink
            deriving (Show,Eq,Ord)

data RPLLink = RPLLocalLink LocalLink
             | RPLSRLink SRLink
             deriving (Show,Eq,Ord)

linkFindingType :: RPLLink -> FindingType
linkFindingType (RPLLocalLink _) = FindingLocal
linkFindingType (RPLSRLink _) = FindingSR

instance LinkAttributes RPLLink where
  writeLinkAttributes (RPLLocalLink ll) = do
    ft_steps <- writeFindingTypeProps FindingLocal
    nt_steps <- writeNeighborTypeProps $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (ft_steps <> nt_steps <> other)
    where
      pairs = [ ("metric", toJSON $ metric ll),
                ("rssi", toJSON $ rssi ll)
              ]
  writeLinkAttributes (RPLSRLink _) = writeFindingTypeProps FindingSR
  parseLinkAttributes ps = do
    ft <- parseFindingTypeProps ps
    case ft of
      FindingLocal -> parseLocal
      FindingSR -> parseSR
    where
      parseLocal =
        fmap RPLLocalLink $ LocalLink
        <$> parseLinkAttributes ps
        <*> parseOneValue "metric" ps
        <*> parseOneValue "rssi" ps
      parseSR = return $ RPLSRLink SRLink

data MergedLocalLink =
  MergedLocalLink
  { fromSource :: !LocalLink,
    fromDest :: !(Maybe LocalLink)
  }
  deriving (Show,Eq,Ord)

data MergedRPLLink = MergedRPLLocalLink MergedLocalLink
                   | MergedRPLSRLink SRLink
                   deriving (Show,Eq,Ord)

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
  
  


    
