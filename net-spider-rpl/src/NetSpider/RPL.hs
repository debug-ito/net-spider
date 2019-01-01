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
         NeighborType(..),
         RSSI,
         LocalLink(..),
         SRLink(..),
         RPLLink(..)
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

parseText :: (PropertyMap m, Property p) => Text -> m p GValue -> Parser Text
parseText key = parseOneValue key

instance NodeAttributes RPLNode where
  writeNodeAttributes (RPLLocalNode ln) = writePropertyKeyValues pairs
    where
      pairs = [ ("type", Aeson.String "local"),
                ("rank", toJSON $ rank ln)
              ]
  writeNodeAttributes (RPLSRNode _) = writePropertyKeyValues [ ("type", Aeson.String "sr") ]
  parseNodeAttributes ps = do
    t <- parseText "type" ps
    case t of
      "local" -> parseLocal
      "sr" -> parseSR
      _ -> fail ("Unknown finding type: " <> unpack t)
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

instance LinkAttributes RPLLink where
  writeLinkAttributes (RPLLocalLink ll) = do
    nt_steps <- writeNeighborTypeProps $ neighborType ll
    other <- writePropertyKeyValues pairs
    return (nt_steps <> other)
    where
      pairs = [ ("type", Aeson.String "local"),
                ("metric", toJSON $ metric ll),
                ("rssi", toJSON $ rssi ll)
              ]
  writeLinkAttributes (RPLSRLink _) = writePropertyKeyValues [("type", Aeson.String "sr")]
  parseLinkAttributes ps = do
    t <- parseText "type" ps
    case t of
      "local" -> parseLocal
      "sr" -> parseSR
      _ -> fail ("Unknown type: " <> unpack t)
    where
      parseLocal =
        fmap RPLLocalLink $ LocalLink
        <$> parseLinkAttributes ps
        <*> parseOneValue "metric" ps
        <*> parseOneValue "rssi" ps
      parseSR = return $ RPLSRLink SRLink
    
