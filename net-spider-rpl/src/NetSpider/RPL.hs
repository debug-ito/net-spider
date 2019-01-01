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
import Data.Greskell (PropertyMap, Property, GValue, parseOneValue)
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

parseFindingType :: (PropertyMap m, Property p) => m p GValue -> Parser Text
parseFindingType = parseOneValue "type"

instance NodeAttributes RPLNode where
  writeNodeAttributes (RPLLocalNode ln) = writePropertyKeyValues pairs
    where
      pairs = [ ("type", Aeson.String "local"),
                ("rank", toJSON $ rank ln)
              ]
  writeNodeAttributes (RPLSRNode _) = writePropertyKeyValues [ ("type", Aeson.String "sr") ]
  parseNodeAttributes ps = do
    t <- parseFindingType ps
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
