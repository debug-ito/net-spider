{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: NetSpider.RPL.FindingID
-- Description: NetSpider Node ID type for RPL network graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.FindingID
  ( -- * FindingID
    FindingID(..),
    idToText,
    idFromText,
    -- * FindingType
    FindingType(..),
    typeToText,
    typeFromText,
    -- * IPv6ID
    IPv6ID(..),
    ipv6ToText,
    ipv6FromText,
    ipv6Only
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Fail (MonadFail)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Greskell (FromGraphSON(..))
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Net.IPv6 (IPv6(..))
import qualified Net.IPv6 as IPv6
import NetSpider.Pangraph.Atom (ToAtom(..))
import NetSpider.GraphML.Writer (ToNodeID(..))

-- | Type of local finding.
data FindingType = FindingDIO
                   -- ^ Local finding about DIO (Upward) routes.
                 | FindingDAO
                   -- ^ Local finding about DAO (Downward) routes.
                 deriving (Show,Eq,Ord,Enum,Bounded,Generic)

instance Hashable FindingType

typeToText :: FindingType -> Text
typeToText ft =
  case ft of
    FindingDIO -> "dio"
    FindingDAO -> "dao"

typeFromText :: Text -> Maybe FindingType
typeFromText t =
  case t of
    "dio" -> Just FindingDIO
    "dao" -> Just FindingDAO
    _ -> Nothing

-- | The node ID.
--
-- Basically a node is identified by its IPv6 address in RPL
-- network. 'FindingID' is distinguished by 'FindingType' as well,
-- because in RPL there can be difference between topology formed by
-- DIOs and DAOs.
data FindingID =
  FindingID
  { findingType :: FindingType,
    -- ^ Finding type
    nodeAddress :: IPv6
    -- ^ IPv6 address of the subject node.
  }
  deriving (Show,Eq,Ord,Generic)

idToText :: FindingID -> Text
idToText fid = ft_str <> "://[" <> addr_str <> "]"
  where
    ft_str = typeToText $ findingType fid
    addr_str = ipv6ToText $ IPv6ID $ nodeAddress fid

idFromText :: Text -> Maybe FindingID
idFromText t = FindingID <$> m_ft <*> m_addr
  where
    (ft_str, rest) = T.breakOn "://[" t
    (addr_str, _) = T.breakOn "]" $ T.drop 4 rest
    m_ft = typeFromText ft_str
    m_addr = fmap unIPv6ID $ ipv6FromText addr_str

instance ToJSON FindingID where
  toJSON = Aeson.String . idToText

parseFromText :: MonadFail m => Text -> m FindingID
parseFromText t = 
  case idFromText t of
    Nothing -> fail ("Invalid FindingID: " <> T.unpack t)
    Just fid -> return fid

instance FromJSON FindingID where
  parseJSON v = parseFromText =<< parseJSON v

instance FromGraphSON FindingID where
  parseGraphSON gv = parseFromText =<< parseGraphSON gv

instance Hashable FindingID where
  hashWithSalt s fid = s `hashWithSalt` ft `hashWithSalt` addr_id
    where
      ft = findingType fid
      addr_id = IPv6ID $ nodeAddress fid

instance ToAtom FindingID where
  toAtom = toAtom . idToText

instance ToNodeID FindingID where
  toNodeID = idToText

-- | 'IPv6' address with additional type-class instances.
newtype IPv6ID = IPv6ID { unIPv6ID :: IPv6 }
               deriving (Show,Eq,Ord,Generic)

instance Hashable IPv6ID where
  hashWithSalt s (IPv6ID a) =
    s `hashWithSalt` (ipv6A a) `hashWithSalt` (ipv6B a)

ipv6ToText :: IPv6ID -> Text
ipv6ToText (IPv6ID a) = IPv6.encode a

ipv6FromText :: Text -> Maybe IPv6ID
ipv6FromText = fmap IPv6ID . IPv6.decode

parseIPv6IDFromText :: MonadFail m => Text -> m IPv6ID
parseIPv6IDFromText t =
  case ipv6FromText t of
    Nothing -> fail ("Invalid IPv6 address: " <> T.unpack t)
    Just a -> return a

instance FromJSON IPv6ID where
  parseJSON v = parseIPv6IDFromText =<< parseJSON v

instance FromGraphSON IPv6ID where
  parseGraphSON gv = parseIPv6IDFromText =<< parseGraphSON gv

instance ToAtom IPv6ID where
  toAtom = toAtom . ipv6ToText

instance ToNodeID IPv6ID where
  toNodeID = ipv6ToText

-- | Extract 'IPv6ID' from 'FindingID'.
ipv6Only :: FindingID -> IPv6ID
ipv6Only = IPv6ID . nodeAddress
