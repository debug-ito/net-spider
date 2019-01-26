{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: NetSpider.RPL.FindingID
-- Description: NetSpider Node ID type for RPL network graph
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.RPL.FindingID
  ( FindingID(..),
    idToText,
    idFromText,
    FindingType(..),
    typeToText,
    typeFromText
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

-- | Type of local finding.
data FindingType = FindingLocal
                   -- ^ Local finding is observed locally at an
                   -- individual node.
                 | FindingSR
                   -- ^ Local finding is observed in the
                   -- source-routing (SR) table. Basically only for
                   -- RPL non-storing mode.
                 deriving (Show,Eq,Ord,Enum,Bounded,Generic)

instance Hashable FindingType

typeToText :: FindingType -> Text
typeToText ft =
  case ft of
    FindingLocal -> "local"
    FindingSR -> "sr"

typeFromText :: Text -> Maybe FindingType
typeFromText t =
  case t of
    "local" -> Just FindingLocal
    "sr" -> Just FindingSR
    _ -> Nothing

-- | The node ID.
--
-- Basically a node is identified by its IPv6 address in RPL
-- network. 'FindingID' is distinguished by 'FindingType' as well,
-- because in RPL there can be difference between topology formed by
-- individual nodes and topology stored in the source routing table of
-- the root node (for non-storing mode of operation)
data FindingID =
  FindingID
  { findingType :: !FindingType,
    -- ^ Finding type
    nodeAddress :: !IPv6
    -- ^ IPv6 address of the subject node.
  }
  deriving (Show,Eq,Ord,Generic)

idToText :: FindingID -> Text
idToText fid = ft_str <> "://" <> addr_str
  where
    ft_str = typeToText $ findingType fid
    addr_str = IPv6.encode $ nodeAddress fid

idFromText :: Text -> Maybe FindingID
idFromText t = FindingID <$> m_ft <*> m_addr
  where
    (ft_str, rest) = T.breakOn "://" t
    addr_str = T.drop 3 rest
    m_ft = typeFromText ft_str
    m_addr = IPv6.decode addr_str

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
  hashWithSalt s fid = s `hashWithSalt` ft `hashWithSalt` addrA `hashWithSalt` addrB
    where
      ft = findingType fid
      addrA = ipv6A $ nodeAddress fid
      addrB = ipv6B $ nodeAddress fid

instance ToAtom FindingID where
  toAtom = toAtom . idToText
