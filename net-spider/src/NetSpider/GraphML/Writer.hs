{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: NetSpider.GraphML.Writer
-- Description: Serialize a Snapshot graph into GraphML format.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- http://graphml.graphdrawing.org/primer/graphml-primer.html
module NetSpider.GraphML.Writer
  ( -- * Functions
    writeGraphML,
    -- * NodeID
    NodeID,
    ToNodeID(..),
    nodeIDByShow,
    -- * Attributes
    AttributeKey,
    AttributeValue(..),
    ToAttributes(..)
  ) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), Monoid(..), mconcat)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time (TimeZone(..))
import GHC.Generics (Generic)

import NetSpider.Snapshot
  ( SnapshotNode, nodeId, nodeTimestamp, isOnBoundary,
    SnapshotLink, sourceNode, destinationNode, linkTimestamp
  )
import NetSpider.Timestamp (Timestamp(epochTime, timeZone))

-- | Node ID in GraphML.
type NodeID = Text

class ToNodeID a where
  toNodeID :: a -> NodeID

nodeIDByShow :: Show a => a -> NodeID
nodeIDByShow = pack . show

instance ToNodeID Text where
  toNodeID = id
  
-- TODO: use ToAtom to define ToNodeID.

-- | Key of attribute.
type AttributeKey = Text

-- | Typed value of attribute.
data AttributeValue = AttrBoolean Bool
                    | AttrInt Int
                    | AttrLong Integer
                    | AttrFloat Float
                    | AttrDouble Double
                    | AttrString Text
                    deriving (Show,Eq,Ord)

-- | Type that can be converted to list of attributes.
class ToAttributes a where
  toAttributes :: a -> [(AttributeKey, AttributeValue)]

instance ToAttributes () where
  toAttributes _ = []

-- TODO: use net-spider-pangraph's ToAttributes to define this
-- ToAttributes.

showAttributeValue :: AttributeValue -> TLB.Builder
showAttributeValue _ = "" -- TODO

-- | Type specifier of 'AttributeValue'
data AttributeType = ATBoolean
                   | ATInt
                   | ATLong
                   | ATFloat
                   | ATDouble
                   | ATString
                   deriving (Show,Eq,Ord,Generic)

instance Hashable AttributeType

valueType :: AttributeValue -> AttributeType
valueType v =
  case v of
    AttrBoolean _ -> ATBoolean
    AttrInt _ -> ATInt
    AttrLong _ -> ATLong
    AttrFloat _ -> ATFloat
    AttrDouble _ -> ATDouble
    AttrString _ -> ATString

showAttributeType :: AttributeType -> TLB.Builder
showAttributeType t =
  case t of
    ATBoolean -> "boolean"
    ATInt -> "int"
    ATLong -> "long"
    ATFloat -> "float"
    ATDouble -> "double"
    ATString -> "string"

-- | Domain (`for` field of the key) of attribute.
data AttributeDomain = DomainGraph
                     | DomainNode
                     | DomainEdge
                     | DomainAll
                     deriving (Show,Eq,Ord,Generic)

instance Hashable AttributeDomain

showDomain :: AttributeDomain -> TLB.Builder
showDomain d =
  case d of
    DomainGraph -> "graph"
    DomainNode -> "node"
    DomainEdge -> "edge"
    DomainAll -> "all"

-- | Meta data for a key.
data KeyMeta =
  KeyMeta
  { kmName :: AttributeKey,
    kmType :: AttributeType,
    kmDomain :: AttributeDomain
  }
  deriving (Show,Eq,Ord,Generic)

instance Hashable KeyMeta

makeMetaValue :: AttributeDomain -> AttributeKey -> AttributeValue -> (KeyMeta, AttributeValue)
makeMetaValue d k v = (meta, v)
  where
    meta = KeyMeta { kmName = k,
                     kmType = valueType v,
                     kmDomain = d
                   }

-- | Storage of key metadata.
data KeyStore =
  KeyStore
  { ksMetas :: [KeyMeta],
    ksIndexFor :: HashMap KeyMeta Int
  }
  deriving (Show,Eq,Ord)

emptyKeyStore :: KeyStore
emptyKeyStore = KeyStore { ksMetas = [],
                           ksIndexFor = HM.empty
                         }

addKey :: KeyMeta -> KeyStore -> KeyStore
addKey new_meta ks = KeyStore { ksMetas = if HM.member new_meta $ ksIndexFor ks
                                          then ksMetas ks
                                          else new_meta : ksMetas ks,
                                ksIndexFor = HM.insertWith (\_ old -> old) new_meta (length $ ksMetas ks) $ ksIndexFor ks
                              }

keyIndex :: KeyStore -> KeyMeta -> Maybe Int
keyIndex ks km = HM.lookup km $ ksIndexFor ks

showAllKeys :: TLB.Builder -> KeyStore -> TLB.Builder
showAllKeys prefix ks = mconcat $ map (prefix <>) $ catMaybes $ map (showKeyMeta ks) $ reverse $ ksMetas ks

showKeyMeta :: KeyStore -> KeyMeta -> Maybe TLB.Builder
showKeyMeta ks km = fmap (\i -> showKeyMetaWithIndex i km) $ keyIndex ks km

showAttributeID :: Int -> TLB.Builder
showAttributeID index = "d" <> (TLB.fromString $ show index)

showKeyMetaWithIndex :: Int -> KeyMeta -> TLB.Builder
showKeyMetaWithIndex index km = "<key id=\"" <> id_str <> "\" for=\"" <> domain_str
                                <> "\" attr.name=\"" <> name_str <> "\" attr.type=\"" <> type_str <> "\"/>\n"
  where
    id_str = showAttributeID index
    domain_str = showDomain $ kmDomain km
    name_str = encodeXML $ kmName km
    type_str = showAttributeType $ kmType km

timestampAttrs :: Timestamp -> [(AttributeKey, AttributeValue)]
timestampAttrs t = [("@timestamp", AttrLong $ toInteger $ epochTime t)] ++ timezone_attrs
  where
    timezone_attrs =
      case timeZone t of
        Nothing -> []
        Just tz -> [ ("@tz_offset_min", AttrInt $ timeZoneMinutes tz),
                     ("@tz_summer_only", AttrBoolean $ timeZoneSummerOnly tz),
                     ("@tz_name", AttrString $ pack $ timeZoneName tz)
                   ]

nodeMetaKeys :: ToAttributes na => SnapshotNode n na -> [KeyMeta]
nodeMetaKeys n = map fst $ nodeMetaValues n

nodeMetaValues :: ToAttributes na => SnapshotNode n na -> [(KeyMeta, AttributeValue)]
nodeMetaValues n = map convert $ base <> attrs
  where
    timestamp = case nodeTimestamp n of
                  Nothing -> []
                  Just t -> timestampAttrs t
    base = timestamp <> [("@is_on_boundary", AttrBoolean $ isOnBoundary n)]
    attrs = [] -- TODO
    convert (k, v) = makeMetaValue DomainNode k v

linkMetaKeys :: ToAttributes la => SnapshotLink n la -> [KeyMeta]
linkMetaKeys l = map fst $ linkMetaValues l

linkMetaValues :: ToAttributes la => SnapshotLink n la -> [(KeyMeta, AttributeValue)]
linkMetaValues l = map convert $ base <> attrs
  where
    base = timestampAttrs $ linkTimestamp l
    attrs = [] -- TODO
    convert (k, v) = makeMetaValue DomainEdge k v

showAttribute :: KeyStore -> KeyMeta -> AttributeValue -> TLB.Builder
showAttribute ks km val =
  case keyIndex ks km of
    Nothing -> ""
    Just index -> "<data key=\"" <> key_id_str <> "\">" <> val_str <> "</data>\n"
      where
        key_id_str = showAttributeID index
        val_str = showAttributeValue val

writeGraphML :: (ToNodeID n, ToAttributes na, ToAttributes la)
             => [SnapshotNode n na] -> [SnapshotLink n la] -> TL.Text
writeGraphML input_nodes input_links =
  TLB.toLazyText ( xml_header
                   <> graphml_header
                   <> keys
                   <> graph_header
                   <> nodes
                   <> edges
                   <> graph_footer
                   <> graphml_footer
                 )
  where
    xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    graphml_header = "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n"
                     <> " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
                     <> " xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"
    graphml_footer = "</graphml>\n"
    keys = showAllKeys "" key_store
    key_metas = concat $ map nodeMetaKeys input_nodes <> map linkMetaKeys input_links
    key_store = foldl' (\acc m -> addKey m acc) emptyKeyStore key_metas
    graph_header = "<graph edgedefault=\"undirected\">\n"
    graph_footer = "</graph>\n"
    nodes = mconcat $ map writeNode input_nodes
    edges = mconcat $ map writeLink input_links
    showAttribute' (k,v) = ("    " <> ) $ showAttribute key_store k v
    writeNode n = "  <node id=\"" <> (encodeNodeID $ nodeId n) <> "\">\n"
                  <> (mconcat $ map showAttribute' $ nodeMetaValues n)
                  <> "  </node>\n"
    writeLink l = "  <edge source=\"" <> (encodeNodeID $ sourceNode l)
                  <> "\" target=\"" <> (encodeNodeID $ destinationNode l) <> "\">\n"
                  <> (mconcat $ map showAttribute' $ linkMetaValues l)
                  <> "  </edge>\n"

encodeNodeID :: ToNodeID n => n -> TLB.Builder
encodeNodeID = encodeXML . toNodeID

encodeXML :: Text -> TLB.Builder
encodeXML = TLB.fromText -- TODO
