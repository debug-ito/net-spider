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
import GHC.Generics (Generic)

import NetSpider.Snapshot
  ( SnapshotNode, nodeId, nodeTimestamp, isOnBoundary,
    SnapshotLink, sourceNode, destinationNode, linkTimestamp
  )
import NetSpider.Timestamp (Timestamp(epochTime))

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

keyMetaTimestamp :: KeyMeta
keyMetaTimestamp = KeyMeta { kmName = "@timestamp",
                             kmType = ATLong,
                             kmDomain = DomainAll
                           }

keyMetaIsOnBoundary :: KeyMeta
keyMetaIsOnBoundary = KeyMeta { kmName = "@is_on_boundary",
                                kmType = ATBoolean,
                                kmDomain = DomainNode
                              }

timestampGraphMLAttrs :: Timestamp -> [(KeyMeta, AttributeValue)]
timestampGraphMLAttrs t = [(keyMetaTimestamp, AttrLong $ toInteger $ epochTime t)]
  -- TODO: write timezone

nodeKeyMetas :: ToAttributes na => SnapshotNode n na -> [KeyMeta]
nodeKeyMetas n = map fst $ nodeGraphMLAttrs n

nodeGraphMLAttrs :: ToAttributes na => SnapshotNode n na -> [(KeyMeta, AttributeValue)]
nodeGraphMLAttrs n = base <> attrs
  where
    timestamp = case nodeTimestamp n of
                  Nothing -> []
                  Just t -> timestampGraphMLAttrs t
    base = timestamp <> [(keyMetaIsOnBoundary, AttrBoolean $ isOnBoundary n)]
    attrs = [] -- TODO

linkKeyMetas :: ToAttributes la => SnapshotLink n la -> [KeyMeta]
linkKeyMetas l = map fst $ linkGraphMLAttrs l

linkGraphMLAttrs :: ToAttributes la => SnapshotLink n la -> [(KeyMeta, AttributeValue)]
linkGraphMLAttrs l = base <> attrs
  where
    base = timestampGraphMLAttrs $ linkTimestamp l
    attrs = [] -- TODO

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
    key_metas = concat $ map nodeKeyMetas input_nodes <> map linkKeyMetas input_links
    key_store = foldl' (\acc m -> addKey m acc) emptyKeyStore key_metas
    graph_header = "<graph edgedefault=\"undirected\">\n"
    graph_footer = "</graph>\n"
    nodes = mconcat $ map writeNode input_nodes
    edges = mconcat $ map writeLink input_links
    showAttribute' (k,v) = ("    " <> ) $ showAttribute key_store k v
    writeNode n = "  <node id=\"" <> (encodeNodeID $ nodeId n) <> "\">\n"
                  <> (mconcat $ map showAttribute' $ nodeGraphMLAttrs n)
                  <> "  </node>\n"
    writeLink l = "  <edge source=\"" <> (encodeNodeID $ sourceNode l)
                  <> "\" target=\"" <> (encodeNodeID $ destinationNode l) <> "\">\n"
                  <> (mconcat $ map showAttribute' $ linkGraphMLAttrs l)
                  <> "  </edge>\n"

encodeNodeID :: ToNodeID n => n -> TLB.Builder
encodeNodeID = encodeXML . toNodeID

encodeXML :: Text -> TLB.Builder
encodeXML = TLB.fromText -- TODO
