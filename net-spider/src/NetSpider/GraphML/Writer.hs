{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module: NetSpider.GraphML.Writer
-- Description: Serialize a Snapshot graph into GraphML format.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines tools to serialize a 'SnapshotGraph' into
-- [GraphML](http://graphml.graphdrawing.org/primer/graphml-primer.html)
-- format.
--
-- @since 0.3.1.0
module NetSpider.GraphML.Writer
  ( -- * Functions
    writeGraphML,
    writeGraphMLWith,
    -- * Options
    WriteOption,
    defWriteOption,
    -- ** accessors for WriteOption
    woptDefaultDirected,
    -- * NodeID
    NodeID,
    ToNodeID(..),
    nodeIDByShow,
    -- * Attributes
    AttributeKey,
    AttributeValue(..),
    ToAttributes(..),
    valueFromAeson,
    attributesFromAeson
  ) where

import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import Data.Greskell.Graph
  ( PropertyMap, Property(..), allProperties
  )
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), Monoid(..), mconcat)
import qualified Data.Scientific as Sci
import Data.Semigroup (Semigroup(..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time (TimeZone(..))
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

import NetSpider.Snapshot
  ( SnapshotNode, nodeId, nodeTimestamp, isOnBoundary, nodeAttributes,
    SnapshotLink, sourceNode, destinationNode, linkTimestamp, isDirected, linkAttributes,
    SnapshotGraph
  )
import NetSpider.Timestamp
  ( Timestamp(epochTime, timeZone),
    showTimestamp
  )

-- | Node ID in GraphML.
type NodeID = Text

class ToNodeID a where
  toNodeID :: a -> NodeID

nodeIDByShow :: Show a => a -> NodeID
nodeIDByShow = pack . show

instance ToNodeID Text where
  toNodeID = id

instance ToNodeID TL.Text where
  toNodeID = TL.toStrict

instance ToNodeID String where
  toNodeID = pack

instance ToNodeID Int where
  toNodeID = nodeIDByShow

instance ToNodeID Int8 where
  toNodeID = nodeIDByShow

instance ToNodeID Int16 where
  toNodeID = nodeIDByShow

instance ToNodeID Int32 where
  toNodeID = nodeIDByShow

instance ToNodeID Int64 where
  toNodeID = nodeIDByShow

instance ToNodeID Word where
  toNodeID = nodeIDByShow

instance ToNodeID Word8 where
  toNodeID = nodeIDByShow

instance ToNodeID Word16 where
  toNodeID = nodeIDByShow

instance ToNodeID Word32 where
  toNodeID = nodeIDByShow

instance ToNodeID Integer where
  toNodeID = nodeIDByShow

instance ToNodeID Float where
  toNodeID = nodeIDByShow

instance ToNodeID Double where
  toNodeID = nodeIDByShow

instance ToNodeID Bool where
  toNodeID True = "true"
  toNodeID False = "false"

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

instance ToAttributes [(AttributeKey, AttributeValue)] where
  toAttributes = id

instance (PropertyMap m, Property p) => ToAttributes (m p AttributeValue) where
  toAttributes = toAttributes . map toPair . allProperties
    where
      toPair p = (propertyKey p, propertyValue p)

-- | 'Nothing' is mapped to empty attributes.
instance ToAttributes a => ToAttributes (Maybe a) where
  toAttributes Nothing = []
  toAttributes (Just a) = toAttributes a

-- | Make 'AttributeValue' from aeson's 'Aeson.Value'. It returns
-- 'Nothing', if the input is null, an object or an array. If the
-- input is a number, the output uses 'AttrDouble'.
valueFromAeson :: Aeson.Value -> Maybe AttributeValue
valueFromAeson v =
  case v of
    Aeson.String t -> Just $ AttrString t
    Aeson.Bool b -> Just $ AttrBoolean b
    Aeson.Number n -> Just $ AttrDouble $ Sci.toRealFloat n
    _ -> Nothing

-- | Make attributes from aeson's 'Aeson.Value'. It assumes the input
-- is an object, and its values can be converted by
-- 'valueFromAeson'. Otherwise, it returns 'Nothing'.
attributesFromAeson :: Aeson.Value -> Maybe [(AttributeKey, AttributeValue)]
attributesFromAeson v =
  case v of
    Aeson.Object o -> mapM convElem $ HM.toList o
    _ -> Nothing
  where
    convElem (k, val) = fmap ((,) k) $ valueFromAeson val

sbuild :: Show a => a -> TLB.Builder
sbuild = TLB.fromString . show

showAttributeValue :: AttributeValue -> TLB.Builder
showAttributeValue v =
  case v of
    AttrBoolean False -> "false"
    AttrBoolean True -> "true"
    AttrInt i -> sbuild i
    AttrLong i -> sbuild i
    AttrFloat f -> sbuild f
    AttrDouble d -> sbuild d
    AttrString t -> encodeXML t

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
showAttributeID index = "d" <> sbuild index

showKeyMetaWithIndex :: Int -> KeyMeta -> TLB.Builder
showKeyMetaWithIndex index km = "<key id=\"" <> id_str <> "\" for=\"" <> domain_str
                                <> "\" attr.name=\"" <> name_str <> "\" attr.type=\"" <> type_str <> "\"/>\n"
  where
    id_str = showAttributeID index
    domain_str = showDomain $ kmDomain km
    name_str = encodeXML $ kmName km
    type_str = showAttributeType $ kmType km

timestampAttrs :: Timestamp -> [(AttributeKey, AttributeValue)]
timestampAttrs t =
  [ ("@timestamp", AttrLong $ toInteger $ epochTime t),
    ("@timestamp_str", AttrString $ showTimestamp t)
  ] ++ timezone_attrs
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
    attrs = toAttributes $ nodeAttributes n
    convert (k, v) = makeMetaValue DomainNode k v

linkMetaKeys :: ToAttributes la => SnapshotLink n la -> [KeyMeta]
linkMetaKeys l = map fst $ linkMetaValues l

linkMetaValues :: ToAttributes la => SnapshotLink n la -> [(KeyMeta, AttributeValue)]
linkMetaValues l = map convert $ base <> attrs
  where
    base = timestampAttrs $ linkTimestamp l
    attrs = toAttributes $ linkAttributes l
    convert (k, v) = makeMetaValue DomainEdge k v

showAttribute :: KeyStore -> KeyMeta -> AttributeValue -> TLB.Builder
showAttribute ks km val =
  case keyIndex ks km of
    Nothing -> ""
    Just index -> "<data key=\"" <> key_id_str <> "\">" <> val_str <> "</data>\n"
      where
        key_id_str = showAttributeID index
        val_str = showAttributeValue val

-- | Options to write GraphML. Use 'defWriteOption' to get the default
-- values.
data WriteOption =
  WriteOption
  { woptDefaultDirected :: Bool
    -- ^ If 'True', set GraphML's @edgedefault@ attribute to
    -- @directed@. If 'False', set it to @undirected@. Note that
    -- regardless of this option, each @edge@ element specifies
    -- @directed@ attribute explicitly.
    --
    -- Default: 'True'
  }
  deriving (Show,Eq,Ord)

defWriteOption :: WriteOption
defWriteOption =
  WriteOption
  { woptDefaultDirected = True
  }

-- | 'writeGraphMLWith' the default options.
writeGraphML :: (ToNodeID n, ToAttributes na, ToAttributes la)
             => SnapshotGraph n na la
             -> TL.Text
writeGraphML = writeGraphMLWith defWriteOption

writeGraphMLWith :: (ToNodeID n, ToAttributes na, ToAttributes la)
                 => WriteOption
                 -> SnapshotGraph n na la
                 -> TL.Text
writeGraphMLWith wopt (input_nodes, input_links) =
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
    edgedefault_str = if woptDefaultDirected wopt
                      then "directed"
                      else "undirected"
    graph_header = "<graph edgedefault=\"" <> edgedefault_str <> "\">\n"
    graph_footer = "</graph>\n"
    nodes = mconcat $ map writeNode input_nodes
    edges = mconcat $ map writeLink input_links
    showAttribute' (k,v) = ("    " <> ) $ showAttribute key_store k v
    writeNode n = "  <node id=\"" <> (encodeNodeID $ nodeId n) <> "\">\n"
                  <> (mconcat $ map showAttribute' $ nodeMetaValues n)
                  <> "  </node>\n"
    writeLink l = "  <edge source=\"" <> (encodeNodeID $ sourceNode l)
                  <> "\" target=\"" <> (encodeNodeID $ destinationNode l)
                  <> "\" directed=\"" <> showDirected l <> "\">\n"
                  <> (mconcat $ map showAttribute' $ linkMetaValues l)
                  <> "  </edge>\n"
    showDirected l = if isDirected l
                     then "true"
                     else "false"

encodeNodeID :: ToNodeID n => n -> TLB.Builder
encodeNodeID = encodeXML . toNodeID

encodeXML :: Text -> TLB.Builder
encodeXML = mconcat . map escape . unpack
  where
    escape c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&apos;"
        '\n' -> "&#x0a;"
        '\r' -> "&#x0d;"
        _ -> TLB.singleton c
