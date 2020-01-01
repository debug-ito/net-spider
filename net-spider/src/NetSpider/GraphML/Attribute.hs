{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
-- |
-- Module: NetSpider.GraphML.Attribute
-- Description: GraphML attribute types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module NetSpider.GraphML.Attribute
  ( AttributeKey,
    AttributeValue(..),
    ToAttributes(..),
    valueFromAeson,
    attributesFromAeson,
    attributesToAeson
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Sci
import Data.Text (Text, pack)
import Data.Time (TimeZone(..))

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

-- | Based on 'valueFromAeson'.
--
-- @since 0.4.1.0
instance FromJSON AttributeValue where
  parseJSON v = maybe empty return $ valueFromAeson v

-- | @since 0.4.1.0
instance ToJSON AttributeValue where
  toJSON v =
    case v of
      AttrBoolean b -> toJSON b
      AttrInt i -> toJSON i
      AttrLong l -> toJSON l
      AttrFloat f -> toJSON f
      AttrDouble d -> toJSON d
      AttrString t -> toJSON t

-- | Type that can be converted to list of attributes.
class ToAttributes a where
  toAttributes :: a -> [(AttributeKey, AttributeValue)]

instance ToAttributes () where
  toAttributes _ = []

instance ToAttributes [(AttributeKey, AttributeValue)] where
  toAttributes = id

-- | 'Nothing' is mapped to empty attributes.
instance ToAttributes a => ToAttributes (Maybe a) where
  toAttributes Nothing = []
  toAttributes (Just a) = toAttributes a

-- | @since 0.4.1.0
instance ToAttributes TimeZone where
  toAttributes tz =
    [ ("@tz_offset_min", AttrInt $ timeZoneMinutes tz),
      ("@tz_summer_only", AttrBoolean $ timeZoneSummerOnly tz),
      ("@tz_name", AttrString $ pack $ timeZoneName tz)
    ]

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

-- | Make aeson 'Aeson.Object' as 'Aeson.Value' from attributes.
--
-- @since 0.4.1.0
attributesToAeson :: [(AttributeKey, AttributeValue)] -> Aeson.Value
attributesToAeson = Aeson.Object . HM.fromList . (fmap . fmap) toJSON
