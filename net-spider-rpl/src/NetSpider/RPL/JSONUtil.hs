-- |
-- Module: NetSpider.RPL.JSONUtil
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. End-users should not use it.__
module NetSpider.RPL.JSONUtil
  ( optSnake,
    optCombinedNode,
    optCombinedLink
  ) where

import Control.Applicative ((*>), (<*), (<|>))
import qualified Data.Aeson as Aeson
import Data.Char (isUpper, toLower)
import qualified Text.Regex.Applicative as RE

toSnake :: String -> String
toSnake = RE.replace $ RE.msym (\c -> if isUpper c then Just ['_', toLower c] else Nothing)

optSnake :: Aeson.Options
optSnake = Aeson.defaultOptions { Aeson.fieldLabelModifier = toSnake }

optCombinedNode :: Aeson.Options
optCombinedNode = Aeson.defaultOptions { Aeson.fieldLabelModifier = modifier }
  where
    modifier = map toLower . RE.replace (fmap (const "") $ RE.string "attrs")

optCombinedLink :: Aeson.Options
optCombinedLink =
  Aeson.defaultOptions
  { Aeson.constructorTagModifier = tag_modifier,
    Aeson.sumEncoding =
      Aeson.TaggedObject
      { Aeson.tagFieldName = "link_type",
        Aeson.contentsFieldName = "link"
      }
  }
  where
    tag_modifier = RE.replace regex
    regex = fmap (map toLower) (RE.string "Combined" *> (RE.string "DIO" <|> RE.string "DAO") <* RE.string "Link")
