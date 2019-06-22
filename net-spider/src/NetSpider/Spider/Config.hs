{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.Spider.Config
-- Description: Configuration of Spider
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module NetSpider.Spider.Config
       ( Config(..),
         defConfig,
         Host,
         Port,
         LogLevel(..)
       ) where

import Control.Monad.Logger (LogLevel(..))
import Data.Greskell (Key)
import Data.Text (Text)
import Network.Greskell.WebSocket (Host, Port)

import qualified Network.Greskell.WebSocket as Gr

import NetSpider.Graph (VNode)

-- | Configuration to create a 'Spider' object.
data Config =
  Config
  { wsHost :: Gr.Host,
    -- ^ Host of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: \"localhost\".
    wsPort :: Gr.Port,
    -- ^ Port of WebSocket endpoint of Tinkerpop Gremlin
    -- Server. Default: 8182
    nodeIdKey :: Text,
    -- ^ Name of vertex property that stores the node ID. Default:
    -- \"@node_id\".
    logThreshold :: LogLevel
    -- ^ Logs with the level higher than or equal to this threshold
    -- are printed. Default: 'LevelWarn'.
    --
    -- @since 0.2.0.0
  }

defConfig :: Config
defConfig =
  Config
  { wsHost = "localhost",
    wsPort = 8182,
    nodeIdKey = "@node_id",
    logThreshold = LevelWarn
  }


