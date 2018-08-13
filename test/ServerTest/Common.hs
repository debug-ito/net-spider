{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Common
       ( withServer,
         withSpider,
         withSpider',
         toSortedList,
         AText(..),
         AInt(..)
       ) where

import Control.Applicative ((<$>))
import Control.Exception.Safe (bracket, withException)
import Data.Greskell (parseOneValue, gProperty, newBind)
import Data.List (sort)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
import NetSpider.Spider
  ( Host, Port, Spider,
    connectWith, close, clearAll,
    Config(..), defConfig
  )

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: (Spider n la na -> IO ()) -> (Host, Port) -> IO ()
withSpider = withSpider' defConfig

withSpider' :: Config n la na -> (Spider n la na -> IO ()) -> (Host, Port) -> IO ()
withSpider' orig_conf action (host, port) = bracket (connectWith conf) close $ \spider -> do
  clearAll spider
  action spider
    where
      conf = orig_conf { wsHost = host,
                         wsPort = port
                       }

toSortedList :: Ord a => Vector a -> [a]
toSortedList = sort . V.toList



newtype AText = AText Text
              deriving (Show,Eq,Ord)

instance NodeAttributes AText where
  writeNodeAttributes (AText t) = gProperty "text" <$> newBind t
  parseNodeAttributes ps = AText <$> parseOneValue "text" ps

instance LinkAttributes AText where
  writeLinkAttributes (AText t) = gProperty "text" <$> newBind t
  parseLinkAttributes ps = AText <$> parseOneValue "text" ps

newtype AInt = AInt Int
             deriving (Show,Eq,Ord)

instance NodeAttributes AInt where
  writeNodeAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseNodeAttributes ps = AInt <$> parseOneValue "integer" ps

instance LinkAttributes AInt where
  writeLinkAttributes (AInt n) = gProperty "integer" <$> newBind n
  parseLinkAttributes ps = AInt <$> parseOneValue "integer" ps

