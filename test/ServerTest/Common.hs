module ServerTest.Common
       ( withServer,
         withSpider,
         withSpider',
         toSortedList
       ) where

import Control.Exception.Safe (bracket, withException)
import Data.List (sort)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

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
