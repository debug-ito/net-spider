module ServerTest.Common
       ( withServer,
         withSpider,
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
    connectWS, close, clearAll
  )

withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: (Spider n la na -> IO ()) -> (Host, Port) -> IO ()
withSpider action (host, port) = bracket (connectWS host port) close $ \spider -> do
  clearAll spider
  action spider

toSortedList :: Ord a => Vector a -> [a]
toSortedList = sort . V.toList
