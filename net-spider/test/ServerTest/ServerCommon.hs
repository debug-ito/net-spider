module ServerTest.ServerCommon
  ( withServer,
    withSpider,
    withSpider'
  ) where

import Control.Exception.Safe (bracket)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Spider
  ( connectWith, close, clearAll, Spider
  )
import NetSpider.Spider.Config
  ( Host, Port, Config(..), defConfig, LogLevel(..)
  )


withServer :: SpecWith (Host,Port) -> Spec
withServer = before $ needEnvHostPort Need "NET_SPIDER_TEST"

withSpider :: Eq n => (Spider n na fla -> IO ()) -> (Host, Port) -> IO ()
withSpider = withSpider' $ defConfig { logThreshold = LevelWarn }

withSpider' :: Config n na fla -> (Spider n na fla -> IO ()) -> (Host, Port) -> IO ()
withSpider' orig_conf action (host, port) = bracket (connectWith conf) close $ \spider -> do
  clearAll spider
  action spider
    where
      conf = orig_conf { wsHost = host,
                         wsPort = port
                       }


