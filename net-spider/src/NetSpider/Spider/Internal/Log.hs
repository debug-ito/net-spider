-- |
-- Module: NetSpider.Spider.Internal.Log
-- Description: Logging functions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Log
       ( runLogger,
         logDebug,
         logWarn
       ) where

import Control.Monad.Logger (LogLevel, LoggingT)
import qualified Control.Monad.Logger as Log
import Data.Text (Text)

import NetSpider.Spider.Internal.Spider (Spider(..))
import NetSpider.Spider.Config (Config(..))

runLogger :: Spider n na fla -> LoggingT IO a -> IO a
runLogger spider act = Log.runStderrLoggingT $ Log.filterLogger fil act
  where
    fil _ level = level >= (logThreshold $ spiderConfig spider)

logDebug :: Spider n na fla -> Text -> IO ()
logDebug spider msg = runLogger spider $ Log.logDebugN msg

logWarn :: Spider n na fla -> Text -> IO ()
logWarn spider msg = runLogger spider $ Log.logWarnN msg

