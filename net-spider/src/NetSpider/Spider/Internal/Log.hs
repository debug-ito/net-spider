-- |
-- Module: NetSpider.Spider.Internal.Log
-- Description: Logging functions related to Spider
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __this module is internal. End-users should not use it.__
module NetSpider.Spider.Internal.Log
       ( runLogger,
         logLine,
         logDebug,
         logWarn
       ) where

import Control.Monad.Logger (LogLevel, LoggingT)
import qualified Control.Monad.Logger as Log
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)

import NetSpider.Log (LogLine)
import NetSpider.Spider.Internal.Spider (Spider(..))
import NetSpider.Spider.Config (Config(..))

runLogger :: Spider n na fla -> LoggingT IO a -> IO a
runLogger spider act = Log.runStderrLoggingT $ Log.filterLogger fil act
  where
    fil _ level = level >= (logThreshold $ spiderConfig spider)

logLine :: Spider n na fla -> LogLine -> IO ()
logLine spider (loc, src, level, msg) = runLogger spider $ Log.monadLoggerLog loc src level msg

logDebug :: Spider n na fla -> Text -> IO ()
logDebug spider msg = runLogger spider $ Log.logDebugN msg

logWarn :: Spider n na fla -> Text -> IO ()
logWarn spider msg = runLogger spider $ Log.logWarnN msg

