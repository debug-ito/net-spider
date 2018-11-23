-- |
-- Module: NetSpider.Log
-- Description: Types and functions for basic logging
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module is rather for internal use, but end-users can use it,
-- e.g. to implement the unifier.
--
-- @since 0.2.0.0
module NetSpider.Log
       ( LogLine,
         WriterLoggingM,
         runWriterLoggingM,
         logDebugW,
         spack
       ) where

import qualified Control.Monad.Logger as Log
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text, pack)

type LogLine = (Log.Loc, Log.LogSource, Log.LogLevel, Log.LogStr)

type WriterLoggingM = Log.WriterLoggingT Identity

runWriterLoggingM :: WriterLoggingM a -> (a, [LogLine])
runWriterLoggingM = runIdentity . Log.runWriterLoggingT

logDebugW :: Text -> WriterLoggingM ()
logDebugW = Log.logDebugN

spack :: Show a => a -> Text
spack = pack . show

