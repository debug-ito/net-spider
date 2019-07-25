-- |
-- Module: NetSpider.CLI.TestCommon
-- Description: common utils for test
-- Maintainer: Toshio Ito <toshio9.ito@toshiba.co.jp>
--
-- 
module NetSpider.CLI.TestCommon
  ( runP
  ) where

import qualified Options.Applicative as Opt

runP :: Opt.Parser a -> [String] -> Either String a
runP p args = toEither $ Opt.execParserPure prefs pinfo args
  where
    prefs = Opt.prefs mempty
    pinfo = Opt.info p mempty
    toEither (Opt.Success a) = Right a
    toEither (Opt.Failure f) = Left $ show f
    toEither (Opt.CompletionInvoked c) = Left $ show c

