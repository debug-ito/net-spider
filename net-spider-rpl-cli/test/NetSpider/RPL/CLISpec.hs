module NetSpider.RPL.CLISpec (main,spec) where

import Data.Monoid (mempty)
import qualified Options.Applicative as Opt
import Net.IPv6 (ipv6)
import NetSpider.Query (startsFrom)
import NetSpider.RPL.FindingID (IPv6ID(..))
import Test.Hspec

import NetSpider.RPL.CLI (optionParser, Cmd(..))

main :: IO ()
main = hspec spec

runParser :: Opt.Parser a -> [String] -> Either String a
runParser opt args = toEither $ Opt.execParserPure prefs info args
  where
    prefs = Opt.prefs mempty
    info = Opt.info opt mempty
    toEither (Opt.Success a) = Right a
    toEither (Opt.Failure f) = Left $ show f
    toEither (Opt.CompletionInvoked c) = Left $ show c

spec :: Spec
spec = do
  describe "optionParser" $ do
    specify "--starts-from" $ do
      let parse_result = runParser optionParser ["snapshot", "-s", "fd00::212:4b00:13a4:c554"]
      case parse_result of
        Right (_, CmdSnapshot got_q) ->
          startsFrom got_q `shouldBe` [IPv6ID $ ipv6 0xfd00 0 0 0 0x0212 0x4b00 0x13a4 0xc554]
        Right (_, _) ->
          expectationFailure ("Unexpected Cmd option.")
        Left err ->
          expectationFailure (err)
      
  
