module NetSpider.CLI.SpiderSpec (main,spec) where

import Data.Monoid (mempty)
import NetSpider.Spider.Config (Config(..))
import Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import Test.Hspec

import NetSpider.CLI.Spider (parserSpiderConfig)

main :: IO ()
main = hspec spec

runP :: Parser a -> [String] -> Either String a
runP p args = toEither $ Opt.execParserPure prefs pinfo args
  where
    prefs = Opt.prefs mempty
    pinfo = Opt.info p mempty
    toEither (Opt.Success a) = Right a
    toEither (Opt.Failure f) = Left $ show f
    toEither (Opt.CompletionInvoked c) = Left $ show c

spec :: Spec
spec = describe "parserSpiderConfig" $ do
  specify "host and port" $ do
    let (Right sconf) = runP parserSpiderConfig
                        [ "--host", "foo.example.com",
                          "--port", "18822",
                          "--node-id-key", "@foo"
                        ]
    wsHost sconf `shouldBe` "foo.example.com"
    wsPort sconf `shouldBe` 18822
    nodeIdKey sconf `shouldBe` "@foo"
