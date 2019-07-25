module NetSpider.CLI.SpiderSpec (main,spec) where

import Data.Monoid (mempty)
import NetSpider.Spider.Config (Config(..), LogLevel(..))
import Test.Hspec

import NetSpider.CLI.Spider (parserSpiderConfig)

import NetSpider.CLI.TestCommon (runP)

main :: IO ()
main = hspec spec

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
  specify "verbosity" $ do
    let (Right warn) = runP parserSpiderConfig []
        (Right info) = runP parserSpiderConfig ["-v"]
        (Right debug) = runP parserSpiderConfig ["-vv"]
    logThreshold warn `shouldBe` LevelWarn
    logThreshold info `shouldBe` LevelInfo
    logThreshold debug `shouldBe` LevelDebug
