{-# LANGUAGE OverloadedStrings #-}
module NetSpider.FoundSpec (main,spec) where

import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.Text (Text)
import Test.Hspec

import NetSpider.Found (FoundLink(..), LinkState(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FoundLink" $ do
    specify "FromJSON and ToJSON" $ do
      let input = "{\"target_node\":\"target\", \"link_state\": \"to_target\", \"link_attrs\":125}"
          expected :: FoundLink Text Int
          expected = FoundLink
                     { targetNode = "target",
                       linkState = LinkToTarget,
                       linkAttributes = 125
                     }
          got_dec = Aeson.eitherDecode input
          got_enc_v :: Either String Aeson.Value
          got_enc_v = Aeson.eitherDecode $ Aeson.encode expected
          exp_enc = Aeson.eitherDecode input
      got_dec `shouldBe` Right expected
      isRight exp_enc `shouldBe` True
      got_enc_v `shouldBe` exp_enc
      
