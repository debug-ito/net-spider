module NetSpider.WeaverSpec (main,spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Weaver" $ do
  specify "mark and add" $ True `shouldBe` False -- TODO
  specify "add and mark" $ True `shouldBe` False -- TODO
  specify "policyOverwrite" $ True `shouldBe` False -- TODO
  specify "policyAppend" $ True `shouldBe` False -- TODO
    
