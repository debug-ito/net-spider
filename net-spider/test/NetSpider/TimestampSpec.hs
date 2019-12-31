module NetSpider.TimestampSpec (main,spec) where

import Test.Hspec

import NetSpider.Timestamp (Timestamp)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FromJSON and ToJSON" $ do
  specTS undefined -- TODO


specTS :: Timestamp -> Spec
specTS t = specify (show t) $ do
  True `shouldBe` False -- TODO
