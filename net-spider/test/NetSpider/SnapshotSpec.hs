module NetSpider.SnapshotSpec (main,spec) where

import Test.Hspec

import NetSpider.Snapshot.Internal (SnapshotLink(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SnapshotLink" $ do
    specify "sample" $ True `shouldBe` False -- TODO
