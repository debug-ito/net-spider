{-# LANGUAGE OverloadedStrings #-}
module NetSpider.GraphML.WriterSpec (main,spec) where

import Data.Text (Text)
import NetSpider.Snapshot.Internal
  ( SnapshotNode(..), SnapshotLink(..)
  )
import Test.Hspec

import NetSpider.GraphML.Writer (writeGraphML)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "writeGraphML" $ do
    specify "no attribute" $ do
      let nodes :: [SnapshotNode Text ()]
          nodes = undefined
          links :: [SnapshotLink Text ()]
          links = undefined
          expected = ""
      writeGraphML nodes links `shouldBe` expected
