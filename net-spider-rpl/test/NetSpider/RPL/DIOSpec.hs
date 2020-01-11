{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.DIOSpec (main,spec) where

import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.RPL.DIO
  ( DIONode(..), NeighborType(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DIONode" $ do
    specJSONFromTo
      "DIONode"
      "{\"rank\": 355, \"dio_interval\": 5}"
      (DIONode 355 5)
  describe "NeighborType" $ do
    specJSONFromTo "PreferredParent" "\"preferred_parent\"" PreferredParent
    specJSONFromTo "ParentCandidate" "\"parent_candidate\"" ParentCandidate
    specJSONFromTo "OtherNeighbor" "\"other_neighbor\"" OtherNeighbor
  describe "DIOLink" $ do
    specify "TODO" (True `shouldBe` False) -- TODO
  describe "MergedDIOLink" $ do
    specify "TODO" (True `shouldBe` False) -- TODO
