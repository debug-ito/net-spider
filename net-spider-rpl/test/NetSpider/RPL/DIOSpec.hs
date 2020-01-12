{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.DIOSpec (main,spec) where

import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.RPL.DIO
  ( DIONode(..), NeighborType(..), DIOLink(..), MergedDIOLink(..)
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
    specJSONFromTo
      "DIOLink with non-empty metric"
      "{\"neighbor_type\": \"parent_candidate\", \"neighbor_rank\": 590, \"metric\": 132}"
      DIOLink
      { neighborType = ParentCandidate,
        neighborRank = 590,
        metric = Just 132
      }
  describe "MergedDIOLink" $ do
    specJSONFromTo
      "MergedDIOLink with non-empty fromDest"
      (    "{\"from_source\": {\"neighbor_type\": \"preferred_parent\", \"neighbor_rank\": 590, \"metric\": 132},"
        <> " \"from_dest\": {\"neighbor_type\": \"other_neighbor\", \"neighbor_rank\": 722, \"metric\": null}}"
      )
      MergedDIOLink
      { fromSource = DIOLink PreferredParent 590 (Just 132),
        fromDest = Just $ DIOLink OtherNeighbor 722 Nothing
      }
