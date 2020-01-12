{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.DAOSpec (main,spec) where

import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.RPL.DAO (DAONode(..), DAOLink(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DAONode" $ do
    specJSONFromTo
      "non-empty daoRouteNum"
      "{\"dao_route_num\": 25}"
      DAONode { daoRouteNum = Just 25 }
  describe "DAOLink" $ do
    specJSONFromTo
      "normal"
      "{\"path_lifetime_sec\": 2560}"
      (DAOLink 2560)
