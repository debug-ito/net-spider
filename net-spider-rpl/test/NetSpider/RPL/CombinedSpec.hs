{-# LANGUAGE OverloadedStrings #-}
module NetSpider.RPL.CombinedSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import JSONUtil (specJSONFromTo)

import NetSpider.RPL.DIO (DIONode(..), MergedDIOLink(..), DIOLink(..), NeighborType(..))
import NetSpider.RPL.DAO (DAONode(..), DAOLink(..))
import NetSpider.RPL.Combined (CombinedNode(..), CombinedLink(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CombinedNode" $ do
    specJSONFromTo
      "non-empty DIO and DAO attributes"
      "{\"dio\": {\"rank\": 256, \"dio_interval\": 12}, \"dao\": {\"dao_route_num\": null}}"
      CombinedNode
      { attrsDIO = Just $ DIONode { rank = 256, dioInterval = 12 },
        attrsDAO = Just $ DAONode { daoRouteNum = Nothing }
      }
  describe "CombinedLink" $ do
    specJSONFromTo
      "case DIOLink"
      (    "{\"type\": \"dio\", \"link\": "
        <> "  {\"from_dest\": null, \"from_source\": {\"neighbor_type\": \"preferred_parent\", \"neighbor_rank\": 332, \"metric\": 183}}}"
      )
      ( CombinedDIOLink $
        MergedDIOLink
        { fromDest = Nothing,
          fromSource =
            DIOLink
            { neighborType = PreferredParent,
              neighborRank = 32,
              metric = Just 183
            }
        }
      )
    specJSONFromTo
      "case DAOLink"
      (    "{\"type\": \"dao\", \"link\": "
        <> "  {\"path_lifetime_sec\": 1990}}"
      )
      ( CombinedDAOLink $
        DAOLink
        { pathLifetimeSec = 1990
        }
      )
      
