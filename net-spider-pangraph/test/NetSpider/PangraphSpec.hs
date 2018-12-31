{-# LANGUAGE OverloadedStrings #-}
module NetSpider.PangraphSpec (main, spec) where

import Data.List (sort)
import Data.Text (Text)
import NetSpider.Snapshot.Internal (SnapshotNode(..))
import NetSpider.Timestamp (fromS)
import qualified Pangraph as P
import Test.Hspec

import NetSpider.Pangraph (makeVertex)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeVertex" $ do
    specify "text ID, empty attriubutes" $ do
      let sn :: SnapshotNode Text ()
          sn = SnapshotNode { _nodeId = "node ID",
                              _isOnBoundary = False,
                              _nodeTimestamp = Just $ fromS "2018-03-22T09:00:00+09:00",
                              _nodeAttributes = Just ()
                            }
          got = makeVertex sn
      P.vertexID got `shouldBe` "node ID"
      (sort $ P.vertexAttributes got)
        `shouldBe` [ ("@is_on_boundary", "False"),
                     ("@timestamp", "1521676800000"),
                     ("@tz_name", ""),
                     ("@tz_offset_min", "540"),
                     ("@tz_summary_only", "False")
                   ]
