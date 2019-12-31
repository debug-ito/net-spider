module NetSpider.TimestampSpec (main,spec) where

import Data.Aeson (encode, decode)
import Test.Hspec

import NetSpider.Timestamp (Timestamp, fromS)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FromJSON and ToJSON" $ do
  specTS $ fromS "2019-12-31T18:46"
  specTS $ fromS "2019-12-31T18:46:10"
  specTS $ fromS "2019-12-31 18:46:11.037"
  specTS $ fromS "2019-09-21T00:32Z"
  specTS $ fromS "2019-08-07 11:18:43+07:00"
  specTS $ fromS "2019-08-07T11:18:43.112-02:30"


specTS :: Timestamp -> Spec
specTS t = specify (show t) $ do
  (decode $ encode t) `shouldBe` Just t
