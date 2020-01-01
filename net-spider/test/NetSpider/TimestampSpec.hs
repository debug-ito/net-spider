{-# LANGUAGE OverloadedStrings #-}
module NetSpider.TimestampSpec (main,spec) where

import Data.Aeson (encode, eitherDecode, ToJSON(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Time (TimeZone(..))
import Test.Hspec

import NetSpider.Timestamp (Timestamp(..), fromS, fromEpochMillisecond)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FromJSON and ToJSON" $ do
    specTS $ fromS "2019-12-31T18:46"
    specTS $ fromS "2019-12-31T18:46:10"
    specTS $ fromS "2019-12-31 18:46:11.037"
    specTS $ fromS "2019-09-21T00:32Z"
    specTS $ fromS "2019-08-07 11:18:43+07:00"
    specTS $ fromS "2019-08-07T11:18:43.112-02:30"
  describe "ToJSON" $ do
    specToJSON (fromEpochMillisecond 1000) "{\"epoch_time\":1000}"
    specToJSON (fromS "2019-09-21T00:32Z")
      "{\"epoch_time\":1569025920000, \"tz_offset_min\": 0, \"tz_summer_only\":false, \"tz_name\":\"UTC\"}"
    specToJSON (fromS "2019-08-07 11:18:43+07:00")
      "{\"epoch_time\":1565151523000, \"tz_offset_min\": 420, \"tz_summer_only\": false, \"tz_name\":\"\"}"
  describe "FromJSON - string" $ do
    specFromJSON "\"2019-12-31 18:46:11.037\"" (Timestamp 1577817971037 Nothing)
    specFromJSON "\"2020-08-07T11:18:43.112-02:30\"" (Timestamp 1596808123112 $ Just $ TimeZone (-150) False "")
  describe "FromJSON - object" $ do
    specFromJSON "{\"epoch_time\": 1000}" (Timestamp 1000 Nothing)
    specFromJSON "{\"epoch_time\": 1000, \"tz_offset_min\": 180, \"tz_summer_only\": true, \"tz_name\": \"\"}"
      (Timestamp 1000 $ Just $ TimeZone 180 True "")
    specFromJSON "{\"epoch_time\": 1000, \"tz_offset_min\": 0, \"tz_summer_only\": false, \"tz_name\": \"UTC\"}"
      (Timestamp 1000 $ Just $ TimeZone 0 False "UTC")
    

specTS :: Timestamp -> Spec
specTS t = specify (show t) $ do
  (eitherDecode $ encode t) `shouldBe` Right t

specToJSON :: Timestamp -> BSL.ByteString -> Spec
specToJSON input_ts expected = do
  specify (show input_ts) $ do
    (Right $ toJSON input_ts) `shouldBe` (eitherDecode expected)

specFromJSON :: BSL.ByteString -> Timestamp -> Spec
specFromJSON input expected = do
  specify (show input) $ do
    eitherDecode input `shouldBe` Right expected
