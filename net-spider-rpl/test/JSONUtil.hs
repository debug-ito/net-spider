module JSONUtil
  ( specJSONFromTo
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import Test.Hspec

specJSONFromTo :: (FromJSON a, ToJSON a, Show a, Eq a)
               => String -- ^ spec name
               -> BSL.ByteString -- ^ JSON document
               -> a -- ^ expected data
               -> Spec
specJSONFromTo spec_name json_doc exp_data = do
  specify ("JSONFromTo: " ++ spec_name) $ do
    let got_dec = Aeson.eitherDecode json_doc
        got_enc_v :: Either String Aeson.Value
        got_enc_v = Aeson.eitherDecode $ Aeson.encode exp_data
        exp_enc = Aeson.eitherDecode json_doc
    got_dec `shouldBe` Right exp_data
    isRight exp_enc `shouldBe` True
    got_enc_v `shouldBe` exp_enc
