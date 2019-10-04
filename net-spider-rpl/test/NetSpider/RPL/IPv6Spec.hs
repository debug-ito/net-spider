module NetSpider.RPL.IPv6Spec (main,spec) where

import Net.IPv6 (ipv6)
import Test.Hspec

import NetSpider.RPL.IPv6 (getPrefix, setPrefix)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "prefix" $ do
  let input = ipv6 0xfe00 0x0012 0x4310 0x0020 0xaa9d 0xd393 0xaa00 0x49be
  specify "getPrefix" $ do
    getPrefix input `shouldBe` 0xfe00001243100020
  specify "setPrefix" $ do
    let prefix = 0xf055aa0d00329391
        expected = ipv6 0xf055 0xaa0d 0x0032 0x9391 0xaa9d 0xd393 0xaa00 0x49be
    setPrefix prefix input `shouldBe` expected
