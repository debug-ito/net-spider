module NetSpider.UtilSpec (main,spec) where

import Data.Monoid (mempty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec

import NetSpider.Util (groupWith)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "groupWith" $ do
    specify "empty" $ do
      let input :: Vector Int
          input = mempty
      groupWith id input `shouldBe` mempty
    specify "single group" $ do
      let input :: Vector Int
          input = V.fromList [1,2,3,4,5]
      groupWith (const 'a') input `shouldBe` return input
    it "should preserve element order within group" $ do
      let input :: Vector Int
          input = V.fromList [3, 5, 1, 0, 9, 4, 3, 7, 4]
          expected = map V.fromList [ [3, 0, 9, 3],
                                      [5],
                                      [1,4,7,4]
                                    ]
      (V.toList $ groupWith (`mod` 3) input) `shouldMatchList` expected
