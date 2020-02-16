module NetSpider.WeaverSpec (main,spec) where

import Data.Maybe (fromJust)
import Test.Hspec

import NetSpider.Found (FoundNode(..))
import NetSpider.Query (policyOverwrite, policyAppend)
import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Weaver
  ( Weaver, newWeaver,
    markAsVisited, addFoundNode,
    isVisited, getVisitedNodes
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Weaver" $ do
  specify "mark and add" $ do
    let fn = FoundNode { subjectNode = 5,
                         foundAt = fromEpochMillisecond 100,
                         neighborLinks = [],
                         nodeAttributes = ()
                       }
        w :: Weaver Int () ()
        w = addFoundNode fn $ markAsVisited 10 $ markAsVisited 5 $ newWeaver policyOverwrite
    isVisited 1 w `shouldBe` False
    isVisited 5 w `shouldBe` True
    isVisited 10 w `shouldBe` True
    getVisitedNodes 1 w `shouldBe` Nothing
    getVisitedNodes 5 w `shouldBe` Just [fn]
    getVisitedNodes 10 w `shouldBe` Just []
  specify "add and mark" $ do
    let fn = FoundNode { subjectNode = 5,
                         foundAt = fromEpochMillisecond 100,
                         neighborLinks = [],
                         nodeAttributes = ()
                       }
        w :: Weaver Int () ()
        w = markAsVisited 10 $ markAsVisited 5 $ addFoundNode fn $ newWeaver policyOverwrite
    isVisited 1 w `shouldBe` False
    isVisited 5 w `shouldBe` True
    isVisited 10 w `shouldBe` True
    getVisitedNodes 1 w `shouldBe` Nothing
    getVisitedNodes 5 w `shouldBe` Just [fn]
    getVisitedNodes 10 w `shouldBe` Just []
  describe "FoundNodePolicy for Weaver" $ do
    let fn1 = FoundNode { subjectNode = 5,
                          foundAt = fromEpochMillisecond 100,
                          neighborLinks = [],
                          nodeAttributes = "foobar"
                        }
        fn2 = FoundNode { subjectNode = 5,
                          foundAt = fromEpochMillisecond 150,
                          neighborLinks = [],
                          nodeAttributes = "quux"
                        }
    specify "policyOverwrite should keep only the latest FoundNode" $ do
      let w :: Weaver Int String ()
          w = newWeaver policyOverwrite
      (getVisitedNodes 5 $ addFoundNode fn1 w)
        `shouldBe` Just [fn1]
      (getVisitedNodes 5 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (getVisitedNodes 5 $ addFoundNode fn1 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (getVisitedNodes 5 $ addFoundNode fn2 $ addFoundNode fn1 w)
        `shouldBe` Just [fn2]
    specify "policyAppend should keep all FoundNodes" $ do
      let w :: Weaver Int String ()
          w = newWeaver policyAppend
      (getVisitedNodes 5 $ addFoundNode fn1 w)
        `shouldBe` Just [fn1]
      (getVisitedNodes 5 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (fromJust $ getVisitedNodes 5 $ addFoundNode fn1 $ addFoundNode fn2 w)
        `shouldMatchList` [fn1, fn2]
      (fromJust $ getVisitedNodes 5 $ addFoundNode fn2 $ addFoundNode fn1 w)
        `shouldMatchList` [fn1, fn2]
