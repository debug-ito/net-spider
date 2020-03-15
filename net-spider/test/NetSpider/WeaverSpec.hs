module NetSpider.WeaverSpec (main,spec) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Test.Hspec

import NetSpider.Found (FoundNode(..))
import NetSpider.Query
  ( policyOverwrite, policyAppend,
    foundNodePolicy, unifyLinkSamples
  )
import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Weaver
  ( Weaver, newWeaver,
    markAsVisited, addFoundNode,
    isVisited, getFoundNodes, getSnapshot,
    visitAllBoundaryNodes
  )

import SnapshotTestCase (SnapshotTestCase(..))
import qualified SnapshotTestCase

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Weaver" $ do
  spec_visitedNodes
  describe "SnapshotTestCase" $ do
    mapM_ makeTestCase SnapshotTestCase.basics
  
spec_visitedNodes :: Spec
spec_visitedNodes = describe "visited nodes" $ do
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
    getFoundNodes 1 w `shouldBe` Nothing
    getFoundNodes 5 w `shouldBe` Just [fn]
    getFoundNodes 10 w `shouldBe` Just []
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
    getFoundNodes 1 w `shouldBe` Nothing
    getFoundNodes 5 w `shouldBe` Just [fn]
    getFoundNodes 10 w `shouldBe` Just []
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
      (getFoundNodes 5 $ addFoundNode fn1 w)
        `shouldBe` Just [fn1]
      (getFoundNodes 5 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (getFoundNodes 5 $ addFoundNode fn1 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (getFoundNodes 5 $ addFoundNode fn2 $ addFoundNode fn1 w)
        `shouldBe` Just [fn2]
    specify "policyAppend should keep all FoundNodes" $ do
      let w :: Weaver Int String ()
          w = newWeaver policyAppend
      (getFoundNodes 5 $ addFoundNode fn1 w)
        `shouldBe` Just [fn1]
      (getFoundNodes 5 $ addFoundNode fn2 w)
        `shouldBe` Just [fn2]
      (fromJust $ getFoundNodes 5 $ addFoundNode fn1 $ addFoundNode fn2 w)
        `shouldMatchList` [fn1, fn2]
      (fromJust $ getFoundNodes 5 $ addFoundNode fn2 $ addFoundNode fn1 w)
        `shouldMatchList` [fn1, fn2]

addAllFoundNodes :: (Eq n, Hashable n) => [FoundNode n na la] -> Weaver n na la -> Weaver n na la
addAllFoundNodes fns weaver = foldl' (\w fn -> addFoundNode fn w) weaver fns

makeTestCase :: SnapshotTestCase -> Spec
makeTestCase SnapshotTestCase { caseName = name, caseInput = input, caseQuery = query, caseAssert = assert } = do
  specify name $ do
    let init_w = newWeaver $ foundNodePolicy query
        got_w = visitAllBoundaryNodes  -- SnapshotTestCase assumes unlimited number of traverse steps. So, mark all nodes as visited.
                $ addAllFoundNodes input init_w
        got_graph = getSnapshot (unifyLinkSamples query) got_w
    assert got_graph
  
