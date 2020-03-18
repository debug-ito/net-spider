{-# LANGUAGE OverloadedStrings #-}
module NetSpider.WeaverSpec (main,spec) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Query
  ( policyOverwrite, policyAppend,
    foundNodePolicy, unifyLinkSamples
  )
import NetSpider.Timestamp (fromEpochMillisecond)
import NetSpider.Weaver
  ( Weaver, newWeaver,
    markAsVisited, addFoundNode,
    isVisited, getFoundNodes, getSnapshot,
    visitAllBoundaryNodes,
    getBoundaryNodes
  )

import SnapshotTestCase (SnapshotTestCase(..))
import qualified SnapshotTestCase

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Weaver" $ do
  spec_visitedNodes
  spec_boundaryNodes
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

shouldReturnSet :: (HasCallStack, Eq a, Show a) => IO [a] -> [a] -> IO ()
shouldReturnSet action expected = do
  got <- action
  got `shouldMatchList` expected

spec_boundaryNodes :: Spec
spec_boundaryNodes = describe "boundary nodes" $ do
  let addFN ref_w fn = do
        new_w <- fmap (addFoundNode fn) $ readIORef ref_w
        writeIORef ref_w new_w
      getBN ref_w = fmap getBoundaryNodes $ readIORef ref_w
      makeFN :: Text -> [Text] -> Int64 -> FoundNode Text () ()
      makeFN sub_n tar_ns time =
        FoundNode { subjectNode = sub_n,
                    foundAt = fromEpochMillisecond time,
                    nodeAttributes = (),
                    neighborLinks = map makeFL tar_ns
                  }
      makeFL tar_n = FoundLink { targetNode = tar_n, linkState = LinkToTarget, linkAttributes = () }
  specify "policyOverwrite" $ do
    rweaver <- newIORef $ newWeaver policyOverwrite
    let addFN' = addFN rweaver
        getBN' = getBN rweaver
    getBN' `shouldReturnSet` []
    addFN' (makeFN "n1" [] 100)
    getBN' `shouldReturnSet` []
    addFN' (makeFN "n1" ["n2"] 200)
    getBN' `shouldReturnSet` ["n2"]
    addFN' (makeFN "n2" ["n3", "n4", "n5", "n1"] 250)
    getBN' `shouldReturnSet` ["n3", "n4", "n5"]
    addFN' (makeFN "n3" ["n4"] 200)
    getBN' `shouldReturnSet` ["n4", "n5"] -- should return unique node IDs
    
    addFN' (makeFN "n5" ["n1", "n6", "n7", "n8", "n2"] 200)
    getBN' `shouldReturnSet` ["n4", "n6", "n7", "n8"]
  
    -- TODO: keep testing boundary nodes.
    -- - What if a new FoundNode overwrites the old one for the same node ID?
    -- - What if an old FoundNode is discarded due to the new one already in the weaver?
    -- - How do boundary nodes respond to markAsVisited?
  
  

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
  
