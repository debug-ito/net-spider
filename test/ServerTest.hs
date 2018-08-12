{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Test.Hspec

import qualified ServerTest.Snapshot as Snapshot
import qualified ServerTest.Attributes as Attributes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Snapshot.spec
  Attributes.spec
