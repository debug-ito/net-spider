{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Test.Hspec

import qualified ServerTest.Snapshot as Snapshot

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Snapshot.spec
