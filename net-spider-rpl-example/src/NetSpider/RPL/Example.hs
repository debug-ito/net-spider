{-# LANGUAGE RankNTypes #-}
-- |
-- Module: NetSpider.RPL.Example
-- Description: Example executable of NetSpider.RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.RPL.Example
       ( main
       ) where

import qualified Data.Text.Lazy.IO as TLIO
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.List (sortOn)
import Data.Monoid ((<>))
import NetSpider.GraphML.Writer (writeGraphML)
import NetSpider.Input
  ( defConfig,
    connectWith, close, addFoundNode, clearAll,
    FoundNode(subjectNode, foundAt)
  )
import NetSpider.Output
  ( getSnapshot,
    defQuery, unifyLinkSamples, unifyStd,
    SnapshotNode, SnapshotLink
  )
import NetSpider.RPL.FindingID (FindingID)
import NetSpider.RPL.DIO
  ( dioUnifierConf, FoundNodeDIO, DIONode, MergedDIOLink
  )
import NetSpider.RPL.DAO (FoundNodeDAO)
import NetSpider.RPL.ContikiNG (parseFile, pSyslogHead)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

putDIONodes :: [FoundNodeDIO] -> IO ([SnapshotNode FindingID DIONode], [SnapshotLink FindingID MergedDIOLink])
putDIONodes dio_nodes = do
  let conf = defConfig
      start_id = subjectNode (dio_nodes !! 0)
      query = (defQuery [start_id])
              { unifyLinkSamples = unifyStd dioUnifierConf
              }
  bracket (connectWith conf) close $ \sp -> do
    clearAll sp
    forM_ (zip dio_nodes ([0 ..] :: [Integer])) $ \(dio_node, index) -> do
      when ((index `mod` 100) == 0) $ hPutStrLn stderr ("Add DIO node [" <> show index <> "]")
      addFoundNode sp dio_node
    getSnapshot sp query

loadFile :: FilePath
         -> IO ([FoundNodeDIO], [FoundNodeDAO])
loadFile file = do
  putStrLn ("------ Loading " <> file)
  (dio_nodes, dao_nodes) <- parseFile phead file
  putStrLn ("------ " <> (show $ length dio_nodes) <> " DIO Nodes")
  -- print dio_nodes
  putStrLn ("------ " <> (show $ length dao_nodes) <> " DAO Nodes")
  return (dio_nodes, dao_nodes)
  where
    phead = pSyslogHead 2019 Nothing

filterPairs :: (forall na la . [FoundNode FindingID na la] -> [FoundNode FindingID na la])
            -> ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
            -> ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
filterPairs f (ns1, ns2) = (f ns1, f ns2)

concatPairs :: [([a], [b])] -> ([a], [b])
concatPairs [] = ([],[])
concatPairs ((as, bs) : rest) = (as ++ rest_as, bs ++ rest_bs)
  where
    (rest_as, rest_bs) = concatPairs rest

main :: IO ()
main = do
  filenames <- getArgs
  (dio_nodes, _) <- fmap (concatPairs . map (filterPairs getHead)) $ mapM loadFile filenames
  (snodes, slinks) <- putDIONodes dio_nodes
  putStrLn "--------- SnapshotNodes"
  print snodes
  putStrLn "--------- SnapshotLinks"
  print slinks
  TLIO.writeFile "result.graphml" $ writeGraphML snodes slinks
    where
      getHead nodes = case reverse $ sortOn foundAt nodes of
        [] -> []
        (x : _) -> [x]
