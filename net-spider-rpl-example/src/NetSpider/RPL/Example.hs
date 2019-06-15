{-# LANGUAGE RankNTypes #-}
-- |
-- Module: NetSpider.RPL.Example
-- Description: Example executable of NetSpider.RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.RPL.Example
       ( main
       ) where

import qualified Data.ByteString as B
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Monoid ((<>))
import NetSpider.Input
  ( defConfig,
    connectWith, close, addFoundNode, clearAll,
    FoundNode(subjectNode)
  )
import NetSpider.Output
  ( getSnapshot,
    defQuery, unifyLinkSamples, unifyStd,
    SnapshotNode, SnapshotLink
  )
import NetSpider.Pangraph (makePangraphIO)
import NetSpider.RPL.FindingID (FindingID)
import NetSpider.RPL.DIO
  ( dioUnifierConf, FoundNodeDIO, DIONode, MergedDIOLink
  )
import NetSpider.RPL.DAO (FoundNodeDAO)
import NetSpider.RPL.ContikiNG (parseFile, pSyslogHead)
-- import qualified Pangraph.GraphML.Writer as GraphML
import qualified Pangraph.Gml.Writer as GML
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

loadFile :: (forall na la . [FoundNode FindingID na la] -> [FoundNode FindingID na la])
         -> FilePath
         -> IO ([FoundNodeDIO], [FoundNodeDAO])
loadFile selectNode file = do
  putStrLn ("------ Loading " <> file)
  (dio_nodes, dao_nodes) <- parseFile phead file
  putStrLn ("------ " <> (show $ length dio_nodes) <> " DIO Nodes")
  -- print dio_nodes
  putStrLn ("------ " <> (show $ length dao_nodes) <> " DAO Nodes")
  return (selectNode dio_nodes, selectNode dao_nodes)
  where
    phead = pSyslogHead 2019 Nothing

concatPairs :: [([a], [b])] -> ([a], [b])
concatPairs [] = ([],[])
concatPairs ((as, bs) : rest) = (as ++ rest_as, bs ++ rest_bs)
  where
    (rest_as, rest_bs) = concatPairs rest

main :: IO ()
main = do
  filenames <- getArgs
  (dio_nodes, _) <- fmap concatPairs $ mapM (loadFile id) filenames
  (snodes, slinks) <- putDIONodes dio_nodes
  putStrLn "--------- SnapshotNodes"
  print snodes
  putStrLn "--------- SnapshotLinks"
  print slinks
  graph <- makePangraphIO snodes slinks
  B.writeFile "result.gml" $ GML.write graph
  

