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
import NetSpider.RPL.ContikiNG (parseFile, pSyslogHead)
import qualified Pangraph.GraphML.Writer as GraphML
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


main :: IO ()
main = do
  (file : _) <- getArgs
  let phead = pSyslogHead 2019 Nothing
  (dio_nodes, dao_nodes) <- parseFile phead file
  putStrLn ("------ " <> (show $ length dio_nodes) <> " DIO Nodes")
  -- print dio_nodes
  putStrLn ("------ " <> (show $ length dao_nodes) <> " DAO Nodes")
  (snodes, slinks) <- putDIONodes dio_nodes
  putStrLn "--------- SnapshotNodes"
  print snodes
  putStrLn "--------- SnapshotLinks"
  print slinks
  graph <- makePangraphIO snodes slinks
  B.writeFile "result.graphml" $ GraphML.write graph
  

