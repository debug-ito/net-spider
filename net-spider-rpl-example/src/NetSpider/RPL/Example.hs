{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- |
-- Module: NetSpider.RPL.Example
-- Description: Example executable of NetSpider.RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.RPL.Example
       ( main
       ) where

import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import Control.Applicative (many, (<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, reverse)
import Data.Monoid ((<>), mconcat)
import Data.Text (pack)
import NetSpider.GraphML.Writer (writeGraphML)
import qualified NetSpider.CLI.Snapshot as CLIS
import NetSpider.CLI.Spider (SpiderConfig, parserSpiderConfig)
import NetSpider.Input
  ( defConfig,
    connectWith, close, addFoundNode, clearAll,
    FoundNode(subjectNode, foundAt, neighborLinks, nodeAttributes),
    FoundLink(targetNode, linkAttributes),
    LinkAttributes, NodeAttributes,
    Spider, withSpider
  )
import NetSpider.Output
  ( getSnapshot,
    defQuery, unifyLinkSamples, unifyStd,
    Query(startsFrom, timeInterval),
    SnapshotNode, SnapshotLink, secUpTo,
    SnapshotGraph
  )
import NetSpider.RPL.FindingID
  ( FindingID, idToText,
    IPv6ID, ipv6FromText
  )
import NetSpider.RPL.DIO
  ( FoundNodeDIO, DIONode, MergedDIOLink
  )
import qualified NetSpider.RPL.DIO as DIO
import NetSpider.RPL.DAO (FoundNodeDAO)
import qualified NetSpider.RPL.DAO as DAO
import qualified NetSpider.RPL.Combined as RPL
import NetSpider.RPL.ContikiNG (parseFile, pSyslogHead)
import qualified Options.Applicative as Opt
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

data Cmd = CmdClear
         | CmdInput [FilePath]
         | CmdSnapshot (Query IPv6ID () () ())

optionParser :: Opt.Parser (SpiderConfig n na fla, Cmd)
optionParser = (,) <$> parserSpiderConfig <*> parserCommands
  where
    parserCommands = Opt.hsubparser $ mconcat commands
    commands = [ Opt.command "clear" $
                 Opt.info (pure CmdClear) (Opt.progDesc "Clear the entire database."),
                 Opt.command "input" $
                 Opt.info parserInput (Opt.progDesc "Input local findings into the database."),
                 Opt.command "snapshot" $
                 Opt.info parserSnapshot (Opt.progDesc "Get a snapshot graph from the database.")
               ]
    parserInput = fmap CmdInput $ many $ Opt.strArgument $ mconcat [Opt.metavar "FILE"]
    ipv6Reader = (maybe (fail "Invalid IPv6") return  . ipv6FromText) =<< Opt.auto
    parserSnapshot = fmap CmdSnapshot $ CLIS.parserSnapshotQuery $
                     CLIS.Config { CLIS.nodeIDReader = ipv6Reader,
                                   CLIS.basisSnapshotQuery = defQuery []
                                 }

main :: IO ()
main = do
  (sconf, cmd) <- Opt.execParser $ Opt.info (Opt.helper <*> optionParser) $ mconcat $
                  [ Opt.progDesc "Example net-spider front-end for RPL data model."
                  ]
  case cmd of
    CmdClear -> doClear sconf
    CmdInput fs -> doInput sconf fs
    CmdSnapshot q -> doSnapshot sconf q
  where
    doClear sconf = do
      hPutStrLn stderr "---- Clear graph database"
      withSpider sconf $ clearAll
    doInput sconf filenames = do
      -- filenames is a list of syslog filenames

----------- we need to convert sconf into DIO and DAO...
      
      -- Read DIO and DAO FoundNodes. It might take a long time to insert
      -- a lot of FoundNodes, so this example inserts only the latest
      -- FoundNode per node into the net-spider database.
      (dio_nodes, dao_nodes) <- fmap (concatPairs . map (filterPairs getLatestForEachNode))
                                $ mapM loadFile filenames
      hPutStrLn stderr ("---- Load done")
      forM_ dio_nodes printDIONode
      forM_ dao_nodes printDAONode

---------- TODO
  
  clearDB
  -- Input DIO FoundNodes and get DIO SnapshotGraph.
  dio_graph <- if null dio_nodes
               then return ([], [])
               else let dio_query = (DIO.dioDefQuery [subjectNode (dio_nodes !! 0)])
                    in putNodes dio_query dio_nodes
  -- Input DAO FoundNodes and get DAO SnapshotGraph.
  dao_graph <- if null dao_nodes
               then return ([], [])
               else let dao_input = sortDAONodes dao_nodes
                        dao_query = (DAO.daoDefQuery [subjectNode (dao_input !! 0)])
                          { timeInterval = 0 `secUpTo` foundAt (dao_input !! 0)
                          }
                    in putNodes dao_query dao_input

  -- Merge DIO and DAO SnapshotGraphs into one.
  let com_graph = RPL.combineGraphs dio_graph dao_graph
  -- Write the merged SnapshotGraph in GraphML to stdout.
  TLIO.putStr $ writeGraphML com_graph

----------

-- | Read a Contiki-NG log file, parse it with
-- 'NetSpider.RPL.ContikiNG.parseFile' to get 'FoundNodeDIO' and
-- 'FoundNodeDAO'.
loadFile :: FilePath
         -> IO ([FoundNodeDIO], [FoundNodeDAO])
loadFile file = do
  hPutStrLn stderr ("---- Loading " <> file)
  (dio_nodes, dao_nodes) <- parseFile phead file
  hPutStrLn stderr ((show $ length dio_nodes) <> " DIO Nodes")
  hPutStrLn stderr ((show $ length dao_nodes) <> " DAO Nodes")
  return (dio_nodes, dao_nodes)
  where
    phead = pSyslogHead 2019 Nothing
  
-- | Put (insert) the given 'FoundNode's into the net-spider database
-- and get a 'SnapshotGraph' by the given 'Query'.
putNodes :: (LinkAttributes fla, NodeAttributes na)
         => Query FindingID na fla sla
         -> [FoundNode FindingID na fla]
         -> IO (SnapshotGraph FindingID na sla)
putNodes query input_nodes = do
  bracket (connectWith defConfig) close $ \sp -> do
    hPutStrLn stderr ("---- Add " <> (show $ length $ input_nodes) <> " FoundNodes")
    forM_ (zip input_nodes ([0 ..] :: [Integer])) $ \(input_node, index) -> do
      when ((index `mod` 100) == 0) $ hPutStrLn stderr ("Add node [" <> show index <> "]")
      addFoundNode sp input_node
    hPutStrLn stderr "Add done"
    getSnapshot sp query

---- Print FoundNodes for debug

printDIONode :: FoundNodeDIO -> IO ()
printDIONode fn = do
  TIO.hPutStrLn stderr ("---- DIONode " <> (idToText $ subjectNode fn) <> ", rank " <> rank_text)
  forM_ plinks $ \l -> do
    TIO.hPutStrLn stderr ("  -> " <> (idToText $ targetNode l))
  where
    plinks = filter isPreferredParentLink $ neighborLinks fn
    rank_text = pack $ show $ DIO.rank $ nodeAttributes fn
    isPreferredParentLink l =
      (DIO.neighborType $ linkAttributes l) == DIO.PreferredParent

printDAONode :: FoundNodeDAO -> IO ()
printDAONode fn = do
  TIO.hPutStrLn stderr ("---- DAONode " <> (idToText $ subjectNode fn) <> route_num_text)
  forM_ (neighborLinks fn) $ \l -> do
    TIO.hPutStrLn stderr ("  -> " <> (idToText $ targetNode l) <> ", lifetime " <> lt_text l)
  where
    lt_text l = pack $ show $ DAO.pathLifetimeSec $ linkAttributes l
    route_num_text =
      case DAO.daoRouteNum $ nodeAttributes fn of
        Nothing -> ""
        Just n -> ", route_num " <> (pack $ show n)

---- General utility functions.

filterPairs :: (forall na la . [FoundNode FindingID na la] -> [FoundNode FindingID na la])
            -> ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
            -> ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
filterPairs f (ns1, ns2) = (f ns1, f ns2)

concatPairs :: [([a], [b])] -> ([a], [b])
concatPairs [] = ([],[])
concatPairs ((as, bs) : rest) = (as ++ rest_as, bs ++ rest_bs)
  where
    (rest_as, rest_bs) = concatPairs rest


---- Filter for FoundNode.

type NodeMap n = HashMap FindingID [n]

collectNodes :: [FoundNode FindingID na la] -> NodeMap (FoundNode FindingID na la)
collectNodes = foldr addNode HM.empty
  where
    addNode n acc = HM.insertWith f (subjectNode n) [n] acc
      where
        f new old = new ++ old

getLatestNodes :: NodeMap (FoundNode n na la) -> [FoundNode n na la]
getLatestNodes nm = concat $ HM.elems $ fmap filterLatest nm
  where
    filterLatest fns = getHead $ reverse $ sortOn foundAt fns
    getHead [] = []
    getHead (a : _) = [a]

getLatestForEachNode :: [FoundNode FindingID na la] -> [FoundNode FindingID na la]
getLatestForEachNode = getLatestNodes . collectNodes


---- FoundNode utility

sortDAONodes :: [FoundNodeDAO] -> [FoundNodeDAO]
sortDAONodes = reverse . sortOn (DAO.daoRouteNum . nodeAttributes)
