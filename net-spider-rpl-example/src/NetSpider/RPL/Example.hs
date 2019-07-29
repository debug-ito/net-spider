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
import Control.Monad (forM_, when, void)
import Data.Greskell (Key(..))
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
    addFoundNode, clearAll,
    FoundNode(subjectNode, foundAt, neighborLinks, nodeAttributes),
    FoundLink(targetNode, linkAttributes),
    LinkAttributes, NodeAttributes,
    Spider, withSpider, nodeIdKey
  )
import NetSpider.Output
  ( getSnapshot,
    defQuery, unifyLinkSamples, unifyStd,
    Query(startsFrom, timeInterval),
    SnapshotNode, SnapshotLink, secUpTo,
    SnapshotGraph
  )
import NetSpider.RPL.FindingID
  ( FindingID(..), idToText, FindingType(..),
    IPv6ID(..), ipv6FromText, ipv6Only
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
         | CmdCIS [FilePath] (Query IPv6ID () () ()) -- ^ Clear + Input + Snapshot

optionParser :: Opt.Parser (SpiderConfig n na fla, Cmd)
optionParser = (,) <$> parserSpiderConfig <*> parserCommands
  where
    parserCommands = Opt.hsubparser $ mconcat commands
    commands = [ Opt.command "clear" $
                 Opt.info (pure CmdClear) (Opt.progDesc "Clear the entire database."),
                 Opt.command "input" $
                 Opt.info parserInput (Opt.progDesc "Input local findings into the database."),
                 Opt.command "snapshot" $
                 Opt.info parserSnapshot (Opt.progDesc "Get a snapshot graph from the database."),
                 Opt.command "cis" $
                 Opt.info (CmdCIS <$> parserInputFiles <*> parserSnapshotQuery)
                 (Opt.progDesc "Clear + Input + Snapshot at once.")
               ]
    parserInput = fmap CmdInput $ parserInputFiles
    parserInputFiles = many $ Opt.strArgument $ mconcat
                       [ Opt.metavar "FILE",
                         Opt.help "Input file. You can specify multiple times."
                       ]
    ipv6Reader = (maybe (fail "Invalid IPv6") return  . ipv6FromText) =<< Opt.auto
    parserSnapshot = fmap CmdSnapshot $ parserSnapshotQuery
    parserSnapshotQuery = CLIS.parserSnapshotQuery $
                          CLIS.Config { CLIS.nodeIDReader = ipv6Reader,
                                        CLIS.basisSnapshotQuery = defQuery [],
                                        CLIS.startsFromAsArguments = True
                                      }

main :: IO ()
main = do
  (sconf, cmd) <- Opt.execParser $ Opt.info (Opt.helper <*> optionParser) $ mconcat $
                  [ Opt.progDesc "Example net-spider front-end for RPL data model."
                  ]
  case cmd of
    CmdClear -> doClear sconf
    CmdInput fs -> void $ doInput sconf fs
    CmdSnapshot q -> doSnapshot sconf q
    CmdCIS fs q -> doCIS sconf fs q
  where
    doClear sconf = do
      hPutStrLn stderr "---- Clear graph database"
      withSpider sconf $ clearAll

    doInput sconf filenames = do
      -- filenames is a list of syslog filenames
      
      -- Read DIO and DAO FoundNodes. It might take a long time to insert
      -- a lot of FoundNodes, so this example inserts only the latest
      -- FoundNode per node into the net-spider database.
      (dio_nodes, dao_nodes) <- fmap (concatPairs . map (filterPairs getLatestForEachNode))
                                $ mapM loadFile filenames
      hPutStrLn stderr ("---- Load done")
      
      -- Input DIO and DAO FoundNodes. Note that we have to cast
      -- SpiderConfig's type to match DIO and DAO FoundNode.
      forM_ dio_nodes printDIONode
      putNodes (castSpiderConfig sconf) dio_nodes
      forM_ dao_nodes printDAONode
      putNodes (castSpiderConfig sconf) dao_nodes
      return (dio_nodes, dao_nodes)

    doSnapshot sconf query = do
      hPutStrLn stderr ("---- Query starts from " ++ (show $ length $ startsFrom query) ++ " nodes")
      forM_ (startsFrom query) $ \nid -> do
        hPutStrLn stderr (show nid)
      -- Get DIO and DAO snapshot graphs with the Query.
      hPutStrLn stderr ("---- Get DIO SnapshotGraph")
      dio_graph <- withSpider (castSpiderConfig sconf) $ \sp -> do
        getSnapshot sp $ rebaseQuery query FindingDIO (DIO.dioDefQuery [])
      hPutStrLn stderr ("---- Get DAO SnapshotGraph")
      dao_graph <- withSpider (castSpiderConfig sconf) $ \sp -> do
        getSnapshot sp $ rebaseQuery query FindingDAO (DAO.daoDefQuery [])
      
      -- Merge DIO and DAO SnapshotGraphs into one.
      let com_graph = RPL.combineGraphs dio_graph dao_graph
      -- Write the merged SnapshotGraph in GraphML to stdout.
      hPutStrLn stderr ("---- Format DIO+DAO SnapshotGraph into GraphML")
      TLIO.putStr $ writeGraphML com_graph

    doCIS sconf filenames query_base = do
      doClear sconf
      (dio_nodes, dao_nodes) <- doInput sconf filenames
      let starts = (map (ipv6Only . subjectNode) $ sortDAONodes dao_nodes)
                   ++
                   (map (ipv6Only . subjectNode) dio_nodes)
          q = query_base { startsFrom = starts }
      doSnapshot sconf q
      
----------

castSpiderConfig :: SpiderConfig n1 na1 fla1 -> SpiderConfig n2 na2 fla2
castSpiderConfig sc = sc { nodeIdKey = Key $ unKey $ nodeIdKey sc }

-- | Convert the base of the original query.
rebaseQuery :: Query IPv6ID na1 fla1 sla1 -- ^ original query
            -> FindingType -- ^ new finding type
            -> Query FindingID na2 fla2 sla2 -- ^ new query base
            -> Query FindingID na2 fla2 sla2
rebaseQuery orig ftype base = base { startsFrom = map liftToFindingID $ startsFrom orig,
                                     timeInterval = timeInterval orig
                                   }
  where
    liftToFindingID (IPv6ID ip) = FindingID ftype ip

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

-- | Put (insert) the given 'FoundNode's into the net-spider
-- database
putNodes :: (LinkAttributes fla, NodeAttributes na)
         => SpiderConfig FindingID na fla
         -> [FoundNode FindingID na fla]
         -> IO ()
putNodes sconf input_nodes = do
  withSpider sconf $ \sp -> do
    hPutStrLn stderr ("---- Add " <> (show $ length $ input_nodes) <> " FoundNodes")
    forM_ (zip input_nodes ([0 ..] :: [Integer])) $ \(input_node, index) -> do
      when ((index `mod` 100) == 0) $ hPutStrLn stderr ("Add node [" <> show index <> "]")
      addFoundNode sp input_node
    hPutStrLn stderr "Add done"

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


-- ---- FoundNode utility

sortDAONodes :: [FoundNodeDAO] -> [FoundNodeDAO]
sortDAONodes = reverse . sortOn (DAO.daoRouteNum . nodeAttributes)
