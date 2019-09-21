{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- |
-- Module: NetSpider.RPL.CLI
-- Description: CLI executable of NetSpider.RPL
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module NetSpider.RPL.CLI
       ( main,
         -- * Symbols only for testing
         optionParser,
         Cmd(..)
       ) where

import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import Control.Applicative (many, (<$>), (<*>), optional)
import Control.Exception (bracket)
import Control.Monad (forM_, when, void)
import Control.Monad.Logger (LogLevel(LevelInfo))
import Data.Greskell (Key(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, reverse, intercalate)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text, pack, unpack)
import Data.Time (getZonedTime, ZonedTime(zonedTimeToLocalTime), LocalTime(localDay), toGregorian)
import NetSpider.GraphML.Writer (writeGraphML)
import qualified NetSpider.CLI.Snapshot as CLIS
import NetSpider.CLI.Spider (SpiderConfig, parserSpiderConfig)
import NetSpider.Input
  ( defConfig, logThreshold,
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
import NetSpider.RPL.ContikiNG (parseFile, parseFileHandle, pSyslogHead)
import qualified Options.Applicative as Opt
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdin)

main :: IO ()
main = do
  cli_conf <- Opt.execParser $ Opt.info (Opt.helper <*> optionParser) $ mconcat $
              [ Opt.progDesc "net-spider front-end for RPL data model."
              ]
  let sconf = cliSpiderConfig cli_conf
      cmd = cliCmd cli_conf
  case cmd of
    CmdClear -> doClear sconf
    CmdInput inp -> void $ doInput sconf inp
    CmdSnapshot q -> doSnapshot sconf q
    CmdCIS inp q -> doCIS sconf inp q
  where
    doClear sconf = do
      hPutStrLn stderr "---- Clear graph database"
      withSpider sconf $ clearAll

    doInput sconf (InputParams filenames fnfilter myear) = do
      -- filenames is a list of syslog filenames

      year <- getYear myear
      
      -- Read DIO and DAO FoundNodes. It might take a long time to
      -- insert a lot of FoundNodes, so this executable inserts only
      -- the latest FoundNode per node into the net-spider database.
      (dio_nodes, dao_nodes) <- applyFoundNodeFilter fnfilter
                                =<< (fmap concatPairs $ mapM (loadFile year) filenames)
      hPutStrLn stderr ("---- Load done")
      
      -- Input DIO and DAO FoundNodes. Note that we have to cast
      -- SpiderConfig's type to match DIO and DAO FoundNode.
      hPutStrLn stderr ("---- Put " <> (show $ length dio_nodes) <> " local findings about DIO")
      when (isVerbose sconf) $ forM_ dio_nodes printDIONode
      putNodes (castSpiderConfig sconf) dio_nodes
      hPutStrLn stderr ("---- Put " <> (show $ length dao_nodes) <> " local findings about DAO")
      when (isVerbose sconf) $ forM_ dao_nodes printDAONode
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

    doCIS sconf input_params query_base = do
      doClear sconf
      (dio_nodes, dao_nodes) <- doInput sconf input_params
      -- Make a query from the FoundNodes just loaded.
      let starts = (map (ipv6Only . subjectNode) $ sortDAONodes dao_nodes)
                   ++
                   (map (ipv6Only . subjectNode) dio_nodes)
          q = query_base { startsFrom = starts }
      doSnapshot sconf q
    isVerbose sconf = logThreshold sconf <= LevelInfo


---- CLI parsers.

-- | Filter function of 'FoundNode', agnostic of node and link
-- attributes.
data FoundNodeFilter =
  FoundNodeFilter
  { fnfRun :: forall na la . [FoundNode FindingID na la] -> [FoundNode FindingID na la],
    fnfSymbol :: String,
    fnfDesc :: Text
  }

applyFoundNodeFilter :: FoundNodeFilter
                     -> ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
                     -> IO ([FoundNode FindingID na1 la1], [FoundNode FindingID na2 la2])
applyFoundNodeFilter fnf input = do
  hPutStrLn stderr ("---- Apply filter '" ++ fnfSymbol fnf ++ "' to local findings.")
  return $ filterPairs (fnfRun fnf) input

data CLIConfig n na fla =
  CLIConfig
  { cliSpiderConfig :: SpiderConfig n na fla,
    cliCmd :: Cmd
  }

-- | Parameters for input command. Filenames to input, filter for
-- FoundNodes, and the year that the parser use to parse the input
-- files.
data InputParams = InputParams [FilePath] FoundNodeFilter (Maybe Year)

-- | CLI subcommands and their arguments.
data Cmd = CmdClear -- ^ Clear the entire database.
         | CmdInput InputParams -- ^ Input FoundNodes to the database
         | CmdSnapshot (Query IPv6ID () () ()) -- ^ Get a snapshot graph.
         | CmdCIS InputParams (Query IPv6ID () () ()) -- ^ Clear + Input + Snapshot

optionParser :: Opt.Parser (CLIConfig n na fla)
optionParser = CLIConfig <$> parserSpiderConfig <*> parserCommands
  where
    parserCommands = Opt.hsubparser $ mconcat commands
    commands = [ Opt.command "clear" $
                 Opt.info (pure CmdClear) (Opt.progDesc "Clear the entire database."),
                 Opt.command "input" $
                 Opt.info (CmdInput <$> parserInputParams)
                 (Opt.progDesc "Input local findings into the database."),
                 Opt.command "snapshot" $
                 Opt.info (parserSnapshot True) (Opt.progDesc "Get a snapshot graph from the database."),
                 Opt.command "cis" $
                 Opt.info (CmdCIS <$> parserInputParams <*> parserSnapshotQuery False)
                 (Opt.progDesc "Clear + Input + Snapshot at once. `startsFrom` of the query is set by local findings loaded from the files.")
               ]
    parserInputParams = InputParams <$> parserInputFiles <*> parserFilter <*> parserYear
    parserInputFiles = many $ Opt.strArgument $ mconcat
                       [ Opt.metavar "FILE",
                         Opt.help "Input file. You can specify multiple times. If '-' is specified, it reads STDIN."
                       ]
    ipv6Reader = (maybe (fail "Invalid IPv6") return  . ipv6FromText . pack) =<< Opt.str
    parserSnapshot parse_arg = fmap CmdSnapshot $ parserSnapshotQuery parse_arg
    parserSnapshotQuery parse_arg =
      CLIS.parserSnapshotQuery $
      CLIS.SnapshotConfig
      { CLIS.nodeIDReader = ipv6Reader,
        CLIS.basisSnapshotQuery = defQuery [],
        CLIS.startsFromAsArguments = parse_arg
      }
    parserFilter = Opt.option readerFilter $ mconcat
                   [ Opt.metavar "FILTER",
                     Opt.help ( "Filter for local findings. Out of the local findings loaded from the input files, "
                                <> "only those that pass the filter are input to the database. "
                                <> "Possible values are: " <> filterDescs
                              ),
                     Opt.short 'F',
                     Opt.long "filter",
                     Opt.value (allFilters !! 0)
                   ]
    filterDescs = intercalate ", " $ map descFor allFilters
    descFor fnf = "'" <> fnfSymbol fnf <> "': " <> (unpack $ fnfDesc fnf)
    readerFilter = selectFoundNodeFilter =<< Opt.str
    selectFoundNodeFilter symbol =
      case filter (\fnf -> fnfSymbol fnf == symbol) allFilters of
        [] -> fail ("Unknown filter: " <> symbol)
        (x : _) -> return x
    allFilters =
      [ FoundNodeFilter
        { fnfRun = id,
          fnfSymbol = "none",
          fnfDesc = "Not filter anything. This is the default."
        },
        FoundNodeFilter
        { fnfRun = getLatestForEachNode,
          fnfSymbol = "latest",
          fnfDesc = "Input only the latest local finding for each node."
        }
      ]
    parserYear = optional $ Opt.option Opt.auto $ mconcat
                 [ Opt.long "year",
                   Opt.metavar "YEAR",
                   Opt.help ( "If specified, the year of timestamps in local findings is set to YEAR. "
                              <> "If not specified, the year of the local system time is used. "
                              <> "This is because the input file format does not contain the year in timestamp."
                            )
                 ]

---- Type adaptation of Config and Query

-- | Cast type variables of 'SpiderConfig'. The type variables are
-- basically phantom types for now.
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

---- I/O of FoundNodes

type Year = Integer

-- | If input is 'Just', it returns that year. If 'Nothing', it gets
-- the local year from the system and returns it.
getYear :: Maybe Year -> IO Year
getYear (Just y) = return y
getYear Nothing = do
  zt <- getZonedTime
  let (y, _, _) = toGregorian $ localDay $ zonedTimeToLocalTime zt
  return y

-- | Read a Contiki-NG log file, parse it with
-- 'NetSpider.RPL.ContikiNG.parseFile' to get 'FoundNodeDIO' and
-- 'FoundNodeDAO'.
loadFile :: Year
         -> FilePath
         -> IO ([FoundNodeDIO], [FoundNodeDAO])
loadFile year file = do
  (dio_nodes, dao_nodes) <- loadNodes
  hPutStrLn stderr ((show $ length dio_nodes) <> " DIO local findings loaded")
  hPutStrLn stderr ((show $ length dao_nodes) <> " DAO local findings loaded")
  return (dio_nodes, dao_nodes)
  where
    phead = pSyslogHead year Nothing
    loadNodes = do
      if file == "-"
        then do
        hPutStrLn stderr ("---- Loading from stdin")
        parseFileHandle phead stdin
        else do
        hPutStrLn stderr ("---- Loading " <> file)
        parseFile phead file


-- | Put (insert) the given 'FoundNode's into the net-spider
-- database
putNodes :: (LinkAttributes fla, NodeAttributes na)
         => SpiderConfig FindingID na fla
         -> [FoundNode FindingID na fla]
         -> IO ()
putNodes sconf input_nodes = do
  withSpider sconf $ \sp -> do
    hPutStrLn stderr ("---- Add " <> (show $ length $ input_nodes) <> " local findings")
    forM_ (zip input_nodes ([0 ..] :: [Integer])) $ \(input_node, index) -> do
      when ((index `mod` 100) == 0) $ hPutStrLn stderr ("Add local finding [" <> show index <> "]")
      addFoundNode sp input_node
    hPutStrLn stderr "Add done"

---- Print FoundNodes for debug

printDIONode :: FoundNodeDIO -> IO ()
printDIONode fn = do
  TIO.hPutStrLn stderr ("---- DIO finding: " <> (idToText $ subjectNode fn) <> ", rank " <> rank_text)
  forM_ plinks $ \l -> do
    TIO.hPutStrLn stderr ("  -> " <> (idToText $ targetNode l))
  where
    plinks = filter isPreferredParentLink $ neighborLinks fn
    rank_text = pack $ show $ DIO.rank $ nodeAttributes fn
    isPreferredParentLink l =
      (DIO.neighborType $ linkAttributes l) == DIO.PreferredParent

printDAONode :: FoundNodeDAO -> IO ()
printDAONode fn = do
  TIO.hPutStrLn stderr ("---- DAO finding: " <> (idToText $ subjectNode fn) <> route_num_text)
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
